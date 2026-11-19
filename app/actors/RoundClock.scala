package actors

import akka.actor.{Actor, ActorRef, Props, Timers}
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest
import palevo.CountdownConfig
import play.api.Logging

import scala.concurrent.duration._

/**
  * When rounds start and end, and when their countdowns are due, without polling: the next deadline across all
  * rounds, for one exact one-shot timer.
  *
  * Every round has, in this order: a `CountdownToStart(m)` edge `m` minutes before its start for each start threshold,
  * `Started` at its start, a `CountdownToEnd(m)` edge `m` minutes before its end for each end threshold that lies
  * after the start, and `Ended` at its end. The thresholds come from `countdown(round)` (palevo's CountdownConfig;
  * empty by default). A round that is finished early (`is_finished`) has its `Ended` due at once and no countdown.
  * Each edge fires exactly once per schedule: the `fired` set records it, and an unchanged row never re-arms it.
  *
  * Start-up rule: the clock starts from the current state and announces nothing retroactively. Edges whose moment has
  * already passed when a round is first seen (at load, or when a ContestChanged brings a round the clock did not know)
  * count as fired: a running round gets no `Started`, an over round no `Ended`, a passed threshold no countdown.
  * A restart therefore never re-announces. Edges that come due while the clock runs fire even when the timer is late,
  * except that a late wake which crossed several thresholds of the same countdown announces only the latest of them,
  * and none at all once their target (the start or the end) has come: the others are marked fired silently.
  *
  * Reschedule rule: when a known round's row changes, its unfired edges simply follow the new times (an end moved
  * into the past is due at once); a fired `Started`/`Ended` stays fired unless the new times put it back in the
  * future (a round extended after its end will end again at the new end). When the start (end) moved, the start (end)
  * thresholds are re-derived from the new time: those already passed are marked fired silently, the others are due
  * again against the new time. The other rounds are untouched.
  */
object RoundClock {
  sealed trait RoundEdge { def roundId: Int }
  final case class Started(roundId: Int) extends RoundEdge
  final case class Ended(roundId: Int) extends RoundEdge
  /** `minutes` before the round's start. For palevo only: never pushed to browsers. */
  final case class CountdownToStart(roundId: Int, minutes: Int) extends RoundEdge
  /** `minutes` before the round's end. For palevo only: never pushed to browsers. */
  final case class CountdownToEnd(roundId: Int, minutes: Int) extends RoundEdge

  /** The countdown thresholds of a round. */
  type Countdown = Contest => CountdownConfig

  val NoCountdown: Countdown = _ => CountdownConfig(Nil, Nil)

  /** Whether the edge is a countdown (palevo-only) edge. */
  def isCountdown(e: RoundEdge): Boolean = e match {
    case _: CountdownToStart | _: CountdownToEnd => true
    case _: Started | _: Ended => false
  }

  /** Order within one instant: countdowns to the start, the start, countdowns to the end, the end. */
  private def rank(e: RoundEdge): (Int, Int) = e match {
    case CountdownToStart(_, m) => (0, -m)
    case _: Started => (1, 0)
    case CountdownToEnd(_, m) => (2, -m)
    case _: Ended => (3, 0)
  }

  /** The moment of an edge by the round's row. */
  def timeOf(e: RoundEdge, c: Contest): DateTime = e match {
    case CountdownToStart(_, m) => c.startTime.minusMinutes(m)
    case _: Started => c.startTime
    case CountdownToEnd(_, m) => c.endTime.minusMinutes(m)
    case _: Ended => c.endTime
  }

  private def startCountdown(c: Contest, cd: Countdown): Seq[RoundEdge] =
    cd(c).start.distinct.map(CountdownToStart(c.id, _))

  /** End thresholds after the start only: one at or before the start would be crossed by the start itself. */
  private def endCountdown(c: Contest, cd: Countdown): Seq[RoundEdge] =
    cd(c).end.distinct.filter(m => c.endTime.minusMinutes(m).isAfter(c.startTime)).map(CountdownToEnd(c.id, _))

  /** Every edge of a round, in no particular order. */
  def edgesOf(c: Contest, cd: Countdown): Seq[RoundEdge] =
    (startCountdown(c, cd) :+ Started(c.id)) ++ endCountdown(c, cd) :+ Ended(c.id)

  /** The edges of one round that may fire next: the not-yet-fired ones whose turn has come. */
  private def candidates(c: Contest, finished: Boolean, now: DateTime, fired: Set[RoundEdge],
                         cd: Countdown): Seq[(DateTime, RoundEdge)] =
    if (fired(Ended(c.id))) Nil
    else if (finished) Seq(now -> Ended(c.id))
    else {
      val edges =
        if (!fired(Started(c.id))) startCountdown(c, cd) :+ Started(c.id)
        else endCountdown(c, cd) :+ Ended(c.id)
      edges.filterNot(fired).map(e => timeOf(e, c) -> e)
    }

  private def order(te: (DateTime, RoundEdge)): (Long, Int, (Int, Int)) = (te._1.getMillis, te._2.roundId, rank(te._2))

  /**
    * The next not-yet-fired edge across the rounds: the earliest, ties broken by round id and then the order of a
    * round's edges. A deadline may be in the past (due now).
    */
  def nextDeadline(rounds: Seq[Contest], isFinished: Int => Boolean, now: DateTime,
                   fired: Set[RoundEdge], countdown: Countdown = NoCountdown): Option[(DateTime, RoundEdge)] = {
    val all = rounds.flatMap(c => candidates(c, isFinished(c.id), now, fired, countdown))
    if (all.isEmpty) None else Some(all.minBy(order))
  }

  /** Whether the edge's moment, by the round's row, is not in the future. */
  private def passed(e: RoundEdge, c: Contest, now: DateTime): Boolean =
    c.isFinished || !now.isBefore(timeOf(e, c))

  /** The edges of a round first seen at `now` that count as fired (the start-up rule). */
  def firedAt(c: Contest, now: DateTime, cd: Countdown = NoCountdown): Set[RoundEdge] = {
    val all = edgesOf(c, cd)
    if (passed(Ended(c.id), c, now)) all.toSet else all.filter(passed(_, c, now)).toSet
  }

  /**
    * A countdown edge that is due at `now` but stale: a later threshold of the same countdown, or its target itself,
    * is due as well. It is marked fired without being announced.
    */
  private def superseded(e: RoundEdge, c: Contest, now: DateTime, cd: Countdown): Boolean = e match {
    case CountdownToStart(_, m) =>
      passed(Started(c.id), c, now) || startCountdown(c, cd).exists {
        case later @ CountdownToStart(_, m2) => m2 < m && passed(later, c, now)
        case _ => false
      }
    case CountdownToEnd(_, m) =>
      passed(Ended(c.id), c, now) || endCountdown(c, cd).exists {
        case later @ CountdownToEnd(_, m2) => m2 < m && passed(later, c, now)
        case _ => false
      }
    case _ => false
  }

  final case class State(rounds: Map[Int, Contest], fired: Set[RoundEdge], countdown: Countdown = NoCountdown) {
    def next(now: DateTime): Option[(DateTime, RoundEdge)] =
      nextDeadline(rounds.values.toSeq, id => rounds.get(id).exists(_.isFinished), now, fired, countdown)

    def fire(e: RoundEdge): State = copy(fired = fired + e)

    /** The edges due at `now` to announce, in order, and the state with them (and any superseded ones) fired. */
    def due(now: DateTime): (Seq[RoundEdge], State) = {
      @annotation.tailrec
      def loop(s: State, acc: Vector[RoundEdge]): (Seq[RoundEdge], State) = s.next(now) match {
        case Some((t, e)) if !t.isAfter(now) =>
          if (superseded(e, s.rounds(e.roundId), now, countdown)) loop(s.fire(e), acc) else loop(s.fire(e), acc :+ e)
        case _ => (acc, s)
      }
      loop(this, Vector.empty)
    }

    /** A round's row, new or changed (see the reschedule rule). */
    def changed(c: Contest, now: DateTime): State = rounds.get(c.id) match {
      case None => copy(rounds = rounds.updated(c.id, c), fired = fired.filter(_.roundId != c.id) ++ firedAt(c, now, countdown))
      case Some(old) =>
        val startMoved = old.startTime.getMillis != c.startTime.getMillis
        val endMoved = old.endTime.getMillis != c.endTime.getMillis
        val kept = fired.filter(e => e.roundId != c.id || (e match {
          case _: CountdownToStart if startMoved => false
          case _: CountdownToEnd if endMoved => false
          case _ => passed(e, c, now)
        }))
        val rederived = edgesOf(c, countdown).filter {
          case _: CountdownToStart => startMoved
          case _: CountdownToEnd => endMoved
          case _ => false
        }.filter(passed(_, c, now))
        copy(rounds = rounds.updated(c.id, c), fired = kept ++ rederived)
    }

    def removed(id: Int): State = copy(rounds = rounds - id, fired = fired.filter(_.roundId != id))
  }

  object State {
    def load(rounds: Seq[Contest], now: DateTime, countdown: Countdown = NoCountdown): State =
      State(rounds.map(c => c.id -> c).toMap, rounds.flatMap(c => firedAt(c, now, countdown)).toSet, countdown)
  }
}

/**
  * Holds the current rounds (the initial load, then StatusActor.ContestChanged / ContestRemoved) and arms ONE
  * single-shot timer for the next deadline. Every change and every firing fires what is due and re-arms. Each edge is
  * told to `subscribers` and published on the event stream as `Fired`.
  */
object RoundClockActor {
  final case class Fired(edge: RoundClock.RoundEdge, round: Contest)

  /** The Akka scheduler refuses delays of about 248 days; a farther deadline is re-armed after this. */
  val MaxDelay: FiniteDuration = 7.days

  def delay(deadline: DateTime, now: DateTime): FiniteDuration =
    (deadline.getMillis - now.getMillis).max(0L).millis.min(MaxDelay)

  def props(initial: Seq[Contest], subscribers: Seq[ActorRef], now: () => DateTime = () => DateTime.now,
            countdown: RoundClock.Countdown = RoundClock.NoCountdown): Props =
    Props(new RoundClockActor(initial, subscribers, now, countdown))

  /** The deadline timer's message; public so tests with an injected clock can deliver it by hand. */
  case object Wake
}

class RoundClockActor(initial: Seq[Contest], subscribers: Seq[ActorRef], now: () => DateTime,
                      countdown: RoundClock.Countdown) extends Actor with Timers with Logging {
  import RoundClockActor._

  private var state = RoundClock.State.load(initial, now(), countdown)

  override def preStart(): Unit = {
    super.preStart()
    rearm()
  }

  private def rearm(): Unit = {
    val t = now()
    val (edges, after) = state.due(t)
    state = after
    edges.foreach { e =>
      val msg = Fired(e, state.rounds(e.roundId))
      logger.info(s"round ${e.roundId}: $e")
      subscribers.foreach(_ ! msg)
      context.system.eventStream.publish(msg)
    }
    state.next(t) match {
      case Some((deadline, _)) => timers.startSingleTimer(Wake, Wake, delay(deadline, t))
      case None => timers.cancel(Wake)
    }
  }

  override def receive: Receive = {
    case StatusActor.ContestChanged(c) =>
      state = state.changed(c, now())
      rearm()
    case StatusActor.ContestRemoved(id) =>
      state = state.removed(id)
      rearm()
    case Wake => rearm()
  }
}
