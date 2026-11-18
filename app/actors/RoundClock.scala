package actors

import akka.actor.{Actor, ActorRef, Props, Timers}
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest
import play.api.Logging

import scala.concurrent.duration._

/**
  * When rounds start and end, without polling: the next deadline across all rounds, for one exact one-shot timer.
  *
  * Every round has two edges, `Started` at its start and `Ended` at its end, in that order; a round that is finished
  * early (`is_finished`) has its `Ended` due at once. Each edge fires exactly once per schedule: the `fired` set records
  * it, and an unchanged row never re-arms it.
  *
  * Start-up rule: the clock starts from the current state and announces nothing retroactively. Edges whose moment has
  * already passed when a round is first seen (at load, or when a ContestChanged brings a round the clock did not know)
  * count as fired: a running round gets no `Started`, an over round no `Ended`. A restart therefore never re-announces.
  * Edges that come due while the clock runs fire even when the timer is late.
  *
  * Reschedule rule: when a known round's row changes, its unfired edges simply follow the new times (an end moved
  * into the past is due at once); a fired edge stays fired unless the new times put it back in the future (a round
  * extended after its end will end again at the new end). The other rounds are untouched.
  *
  * RoundEdge is sealed so that countdown edges can be added (Task 18).
  */
object RoundClock {
  sealed trait RoundEdge { def roundId: Int }
  final case class Started(roundId: Int) extends RoundEdge
  final case class Ended(roundId: Int) extends RoundEdge

  private def rank(e: RoundEdge): Int = e match {
    case _: Started => 0
    case _: Ended => 1
  }

  /** The next edge of one round, None when it has none left. */
  private def nextOf(c: Contest, finished: Boolean, now: DateTime, fired: Set[RoundEdge]): Option[(DateTime, RoundEdge)] =
    if (fired(Ended(c.id))) None
    else if (finished) Some(now -> Ended(c.id))
    else if (!fired(Started(c.id))) Some(c.startTime -> Started(c.id))
    else Some(c.endTime -> Ended(c.id))

  /**
    * The next not-yet-fired edge across the rounds: the earliest, ties broken by round id and then Started before
    * Ended. A deadline may be in the past (due now).
    */
  def nextDeadline(rounds: Seq[Contest], isFinished: Int => Boolean, now: DateTime,
                   fired: Set[RoundEdge]): Option[(DateTime, RoundEdge)] = {
    val candidates = rounds.flatMap(c => nextOf(c, isFinished(c.id), now, fired))
    if (candidates.isEmpty) None
    else Some(candidates.minBy { case (t, e) => (t.getMillis, e.roundId, rank(e)) })
  }

  /** Whether the edge's moment, by the round's row, is not in the future. */
  private def passed(e: RoundEdge, c: Contest, now: DateTime): Boolean = e match {
    case _: Started => c.isFinished || !now.isBefore(c.startTime)
    case _: Ended => c.isFinished || !now.isBefore(c.endTime)
  }

  private def edgesOf(id: Int): Set[RoundEdge] = Set(Started(id), Ended(id))

  /** The edges of a round first seen at `now` that count as fired (the start-up rule). */
  def firedAt(c: Contest, now: DateTime): Set[RoundEdge] = {
    val ended = passed(Ended(c.id), c, now)
    edgesOf(c.id).filter {
      case e: Started => ended || passed(e, c, now)
      case _: Ended => ended
    }
  }

  final case class State(rounds: Map[Int, Contest], fired: Set[RoundEdge]) {
    def next(now: DateTime): Option[(DateTime, RoundEdge)] =
      nextDeadline(rounds.values.toSeq, id => rounds.get(id).exists(_.isFinished), now, fired)

    def fire(e: RoundEdge): State = copy(fired = fired + e)

    /** The edges due at `now`, in order, and the state with them fired. */
    def due(now: DateTime): (Seq[RoundEdge], State) = {
      @annotation.tailrec
      def loop(s: State, acc: Vector[RoundEdge]): (Seq[RoundEdge], State) = s.next(now) match {
        case Some((t, e)) if !t.isAfter(now) => loop(s.fire(e), acc :+ e)
        case _ => (acc, s)
      }
      loop(this, Vector.empty)
    }

    /** A round's row, new or changed (see the reschedule rule). */
    def changed(c: Contest, now: DateTime): State = rounds.get(c.id) match {
      case None => State(rounds.updated(c.id, c), fired -- edgesOf(c.id) ++ firedAt(c, now))
      case Some(_) => State(rounds.updated(c.id, c), fired.filter(e => e.roundId != c.id || passed(e, c, now)))
    }

    def removed(id: Int): State = State(rounds - id, fired -- edgesOf(id))
  }

  object State {
    def load(rounds: Seq[Contest], now: DateTime): State =
      State(rounds.map(c => c.id -> c).toMap, rounds.flatMap(c => firedAt(c, now)).toSet)
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

  def props(initial: Seq[Contest], subscribers: Seq[ActorRef], now: () => DateTime = () => DateTime.now): Props =
    Props(new RoundClockActor(initial, subscribers, now))

  private case object Wake
}

class RoundClockActor(initial: Seq[Contest], subscribers: Seq[ActorRef], now: () => DateTime) extends Actor with Timers with Logging {
  import RoundClockActor._

  private var state = RoundClock.State.load(initial, now())

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
