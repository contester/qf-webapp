package palevo

import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest

import scala.collection.mutable

object ContestTimeTracker {
  /** Countdowns and reschedules are announced only for contests up to this id; the ones above are test copies. */
  val MaxAnnouncedContest = 10
}

/**
  * Turns the passage of time into contest_starting / contest_freezing / contest_ending events ("now", and "in N
  * minutes" before the start and before the end). The first observation of a contest only records its phase and the
  * countdown thresholds already reached, so restarting the application mid-contest does not re-announce anything.
  * Not thread-safe: call it from one actor.
  */
final class ContestTimeTracker(countdown: CountdownConfig = CountdownConfig.Default) {
  private val Started = 1
  private val Frozen = 2
  private val Finished = 3

  private final class State(var phase: Int, val startFired: mutable.Set[Int], val endFired: mutable.Set[Int])

  private val seen = mutable.Map[Int, State]()

  private def freezeEnabled(c: Contest) = c.freezeTime.isBefore(c.endTime)

  private def phaseAt(c: Contest, now: DateTime): Int =
    if (!now.isBefore(c.endTime)) Finished
    else if (freezeEnabled(c) && !now.isBefore(c.freezeTime)) Frozen
    else if (!now.isBefore(c.startTime)) Started
    else 0

  private def remainingMillis(target: DateTime, now: DateTime): Long = target.getMillis - now.getMillis

  /** The thresholds whose time has come: at most `T` minutes remain. */
  private def reached(thresholds: Seq[Int], remaining: Long): Seq[Int] = thresholds.filter(_ * 60000L >= remaining)

  /**
    * Marks every reached threshold as fired and returns the smallest one that was not yet fired, if the moment is
    * still ahead. A late tick that crossed several thresholds therefore announces only the latest.
    */
  private def due(fired: mutable.Set[Int], thresholds: Seq[Int], remaining: Long): Option[Int] = {
    val fresh = reached(thresholds, remaining).filterNot(fired)
    fired ++= fresh
    if (remaining > 0) fresh.sorted.headOption else None
  }

  def observe(c: Contest, now: DateTime): Seq[PalevoEvent] = {
    val pc = PContest(c.id, c.name)
    seen.get(c.id) match {
      case None =>
        seen.put(c.id, new State(phaseAt(c, now),
          mutable.Set(reached(countdown.start, remainingMillis(c.startTime, now)): _*),
          mutable.Set(reached(countdown.end, remainingMillis(c.endTime, now)): _*)))
        Nil
      case Some(st) =>
        val phase = phaseAt(c, now)
        val phaseEvents =
          if (phase > st.phase) {
            val from = st.phase
            st.phase = phase
            ((from + 1) to phase).flatMap {
              case Started => Some(ContestStarting(pc, now))
              case Frozen if freezeEnabled(c) => Some(ContestFreezing(pc, now))
              case Finished => Some(ContestEnding(pc, now))
              case _ => None
            }
          } else Nil
        phaseEvents ++ countdownEvents(c, pc, st, now)
    }
  }

  private def countdownEvents(c: Contest, pc: PContest, st: State, now: DateTime): Seq[PalevoEvent] =
    if (c.id > ContestTimeTracker.MaxAnnouncedContest) Nil
    else {
      val starting =
        if (st.phase == 0 && now.isBefore(c.startTime))
          due(st.startFired, countdown.start, remainingMillis(c.startTime, now))
            .map(m => ContestStarting(pc, now, m, Some(c.startTime)))
        else None
      val ending =
        if ((st.phase == Started || st.phase == Frozen) && now.isBefore(c.endTime) && c.endTime.isAfter(c.startTime))
          due(st.endFired, countdown.end, remainingMillis(c.endTime, now))
            .map(m => ContestEnding(pc, now, m, Some(c.endTime)))
        else None
      starting.toSeq ++ ending
    }
}
