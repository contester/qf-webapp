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

  /** What is remembered of a contest: its phase (never goes back), the row's times and the countdown thresholds already used. */
  private final class State(var phase: Int, var row: Contest, var startFired: mutable.Set[Int], var endFired: mutable.Set[Int])

  private val seen = mutable.Map[Int, State]()

  private def freezeEnabled(c: Contest) = c.freezeTime.isBefore(c.endTime)

  private def phaseAt(c: Contest, now: DateTime): Int =
    if (!now.isBefore(c.endTime)) Finished
    else if (freezeEnabled(c) && !now.isBefore(c.freezeTime)) Frozen
    else if (!now.isBefore(c.startTime)) Started
    else 0

  /** Different instants; the same instant in another zone is not a difference. */
  private def differs(a: DateTime, b: DateTime): Boolean = a.getMillis != b.getMillis

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
        seen.put(c.id, new State(phaseAt(c, now), c,
          mutable.Set(reached(countdown.start, remainingMillis(c.startTime, now)): _*),
          mutable.Set(reached(countdown.end, remainingMillis(c.endTime, now)): _*)))
        Nil
      case Some(st) =>
        val rescheduled = rescheduleEvents(c, pc, st, now)
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
        rescheduled ++ phaseEvents ++ countdownEvents(c, pc, st, now)
    }
  }

  /**
    * One event for an edit of the start, freeze or end (freeze only while it is enabled before or after the edit).
    * The edit restarts the countdowns against the new times; thresholds already reached under them are used up silently.
    * Expose and name changes are remembered but not announced.
    */
  private def rescheduleEvents(c: Contest, pc: PContest, st: State, now: DateTime): Seq[PalevoEvent] = {
    val old = st.row
    st.row = c
    val startMoved = differs(c.startTime, old.startTime)
    val endMoved = differs(c.endTime, old.endTime)
    val changes = Seq(
      if (startMoved) Some(TimeChange("start", old.startTime, c.startTime)) else None,
      if (differs(c.freezeTime, old.freezeTime) && (freezeEnabled(old) || freezeEnabled(c)))
        Some(TimeChange("freeze", old.freezeTime, c.freezeTime)) else None,
      if (endMoved) Some(TimeChange("end", old.endTime, c.endTime)) else None
    ).flatten
    if (startMoved) st.startFired = mutable.Set(reached(countdown.start, remainingMillis(c.startTime, now)): _*)
    if (endMoved) st.endFired = mutable.Set(reached(countdown.end, remainingMillis(c.endTime, now)): _*)
    if (changes.nonEmpty && c.id <= ContestTimeTracker.MaxAnnouncedContest) Seq(ContestRescheduled(pc, now, changes))
    else Nil
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
