package palevo

import actors.RoundClock
import actors.RoundClock.{CountdownToEnd, CountdownToStart, Ended, RoundEdge, Started}
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest

/**
  * Turns the round clock's edges into palevo's contest_starting / contest_ending events ("now", and "in N minutes"
  * before the start and before the end), and an edit of a round's start or end into contest_rescheduled. There is no
  * freeze: a round has a start and an end only.
  *
  * The clock (actors.RoundClock) owns the timing: it fires each edge once, never retroactively after a restart, and
  * for a late wake that crossed several thresholds only the latest. This object only maps; it keeps no state.
  */
object ContestTimeTracker {
  /** Countdowns and reschedules are announced only for contests up to this id; the ones above are test copies. */
  val MaxAnnouncedContest = 10

  /** The countdown of a round as the clock should run it: none for the test copies above MaxAnnouncedContest. */
  def countdownOf(cfg: CountdownConfig): RoundClock.Countdown =
    c => if (c.id > MaxAnnouncedContest) CountdownConfig(Nil, Nil) else cfg

  /** The event for a clock edge of round `c`, observed at `now`. Countdown edges of test copies give nothing. */
  def onEdge(edge: RoundEdge, c: Contest, now: DateTime): Option[PalevoEvent] = {
    val pc = EventBuilders.contestOf(c)
    edge match {
      case _: Started => Some(ContestStarting(pc, now))
      case _: Ended => Some(ContestEnding(pc, now))
      case CountdownToStart(_, m) if announced(c) => Some(ContestStarting(pc, now, m, Some(c.startTime)))
      case CountdownToEnd(_, m) if announced(c) => Some(ContestEnding(pc, now, m, Some(c.endTime)))
      case _ => None
    }
  }

  /**
    * One event for an edit of the start and/or the end of a known round (`old` is its previous row). The same instant in
    * another zone is no change; a name or is_finished change is not a reschedule. Test copies are not announced.
    */
  def rescheduled(old: Contest, c: Contest, now: DateTime): Option[PalevoEvent] = {
    val changes = Seq(
      if (differs(c.startTime, old.startTime)) Some(TimeChange("start", old.startTime, c.startTime)) else None,
      if (differs(c.endTime, old.endTime)) Some(TimeChange("end", old.endTime, c.endTime)) else None
    ).flatten
    if (changes.nonEmpty && announced(c)) Some(ContestRescheduled(EventBuilders.contestOf(c), now, changes)) else None
  }

  private def announced(c: Contest): Boolean = c.id <= MaxAnnouncedContest

  private def differs(a: DateTime, b: DateTime): Boolean = a.getMillis != b.getMillis
}
