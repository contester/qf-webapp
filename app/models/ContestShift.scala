package models

import org.joda.time.{DateTime, Duration}
import org.stingray.contester.dbmodel.Contest

/** The four times of a contest, which move together when the contest starts later or earlier than planned. */
case class ContestTimes(start: DateTime, freeze: DateTime, end: DateTime, expose: DateTime)

object ContestShift {
  /**
   * All four times moved by `delta` (negative: earlier), so durations and "freeze == end means no freeze" are kept.
   * Only a contest that has not started can be shifted, and it cannot be moved to start in the past: that is
   * what editing the times by hand is for.
   */
  def shift(c: Contest, delta: Duration, now: DateTime): Either[String, ContestTimes] =
    if (delta.getMillis == 0) Left("The contest already starts then")
    else if (!now.isBefore(c.startTime)) Left("The contest has already started; edit the times by hand")
    else {
      val moved = ContestTimes(c.startTime.plus(delta), c.freezeTime.plus(delta), c.endTime.plus(delta), c.exposeTime.plus(delta))
      if (moved.start.isBefore(now)) Left("That would start the contest in the past; edit the times by hand")
      else Right(moved)
    }

  /** The contest starts at `start`, and the other times move by as much as the start did. */
  def startAt(c: Contest, start: DateTime, now: DateTime): Either[String, ContestTimes] =
    shift(c, new Duration(c.startTime, start), now)

  /** "5 minutes later", "1 h 5 min 30 s earlier" */
  def describe(delta: Duration): String = {
    val total = math.abs(delta.getStandardSeconds)
    val (h, m, s) = (total / 3600, total % 3600 / 60, total % 60)
    val text = if (total % 60 == 0 && h == 0) s"$m minutes"
      else Seq(h -> "h", m -> "min", s -> "s").filter(_._1 != 0).map { case (n, u) => s"$n $u" }.mkString(" ")
    text + (if (delta.getMillis > 0) " later" else " earlier")
  }
}
