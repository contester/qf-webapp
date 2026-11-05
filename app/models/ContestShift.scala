package models

import org.joda.time.{DateTime, Minutes}
import org.stingray.contester.dbmodel.Contest

/** The four times of a contest, which move together when the contest starts later or earlier than planned. */
case class ContestTimes(start: DateTime, freeze: DateTime, end: DateTime, expose: DateTime)

object ContestShift {
  val MaxMinutes = 240

  /**
   * All four times moved by `minutes` (negative: earlier), so durations and "freeze == end means no freeze" are kept.
   * Only a contest that has not started can be shifted, and it cannot be moved to start in the past: that is
   * what editing the times by hand is for.
   */
  def shift(c: Contest, minutes: Int, now: DateTime): Either[String, ContestTimes] =
    if (minutes == 0 || math.abs(minutes) > MaxMinutes) Left(s"Shift must be 1 to $MaxMinutes minutes, earlier or later")
    else if (!now.isBefore(c.startTime)) Left("The contest has already started; edit the times by hand")
    else {
      val m = Minutes.minutes(minutes)
      val moved = ContestTimes(c.startTime.plus(m), c.freezeTime.plus(m), c.endTime.plus(m), c.exposeTime.plus(m))
      if (moved.start.isBefore(now)) Left("That would start the contest in the past; edit the times by hand")
      else Right(moved)
    }
}
