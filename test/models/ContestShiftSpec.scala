package models

import org.joda.time.{DateTime, Duration}
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Contest

class ContestShiftSpec extends AnyFunSuite {
  private val now = new DateTime(2026, 11, 5, 9, 0)
  private def contest(freezeTime: DateTime = new DateTime(2026, 11, 5, 14, 0)) =
    Contest(1, "c", new DateTime(2026, 11, 5, 10, 0), freezeTime, new DateTime(2026, 11, 5, 15, 0),
      new DateTime(2026, 11, 5, 16, 30), None, "ru", schoolMode = false)

  test("later: all four times move together") {
    val t = ContestShift.shift(contest(), Duration.standardMinutes(5), now).toOption.get
    assert(t == ContestTimes(new DateTime(2026, 11, 5, 10, 5), new DateTime(2026, 11, 5, 14, 5),
      new DateTime(2026, 11, 5, 15, 5), new DateTime(2026, 11, 5, 16, 35)))
  }

  test("earlier, and a contest without a freeze stays without one") {
    val t = ContestShift.shift(contest(freezeTime = new DateTime(2026, 11, 5, 15, 0)), Duration.standardMinutes(-5), now).toOption.get
    assert(t.start == new DateTime(2026, 11, 5, 9, 55) && t.freeze == t.end)
  }

  test("refused: no change, started, or starting in the past") {
    def min(m: Int) = Duration.standardMinutes(m)
    assert(ContestShift.shift(contest(), min(0), now).isLeft)
    assert(ContestShift.shift(contest(), min(5), new DateTime(2026, 11, 5, 10, 0)).isLeft)
    assert(ContestShift.shift(contest(), min(-61), now).isLeft)
    assert(ContestShift.shift(contest(), min(-60), now).isRight)
  }

  test("set the start: the others move by the same amount, to the second") {
    val t = ContestShift.startAt(contest(), new DateTime(2026, 11, 5, 10, 7, 30), now).toOption.get
    assert(t == ContestTimes(new DateTime(2026, 11, 5, 10, 7, 30), new DateTime(2026, 11, 5, 14, 7, 30),
      new DateTime(2026, 11, 5, 15, 7, 30), new DateTime(2026, 11, 5, 16, 37, 30)))
    assert(ContestShift.startAt(contest(), new DateTime(2026, 11, 5, 10, 0), now).isLeft)
    assert(ContestShift.startAt(contest(), new DateTime(2026, 11, 5, 8, 59), now).isLeft)
    assert(ContestShift.startAt(contest(), new DateTime(2026, 11, 7, 10, 0), now).isRight)
  }

  test("describe") {
    assert(ContestShift.describe(Duration.standardMinutes(5)) == "5 minutes later")
    assert(ContestShift.describe(Duration.standardMinutes(-5)) == "5 minutes earlier")
    assert(ContestShift.describe(Duration.standardSeconds(3930)) == "1 h 5 min 30 s later")
    assert(ContestShift.describe(Duration.standardHours(-2)) == "2 h earlier")
  }
}
