package models

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Contest

class ContestShiftSpec extends AnyFunSuite {
  private val now = new DateTime(2026, 11, 5, 9, 0)
  private def contest(freezeTime: DateTime = new DateTime(2026, 11, 5, 14, 0)) =
    Contest(1, "c", new DateTime(2026, 11, 5, 10, 0), freezeTime, new DateTime(2026, 11, 5, 15, 0),
      new DateTime(2026, 11, 5, 16, 30), None, "ru", schoolMode = false)

  test("later: all four times move together") {
    val t = ContestShift.shift(contest(), 5, now).toOption.get
    assert(t == ContestTimes(new DateTime(2026, 11, 5, 10, 5), new DateTime(2026, 11, 5, 14, 5),
      new DateTime(2026, 11, 5, 15, 5), new DateTime(2026, 11, 5, 16, 35)))
  }

  test("earlier, and a contest without a freeze stays without one") {
    val t = ContestShift.shift(contest(freezeTime = new DateTime(2026, 11, 5, 15, 0)), -5, now).toOption.get
    assert(t.start == new DateTime(2026, 11, 5, 9, 55) && t.freeze == t.end)
  }

  test("refused: zero, too big, started, or starting in the past") {
    assert(ContestShift.shift(contest(), 0, now).isLeft)
    assert(ContestShift.shift(contest(), 241, now).isLeft)
    assert(ContestShift.shift(contest(), 5, new DateTime(2026, 11, 5, 10, 0)).isLeft)
    assert(ContestShift.shift(contest(), -61, now).isLeft)
    assert(ContestShift.shift(contest(), -60, now).isRight)
  }
}
