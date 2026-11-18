package org.stingray.contester.dbmodel

import org.joda.time.{DateTime, DateTimeUtils}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

class ContestFrozenSpec extends AnyFunSuite with BeforeAndAfterAll {
  private def t(h: Int, m: Int = 0) = new DateTime(2026, 11, 1, h, m)

  // starts 10:00, ends 15:00: the start and the end are the freeze and expose times
  private val contest = Contest(1, "C", t(10), t(15), None, 1)

  private def frozenAt(c: Contest, now: DateTime) = {
    DateTimeUtils.setCurrentMillisFixed(now.getMillis)
    c.frozen
  }

  override protected def afterAll(): Unit = {
    DateTimeUtils.setCurrentMillisSystem()
    super.afterAll()
  }

  test("freeze and expose are the start and the end") {
    assert(contest.freezeTime == contest.startTime && contest.exposeTime == contest.endTime)
  }

  test("not frozen before the start") {
    assert(!frozenAt(contest, t(9, 59)))
  }

  test("frozen while the round runs") {
    assert(frozenAt(contest, t(10)))
    assert(frozenAt(contest, t(14, 59)))
  }

  test("not frozen once the round ends") {
    assert(!frozenAt(contest, t(15)))
    assert(!frozenAt(contest, t(18)))
  }
}
