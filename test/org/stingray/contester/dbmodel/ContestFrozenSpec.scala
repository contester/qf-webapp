package org.stingray.contester.dbmodel

import org.joda.time.{DateTime, DateTimeUtils}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

class ContestFrozenSpec extends AnyFunSuite with BeforeAndAfterAll {
  private def t(h: Int, m: Int = 0) = new DateTime(2026, 11, 1, h, m)

  // starts 10:00, freezes 14:00, ends 15:00, exposed 16:00
  private def contest(freeze: DateTime = t(14)) =
    Contest(1, "C", t(10), freeze, t(15), t(16), None, "en", schoolMode = false)

  private def frozenAt(c: Contest, now: DateTime) = {
    DateTimeUtils.setCurrentMillisFixed(now.getMillis)
    c.frozen
  }

  override protected def afterAll(): Unit = {
    DateTimeUtils.setCurrentMillisSystem()
    super.afterAll()
  }

  test("not frozen before the freeze") {
    assert(!frozenAt(contest(), t(13, 59)))
  }

  test("frozen from the freeze through the end, until exposed") {
    assert(frozenAt(contest(), t(14)))
    assert(frozenAt(contest(), t(14, 30)))
    assert(frozenAt(contest(), t(15, 30)))
  }

  test("not frozen once exposed") {
    assert(!frozenAt(contest(), t(16)))
    assert(!frozenAt(contest(), t(18)))
  }

  test("freezeTime == endTime means no freeze, including between end and expose") {
    val c = contest(freeze = t(15))
    assert(!frozenAt(c, t(14, 30)))
    assert(!frozenAt(c, t(15)))
    assert(!frozenAt(c, t(15, 30)))
    assert(!frozenAt(c, t(16)))
  }
}
