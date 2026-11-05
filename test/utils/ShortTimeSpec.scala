package utils

import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.funsuite.AnyFunSuite

class ShortTimeSpec extends AnyFunSuite {
  test("seconds only when they are not zero") {
    assert(ShortTime.format(new DateTime(2026, 11, 5, 10, 0, 0)) == "2026-11-05 10:00")
    assert(ShortTime.format(new DateTime(2026, 11, 5, 10, 0, 30)) == "2026-11-05 10:00:30")
    assert(ShortTime.format(new DateTime(2026, 11, 5, 9, 5, 0, 500)) == "2026-11-05 09:05")
  }

  test("a time in another zone is shown in the server's zone") {
    val utc = new DateTime(2026, 11, 5, 10, 0, 0, DateTimeZone.UTC)
    assert(ShortTime.format(utc) == utc.withZone(DateTimeZone.getDefault).toString("yyyy-MM-dd HH:mm"))
  }
}
