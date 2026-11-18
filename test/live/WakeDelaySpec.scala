package live

import actors.LiveMonitorActor
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.duration._

class WakeDelaySpec extends AnyFunSuite {
  private val now = new DateTime(2026, 11, 1, 10, 0)

  test("a deadline is waited for exactly, plus a margin so the clock is past it") {
    assert(LiveMonitorActor.wakeDelay(now.plusSeconds(10), now) == 10.seconds + 50.millis)
  }

  test("a deadline hours away is still waited for exactly: there is no heartbeat any more") {
    assert(LiveMonitorActor.wakeDelay(now.plusHours(10), now) == 10.hours + 50.millis)
  }

  test("a deadline years away is capped at 7 days, which the Akka scheduler accepts; the wake re-arms from there") {
    val d = LiveMonitorActor.wakeDelay(now.plusYears(70), now)
    assert(d == 7.days && d < 248.days)
  }

  test("a deadline that is already here or past means a wake in 50 ms") {
    assert(LiveMonitorActor.wakeDelay(now, now) == 50.millis)
    assert(LiveMonitorActor.wakeDelay(now.minusMinutes(5), now) == 50.millis)
  }
}
