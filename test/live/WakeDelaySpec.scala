package live

import actors.LiveMonitorActor
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.duration._

class WakeDelaySpec extends AnyFunSuite {
  private val now = new DateTime(2026, 11, 1, 10, 0)
  private val heartbeat = 30.seconds

  test("a deadline in the future is waited for exactly, plus a margin so the clock is past it") {
    assert(LiveMonitorActor.wakeDelay(Some(now.plusMinutes(10)), now, heartbeat) == 10.minutes + 50.millis)
  }

  test("a deadline that is already here or past means a wake in 50 ms") {
    assert(LiveMonitorActor.wakeDelay(Some(now), now, heartbeat) == 50.millis)
    assert(LiveMonitorActor.wakeDelay(Some(now.minusMinutes(5)), now, heartbeat) == 50.millis)
  }

  test("no deadline at all means the heartbeat") {
    assert(LiveMonitorActor.wakeDelay(None, now, heartbeat) == heartbeat)
  }
}
