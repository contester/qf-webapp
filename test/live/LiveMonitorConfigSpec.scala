package live

import com.typesafe.config.ConfigFactory
import org.scalatest.funsuite.AnyFunSuite
import play.api.Configuration

import scala.concurrent.duration._

class LiveMonitorConfigSpec extends AnyFunSuite {
  private def cfg(s: String) = LiveMonitorConfig(Configuration(ConfigFactory.parseString(s)))

  test("defaults") {
    val c = cfg("")
    assert(c.enabled)
    assert(c.staticInterval == 30.seconds)
    assert(c.crosscheckInterval == 60.seconds)
    assert(c.publicContests == (1 to 10).toSet)
    assert(c.publicMaxConnections == 1000)
  }

  test("disable_new_monitor disables") {
    assert(!cfg("monitor.disable_new_monitor = true").enabled)
  }

  test("public_contests override") {
    assert(cfg("monitor.public_contests = [3]").publicContests == Set(3))
  }

  test("crosscheck_interval 0s means disabled") {
    assert(cfg("monitor.crosscheck_interval = 0s").crosscheckInterval == Duration.Zero)
  }
}
