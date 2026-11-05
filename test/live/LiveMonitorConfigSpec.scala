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

  private val garbage =
    """monitor.static_interval = "banana"
      |monitor.crosscheck_interval = "x"
      |monitor.public_contests = "nope"
      |monitor.public_max_connections = "many"
      |""".stripMargin

  test("disable_new_monitor = true ignores malformed live keys and returns the defaults") {
    val c = cfg("monitor.disable_new_monitor = true\n" + garbage)
    assert(c == LiveMonitorConfig(enabled = false, 30.seconds, 60.seconds, (1 to 10).toSet, 1000))
  }

  test("enabled: a malformed live key fails fast") {
    for (line <- garbage.linesIterator.filter(_.nonEmpty))
      assertThrows[RuntimeException](cfg("monitor.disable_new_monitor = false\n" + line))
  }
}
