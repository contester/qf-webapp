package live

import play.api.Configuration

import scala.concurrent.duration._

case class LiveMonitorConfig(
  enabled: Boolean,
  staticInterval: FiniteDuration,
  crosscheckInterval: FiniteDuration,
  publicContests: Set[Int],
  publicMaxConnections: Int)

object LiveMonitorConfig {
  private val disabledDefaults = LiveMonitorConfig(
    enabled = false, staticInterval = 30.seconds, crosscheckInterval = 60.seconds,
    publicContests = (1 to 10).toSet, publicMaxConnections = 1000)

  /**
   * With `monitor.disable_new_monitor = true` only that flag is read, so a malformed live key can never stop the
   * rollback switch from working. When enabled, every live key is parsed strictly: a bad value fails boot.
   */
  def apply(c: Configuration): LiveMonitorConfig =
    if (c.getOptional[Boolean]("monitor.disable_new_monitor").getOrElse(false)) disabledDefaults
    else LiveMonitorConfig(
      enabled = true,
      staticInterval = c.getOptional[FiniteDuration]("monitor.static_interval").getOrElse(disabledDefaults.staticInterval),
      crosscheckInterval = c.getOptional[FiniteDuration]("monitor.crosscheck_interval").getOrElse(disabledDefaults.crosscheckInterval),
      publicContests = c.getOptional[Seq[Int]]("monitor.public_contests").map(_.toSet).getOrElse(disabledDefaults.publicContests),
      publicMaxConnections = c.getOptional[Int]("monitor.public_max_connections").getOrElse(disabledDefaults.publicMaxConnections))
}
