package live

import play.api.Configuration

/**
  * The live monitor's settings. The old interval keys `monitor.static_interval` and `monitor.crosscheck_interval` are
  * no longer read (and ignored when present): the static file is written on change and there is no cross-check.
  */
case class LiveMonitorConfig(
  enabled: Boolean,
  publicContests: Set[Int],
  publicMaxConnections: Int)

object LiveMonitorConfig {
  private val disabledDefaults = LiveMonitorConfig(
    enabled = false, publicContests = (1 to 10).toSet, publicMaxConnections = 1000)

  /**
   * With `monitor.disable_new_monitor = true` only that flag is read, so a malformed live key can never stop the
   * rollback switch from working. When enabled, every live key is parsed strictly: a bad value fails boot.
   */
  def apply(c: Configuration): LiveMonitorConfig =
    if (c.getOptional[Boolean]("monitor.disable_new_monitor").getOrElse(false)) disabledDefaults
    else LiveMonitorConfig(
      enabled = true,
      publicContests = c.getOptional[Seq[Int]]("monitor.public_contests").map(_.toSet).getOrElse(disabledDefaults.publicContests),
      publicMaxConnections = c.getOptional[Int]("monitor.public_max_connections").getOrElse(disabledDefaults.publicMaxConnections))
}
