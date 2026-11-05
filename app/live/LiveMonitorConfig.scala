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
  def apply(c: Configuration): LiveMonitorConfig =
    LiveMonitorConfig(
      enabled = !c.getOptional[Boolean]("monitor.disable_new_monitor").getOrElse(false),
      staticInterval = c.getOptional[FiniteDuration]("monitor.static_interval").getOrElse(30.seconds),
      crosscheckInterval = c.getOptional[FiniteDuration]("monitor.crosscheck_interval").getOrElse(60.seconds),
      publicContests = c.getOptional[Seq[Int]]("monitor.public_contests").map(_.toSet).getOrElse((1 to 10).toSet),
      publicMaxConnections = c.getOptional[Int]("monitor.public_max_connections").getOrElse(1000))
}
