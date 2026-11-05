package live

import akka.actor.ActorRef

import scala.util.Try

/** The on/off decision for the live monitor, kept free of Play so it can be tested on its own. */
object LiveWiring {
  case class Built(hooks: LiveHooks, monitor: Option[ActorRef])

  /** The old MonitorActor runs only when the live monitor is off: exactly one of them feeds palevo and static files. */
  def startOldMonitor(cfg: LiveMonitorConfig): Boolean = !cfg.enabled

  /** When disabled, `startActor` is never called and the hooks are no-ops. */
  def build(cfg: LiveMonitorConfig, startActor: () => ActorRef): Built =
    if (cfg.enabled) {
      val ref = startActor()
      Built(new ActorLiveHooks(ref), Some(ref))
    } else Built(NoopLiveHooks, None)

  /**
   * Hooks whose target is resolved on first use. StatusActorModel needs hooks while it is being built, and the
   * live monitor needs StatusActorModel's team and problem clients, so the target cannot be passed directly.
   * Never throws.
   */
  def deferred(target: => LiveHooks): LiveHooks = new LiveHooks {
    private lazy val resolved = target

    private def guard(f: LiveHooks => Unit): Unit = {
      Try(f(resolved))
      ()
    }

    override def submitArrived(contest: Int, rec: SubmitRec): Unit = guard(_.submitArrived(contest, rec))

    override def finished(contest: Int, id: Int, compiled: Boolean, passed: Int, taken: Int): Unit =
      guard(_.finished(contest, id, compiled, passed, taken))

    override def rejudged(id: Int): Unit = guard(_.rejudged(id))

    override def teamChanged(contest: Int, team: org.stingray.contester.dbmodel.Team): Unit =
      guard(_.teamChanged(contest, team))

    override def contestChanged(contest: org.stingray.contester.dbmodel.Contest): Unit =
      guard(_.contestChanged(contest))
  }
}
