package live

import org.stingray.contester.dbmodel.Contest

import scala.util.Try

/** Wiring helpers for the live hooks, kept free of Play so they can be tested on their own. */
object LiveWiring {
  /**
   * Hooks whose target is resolved on first use. StatusActorModel needs hooks while it is being built, and the
   * live monitor needs StatusActorModel's team client, so the target cannot be passed directly. Never throws.
   */
  def deferred(target: => LiveHooks): LiveHooks = new LiveHooks {
    private lazy val resolved = target

    private def guard(f: LiveHooks => Unit): Unit = {
      Try(f(resolved))
      ()
    }

    override def submitArrived(contest: Int, rec: SubmitRec): Unit = guard(_.submitArrived(contest, rec))

    override def finished(contest: Int, id: Int, compiled: Boolean, points: Option[Double]): Unit =
      guard(_.finished(contest, id, compiled, points))

    override def rejudged(id: Int): Unit = guard(_.rejudged(id))

    override def teamsChanged(roundId: Int): Unit = guard(_.teamsChanged(roundId))

    override def contestChanged(contest: Contest): Unit = guard(_.contestChanged(contest))

    override def contestRemoved(roundId: Int): Unit = guard(_.contestRemoved(roundId))
  }
}
