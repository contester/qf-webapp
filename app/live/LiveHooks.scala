package live

import actors.LiveMonitorActor
import akka.actor.ActorRef
import org.stingray.contester.dbmodel.Contest

import scala.util.Try

/**
  * Fire-and-forget notifications of what changed a round's board: for the admin live monitor and for the public
  * board cache. Implementations never throw and never block.
  */
trait LiveHooks {
  def submitArrived(contest: Int, rec: SubmitRec): Unit
  /** A testing finished: the submit's current points (None when it has none). */
  def finished(contest: Int, id: Int, compiled: Boolean, points: Option[Double]): Unit
  def rejudged(id: Int): Unit
  /** Several submits rejudged by one request (a problem rejudge). */
  def rejudgedAll(ids: Seq[Int]): Unit = ids.foreach(rejudged)
  /** The round's participants changed (credential_changed, participant_changed, round_changed). */
  def teamsChanged(roundId: Int): Unit
  /** A contest row changed (or something else of the round: its problems); `contest` is the whole new row. */
  def contestChanged(contest: Contest): Unit
  /** A round vanished or became unusable. */
  def contestRemoved(roundId: Int): Unit
}

object NoopLiveHooks extends LiveHooks {
  override def submitArrived(contest: Int, rec: SubmitRec): Unit = ()
  override def finished(contest: Int, id: Int, compiled: Boolean, points: Option[Double]): Unit = ()
  override def rejudged(id: Int): Unit = ()
  override def teamsChanged(roundId: Int): Unit = ()
  override def contestChanged(contest: Contest): Unit = ()
  override def contestRemoved(roundId: Int): Unit = ()
}

object LiveHooks {
  /** Every call goes to each of the hooks in turn; one that throws does not keep the call from the others. */
  def all(hooks: LiveHooks*): LiveHooks = new LiveHooks {
    private def each(f: LiveHooks => Unit): Unit = hooks.foreach(h => Try(f(h)))

    override def submitArrived(contest: Int, rec: SubmitRec): Unit = each(_.submitArrived(contest, rec))
    override def finished(contest: Int, id: Int, compiled: Boolean, points: Option[Double]): Unit =
      each(_.finished(contest, id, compiled, points))
    override def rejudged(id: Int): Unit = each(_.rejudged(id))
    override def rejudgedAll(ids: Seq[Int]): Unit = each(_.rejudgedAll(ids))
    override def teamsChanged(roundId: Int): Unit = each(_.teamsChanged(roundId))
    override def contestChanged(contest: Contest): Unit = each(_.contestChanged(contest))
    override def contestRemoved(roundId: Int): Unit = each(_.contestRemoved(roundId))
  }
}

class ActorLiveHooks(ref: ActorRef) extends LiveHooks {
  private def tell(msg: Any): Unit = {
    Try(ref ! msg)
    ()
  }

  override def submitArrived(contest: Int, rec: SubmitRec): Unit = tell(LiveMonitorActor.Arrived(contest, rec))

  override def finished(contest: Int, id: Int, compiled: Boolean, points: Option[Double]): Unit =
    tell(LiveMonitorActor.Finished(contest, id, compiled, points))

  override def rejudged(id: Int): Unit = tell(LiveMonitorActor.RejudgeRequested(id))

  override def teamsChanged(roundId: Int): Unit = tell(LiveMonitorActor.TeamsChanged(roundId))

  override def contestChanged(contest: Contest): Unit = tell(LiveMonitorActor.ContestChanged(contest))

  override def contestRemoved(roundId: Int): Unit = tell(LiveMonitorActor.ContestRemoved(roundId))
}
