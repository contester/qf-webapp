package live

import actors.LiveMonitorActor
import akka.actor.ActorRef

import scala.util.Try

/** Fire-and-forget notifications for the live monitor. Implementations never throw and never block. */
trait LiveHooks {
  def submitArrived(contest: Int, rec: SubmitRec): Unit
  def finished(contest: Int, id: Int, compiled: Boolean, passed: Int, taken: Int): Unit
  def rejudged(id: Int): Unit
}

object NoopLiveHooks extends LiveHooks {
  override def submitArrived(contest: Int, rec: SubmitRec): Unit = ()
  override def finished(contest: Int, id: Int, compiled: Boolean, passed: Int, taken: Int): Unit = ()
  override def rejudged(id: Int): Unit = ()
}

class ActorLiveHooks(ref: ActorRef) extends LiveHooks {
  private def tell(msg: Any): Unit = {
    Try(ref ! msg)
    ()
  }

  override def submitArrived(contest: Int, rec: SubmitRec): Unit = tell(LiveMonitorActor.Arrived(contest, rec))

  override def finished(contest: Int, id: Int, compiled: Boolean, passed: Int, taken: Int): Unit =
    tell(LiveMonitorActor.Finished(contest, id, compiled, passed, taken))

  override def rejudged(id: Int): Unit = tell(LiveMonitorActor.RejudgeRequested(id))
}
