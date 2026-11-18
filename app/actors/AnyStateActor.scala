package org.stingray.qf.actors

import akka.actor.{Actor, Stash}
import play.api.Logging

import scala.concurrent.Future
import scala.concurrent.duration._

object AnyStateActor {
  case object Refresh

  private[actors] val MinRetry: FiniteDuration = 1.second
  private[actors] val MaxRetry: FiniteDuration = 30.seconds
}

/**
  * State loaded once at start-up and then only on request (`Refresh`, sent by the subclass for the change messages it
  * handles); there is no periodic refresh. A failed load is retried with backoff (1 s doubling to 30 s) until one
  * succeeds; that retry is the only timer.
  */
trait AnyStateActor[StateType] extends Actor with Stash with Logging {
  import AnyStateActor._
  import context.dispatcher

  case class State(m: StateType)
  private case object LoadFailed

  private[this] var retryIn: FiniteDuration = AnyStateActor.MinRetry

  @throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    self ! Refresh
  }

  def loadStart(): Future[StateType]
  def setState(v: StateType)

  def doRefresh(): Unit = {
    val fu = loadStart()
    fu.foreach(x => self ! State(x))
    fu.failed.foreach { e =>
      logger.error(s"loading state", e)
      self ! LoadFailed
    }
  }

  /** Common handling of the load results; subclasses chain it after their own messages. */
  private def loadResults(initialize: Boolean): Receive = {
    case State(m) =>
      retryIn = AnyStateActor.MinRetry
      setState(m)
      if (initialize) {
        unstashAll()
        context.become(initialized orElse loadResults(initialize = false))
      }
    case LoadFailed =>
      // Retry-on-failure only (bounded backoff), never a periodic refresh.
      context.system.scheduler.scheduleOnce(retryIn, self, Refresh)
      retryIn = (retryIn * 2).min(AnyStateActor.MaxRetry)
  }

  override def receive: Receive = loadResults(initialize = true) orElse {
    case Refresh => doRefresh()
    case _ => stash()
  }

  def initialized: Receive
}
