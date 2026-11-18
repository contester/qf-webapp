package org.stingray.qf.actors

import akka.actor.{Actor, Stash, Timers}
import play.api.Logging

import scala.concurrent.Future
import scala.concurrent.duration._

object AnyStateActor {
  case object Refresh

  private[actors] val MinRetry: FiniteDuration = 1.second
  private[actors] val MaxRetry: FiniteDuration = 30.seconds
}

/**
  * State loaded once at start-up and then only on request (`Refresh`/`doRefresh`, called by the subclass for the
  * change messages it handles); there is no periodic refresh.
  *
  * Loads never overlap: a request while a load runs only marks the state dirty, and exactly one more load starts when
  * the running one is done. So a burst of changes costs at most two loads, and an older load can never overwrite a
  * newer one. A failed load is retried with backoff (1 s doubling to 30 s) on a single timer until one succeeds; that
  * retry is the only timer.
  */
trait AnyStateActor[StateType] extends Actor with Stash with Timers with Logging {
  import AnyStateActor._
  import context.dispatcher

  case class State(m: StateType)
  private case object LoadFailed
  private case object RetryKey

  private[this] var retryIn: FiniteDuration = AnyStateActor.MinRetry
  private[this] var loading = false
  private[this] var dirty = false

  @throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    self ! Refresh
  }

  def loadStart(): Future[StateType]
  def setState(v: StateType)

  /** Starts a load, or, when one is running, asks for one more after it. */
  def doRefresh(): Unit =
    if (loading) dirty = true
    else {
      loading = true
      dirty = false
      val fu = Future.fromTry(scala.util.Try(loadStart())).flatten
      fu.foreach(x => self ! State(x))
      fu.failed.foreach { e =>
        logger.error(s"loading state", e)
        self ! LoadFailed
      }
    }

  /** Common handling of the load results; chained after the subclass's own messages. */
  private def loadResults(initialize: Boolean): Receive = {
    case State(m) =>
      loading = false
      retryIn = AnyStateActor.MinRetry
      setState(m)
      if (initialize) {
        unstashAll()
        context.become(initialized orElse loadResults(initialize = false))
      }
      if (dirty) doRefresh()
    case LoadFailed =>
      loading = false
      // Retry-on-failure only (bounded backoff, one timer), never a periodic refresh. A pending request is covered by it.
      dirty = false
      timers.startSingleTimer(RetryKey, Refresh, retryIn)
      retryIn = (retryIn * 2).min(AnyStateActor.MaxRetry)
  }

  override def receive: Receive = loadResults(initialize = true) orElse {
    case Refresh => doRefresh()
    case _ => stash()
  }

  def initialized: Receive
}
