package org.stingray.qf.actors

import akka.actor.{Actor, Stash}

import scala.concurrent.Future
import scala.util.{Failure, Success}

object AnyStateActor {
  case object Refresh
}

trait AnyStateActor[StateType] extends Actor with Stash {
  import AnyStateActor._
  import context.dispatcher

  case class State(m: StateType)

  @throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    self ! Refresh
  }

  val defaultRefresh = {
    import scala.concurrent.duration._
    import scala.language.postfixOps
    20 seconds
  }

  def loadStart(): Future[StateType]
  def setState(v: StateType)

  def doRefresh() = {
    val fu = loadStart()
    fu.foreach(x => self ! State(x))
    fu.onComplete { _ =>
      context.system.scheduler.scheduleOnce(defaultRefresh, self, Refresh)
    }
  }

  override def receive: Receive = {
    case Refresh => doRefresh()
    case State(m) => {
      setState(m)
      unstashAll()
      context.become(initialized)
    }
    case _ => stash()
  }

  def initialized: Receive
}