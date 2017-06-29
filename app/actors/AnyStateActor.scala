package org.stingray.qf.actors

import akka.actor.{Actor, Stash}

import scala.concurrent.Future
import scala.util.{Failure, Success}

object AnyStateActor {
  case object Start
}

trait AnyStateActor[StateType] extends Actor with Stash {
  import AnyStateActor._
  import context.dispatcher

  case class State(m: StateType)

  @throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    self ! Start
  }

  def loadStart(): Future[StateType]
  def setState(v: StateType)

  override def receive: Receive = {
    case Start => {
      loadStart().onComplete {
        case Success(v) => self ! State(v)
        case Failure(_) => {
          import scala.concurrent.duration._
          import scala.language.postfixOps
          context.system.scheduler.scheduleOnce(20 seconds, self, Start)
        }
      }
    }
    case State(m) => {
      setState(m)
      unstashAll()
      context.become(initialized)
    }
    case _ => stash()
  }

  def initialized: Receive
}