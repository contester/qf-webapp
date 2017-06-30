package org.stingray.qf.actors

import akka.actor.Actor
import models.Submit

object ReactiveMonitor {
  case class UpdateSubmit(s: Submit)
}

class ReactiveMonitor extends Actor {
  override def receive: Receive = {
    case _ => ()
  }
}