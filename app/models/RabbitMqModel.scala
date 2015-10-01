package models

import javax.inject.{Inject, Singleton}

import akka.actor.{Props, ActorSystem}
import com.spingo.op_rabbit.RabbitControl

@Singleton
class RabbitMqModel @Inject() (system: ActorSystem) {
  val rabbitMq = system.actorOf(Props[RabbitControl], "rabbit-actor")
}