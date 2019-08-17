package models

import akka.actor.{ActorSystem, Props}
import com.spingo.op_rabbit._
import models.ContesterResults.{CustomTestResult, FinishedTesting}
import play.api.{Configuration, Logger, Logging}

import scala.concurrent.ExecutionContext

class RabbitMqModel (system: ActorSystem) {
  val rabbitMq = system.actorOf(Props[RabbitControl], "rabbit-actor")
}

class SubscriptionsModel(rabbitMqModel: RabbitMqModel, statusActorModel: StatusActorModel, configuration: Configuration)(implicit ec: ExecutionContext) extends Logging {
  private val rabbitMq = rabbitMqModel.rabbitMq
  import com.spingo.op_rabbit.PlayJsonSupport._

  import akka.pattern.ask

  import scala.concurrent.duration._

  implicit private val recoveryStrategy = RecoveryStrategy.nack(true)

  val finishedRef = Subscription.run(rabbitMq) {
    import Directives._
    channel(qos = 1) {
      consume(queue("contester.finished")) {
        body(as[FinishedTesting]) { submit =>
          logger.info(s"Received finished submit $submit")
          val acked = statusActorModel.statusActor.ask(submit)(1 minute)
          ack(acked)
        }
      }
    }
  }

  val finishedEvalRef = Subscription.run(rabbitMq) {
    import Directives._
    channel(qos = 1) {
      consume(queue("contester.evals")) {
        body(as[CustomTestResult]) { submit =>
          logger.info(s"Received finished custom test $submit")
          val acked = statusActorModel.statusActor.ask(submit)(1 minute)
          ack(acked)
        }
      }
    }
  }

}