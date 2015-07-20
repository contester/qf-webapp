package actors

import javax.inject.Inject

import akka.actor.{ActorRef, Actor, Props}
import play.api.db.slick.DatabaseConfigProvider
import play.api.libs.iteratee.{Concurrent, Enumerator}
import slick.driver.JdbcProfile

object NarrowActor {
  def props(out: ActorRef, foo: String) = Props(classOf[NarrowActor], out, foo)

  case class Join(username: String)
  case class Connected(enumerator:Enumerator[String])
}

class NarrowActor(out: ActorRef, foo: String) extends Actor {
  import NarrowActor._
  import scala.concurrent.duration._

  import context.dispatcher
  val tick =
    context.system.scheduler.schedule(60 seconds, 60 seconds, self, "tick")

  def receive = {
    case "tick" => {
      println("tick", foo)
      out ! foo
    }
  }

  @throws[Exception](classOf[Exception])
  override def postStop(): Unit = {
    println("disconnected", foo)
    tick.cancel()
    super.postStop()
  }
}
