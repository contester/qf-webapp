package actors

import akka.actor.{Actor, ActorRef, Props}
import play.api.libs.iteratee.Enumerator

object NarrowActor {
  def props(out: ActorRef, foo: String) = Props(classOf[NarrowActor], out, foo)

  case class Join(username: String)
  case class Connected(enumerator:Enumerator[String])
}

class NarrowActor(out: ActorRef, foo: String) extends Actor {
  import context.dispatcher

  import scala.concurrent.duration._
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
