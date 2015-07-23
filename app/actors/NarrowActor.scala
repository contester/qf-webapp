package actors

import akka.actor.{Actor, ActorRef, Props}
import models.LoggedInTeam
import play.api.Logger
import play.api.libs.iteratee.Enumerator
import slick.jdbc.JdbcBackend

object NarrowActor {
  def props(out: ActorRef, foo: String) = Props(classOf[NarrowActor], out, foo)

  case object Tick

}

class NarrowActor(out: ActorRef, loggedIn: LoggedInTeam, db: JdbcBackend#DatabaseDef) extends Actor {
  import NarrowActor._
  import context.dispatcher

  import scala.concurrent.duration._
  val tick =
    context.system.scheduler.schedule(60 seconds, 60 seconds, self, Tick)

  import com.github.nscala_time.time.Imports._
  var lastTimestamp = DateTime.now



  def receive = {
    case Tick => {
      println("tick")
      //out ! foo
    }
  }

  @throws[Exception](classOf[Exception])
  override def postStop(): Unit = {
    Logger.debug(s"Disconnected: $loggedIn")
    tick.cancel()
    super.postStop()
  }
}
