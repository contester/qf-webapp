package actors

import javax.inject.Inject

import akka.actor._
import play.api.db.slick.DatabaseConfigProvider
import play.api.libs.iteratee.{Concurrent, Enumerator}
import slick.driver.JdbcProfile

object StatusActor {
  def props = Props[StatusActor]

  case class Join(username: String)
  case class Connected(enumerator:Enumerator[String])
}


class StatusActor @Inject() (dbConfigProvider: DatabaseConfigProvider) extends Actor {
  import StatusActor._
  import scala.concurrent.duration._
  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import utils.Db._
  import dbConfig.driver.api._

  import context.dispatcher
  val tick =
    context.system.scheduler.schedule(60 seconds, 60 seconds, self, "tick")

  val (chatEnumerator, chatChannel) = Concurrent.broadcast[String]

  def receive = {
    case Join(username) => {
      sender ! Connected(chatEnumerator)
    }
    case "tick" => {
      println("tick")
      //chatChannel.push("blah")
    }
  }
}
