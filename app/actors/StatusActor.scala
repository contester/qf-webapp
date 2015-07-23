package actors

import akka.actor.{Props, Actor, ActorRef}
import models.{Contest, Contests, LoggedInTeam}
import play.api.Logger
import play.api.libs.iteratee.Concurrent.Channel
import play.api.libs.iteratee.{Concurrent, Enumerator}
import play.api.libs.json.{Writes, Json, JsValue}
import slick.jdbc.JdbcBackend

import scala.collection.mutable

object StatusActor {
  def props(db: JdbcBackend#DatabaseDef) = Props(classOf[StatusActor], db)

  case object Tick
  case class Join(loggedInTeam: LoggedInTeam)
  case class Connected(enumerator: Enumerator[JsValue])

  implicit val contestWrites = new Writes[Contest] {
    def writes(c: Contest) = Json.obj(
      "name" -> c.name,
      "started" -> c.started
    )
  }
}

class StatusActor(db: JdbcBackend#DatabaseDef) extends Actor {
  import StatusActor._
  import context.dispatcher

  import scala.concurrent.duration._
  val tick =
    context.system.scheduler.schedule(10 seconds, 10 seconds, self, Tick)

  import com.github.nscala_time.time.Imports._
  var lastTimestamp = DateTime.now

  val contestBroadcasts = mutable.Map[Int, (Enumerator[JsValue], Channel[JsValue])]()
  val contestStates = mutable.Map[Int, Contest]()


  private def newCBr(id: Int) =
    synchronized {
      contestBroadcasts.getOrElseUpdate(id, Concurrent.broadcast[JsValue])
    }

  private def contestToJson(c: Contest) =
    Json.obj("kind" -> "contest", "data" -> Json.toJson(c))

  def receive = {
    case Tick => {

      db.run(Contests.getContests).map { contests =>
        println(contests)
        for (c <- contests) {
          contestStates.get(c.id) match {
            case Some(oldState) => {
              if (oldState != c) {
                contestStates.put(c.id, c)
                newCBr(c.id)._2.push(contestToJson(c))
              }
            }
            case None =>
              contestStates.put(c.id, c)
              newCBr(c.id)._2.push(contestToJson(c))
          }
        }
      }

      //out ! foo
    }

    case Join(loggedInTeam) => {
      val cid = loggedInTeam.contest.id

      val br = newCBr(cid)
      val result = contestStates.get(cid) match {
        case Some(o) => Enumerator[JsValue](contestToJson(o))
        case None => Enumerator.empty[JsValue]
      }

      sender ! Connected(result.andThen(br._1))
    }
  }

  @throws[Exception](classOf[Exception])
  override def postStop(): Unit = {
    Logger.debug(s"Disconnected")
    tick.cancel()
    super.postStop()
  }
}
