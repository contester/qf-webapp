package actors

import akka.actor.{Actor, Props}
import controllers.FinishedTesting
import models._
import play.api.Logger
import play.api.libs.iteratee.Concurrent.Channel
import play.api.libs.iteratee.{Concurrent, Enumerator}
import play.api.libs.json.{JsValue, Json, Writes}
import slick.jdbc.JdbcBackend

import scala.collection.mutable

object StatusActor {
  def props(db: JdbcBackend#DatabaseDef) = Props(classOf[StatusActor], db)

  case object Tick
  case object RefreshTick
  case class Join(loggedInTeam: LoggedInTeam)
  case class Connected(enumerator: Enumerator[JsValue])
  case class NewContestState(c: Contest)

  implicit val contestWrites = new Writes[Contest] {
    def writes(c: Contest) = {
      import com.github.nscala_time.time.Imports._
      if (!c.started) {
        Json.obj(
          "name" -> c.name,
          "started" -> c.started,
          "timeval" -> (DateTime.now to c.startTime).toDurationMillis
        )
      } else {
        if (!c.finished) {
          Json.obj(
            "name" -> c.name,
            "started" -> c.started,
            "frozen" -> c.frozen,
            "timeval" -> (DateTime.now to c.endTime).toDurationMillis
          )
        } else {
          Json.obj(
            "name" -> c.name,
            "started" -> c.started,
            "ended" -> c.finished,
            "frozen" -> c.frozen,
            "timeval" -> 0
          )
        }
      }
    }
  }
}

class StatusActor(db: JdbcBackend#DatabaseDef) extends Actor {
  import StatusActor._
  import context.dispatcher

  import scala.concurrent.duration._
  val tick =
    context.system.scheduler.schedule(0 seconds, 10 seconds, self, Tick)

  val refreshTick =
    context.system.scheduler.schedule(30 seconds, 30 seconds, self, RefreshTick)

  import com.github.nscala_time.time.Imports._
  var lastTimestamp = DateTime.now

  val contestBroadcasts = mutable.Map[Int, (Enumerator[JsValue], Channel[JsValue])]()
  val contestStates = mutable.Map[Int, Contest]()
  val teamBroadcasts = mutable.Map[(Int, Int), (Enumerator[JsValue], Channel[JsValue])]()


  private def newCBr(id: Int) =
    synchronized {
      contestBroadcasts.getOrElseUpdate(id, Concurrent.broadcast[JsValue])
    }

  private def newTeamBr(contest: Int, team: Int) =
    teamBroadcasts.getOrElseUpdate((contest, team), Concurrent.broadcast[JsValue])

  private def finishedToJson(s: AnnoSubmit) =
    Json.obj("kind" -> "submit", "data" -> Json.toJson(s))

  private def contestToJson(c: Contest) =
    Json.obj("kind" -> "contest", "data" -> Json.toJson(c))

  def receive = {
    case finished: FinishedTesting => {
      Logger.info(s"FT: $finished")

      SubmitResult.annotateFinished(db, finished).map { annotated =>
        self ! annotated
      }

      sender ! ()
    }

    case annotated: AnnoSubmit => {
      Logger.info(s"${finishedToJson(annotated)}")
      newTeamBr(annotated.contest, annotated.team)._2.push(finishedToJson(annotated))
    }

    case NewContestState(c) => {
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

    case Tick => {
      db.run(Contests.getContests).map { contests =>
        for (c <- contests) {
          self ! NewContestState(c)
        }
      }
    }

    case RefreshTick => {
      contestStates.foreach {
        case (contestId, c) =>
          newCBr(contestId)._2.push(contestToJson(c))
      }
    }

    case Join(loggedInTeam) => {
      val cid = loggedInTeam.contest.id

      val br = newCBr(cid)
      val tr = newTeamBr(cid, loggedInTeam.team.localId)
      val result = contestStates.get(cid) match {
        case Some(o) => Enumerator[JsValue](contestToJson(o))
        case None => Enumerator.empty[JsValue]
      }

      sender ! Connected(result.andThen(br._1.interleave(tr._1)))
    }
  }

  @throws[Exception](classOf[Exception])
  override def postStop(): Unit = {
    Logger.debug(s"Disconnected")
    tick.cancel()
    super.postStop()
  }
}
