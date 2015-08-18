package actors

import akka.actor.{Actor, Props}
import controllers.{CustomTestResult, FinishedTesting}
import models._
import play.api.Logger
import play.api.libs.iteratee.Concurrent.Channel
import play.api.libs.iteratee.{Concurrent, Enumerator}
import play.api.libs.json.{JsValue, Json, Writes}
import slick.jdbc.{GetResult, JdbcBackend}

import scala.collection.mutable

object StatusActor {
  def props(db: JdbcBackend#DatabaseDef) = Props(classOf[StatusActor], db)

  case object Init
  case object Tick
  case object RefreshTick
  case class Join(loggedInTeam: LoggedInTeam)
  case class Connected(enumerator: Enumerator[JsValue])
  case class NewContestState(c: Contest)
  case class Ack(loggedInTeam: LoggedInTeam, msgid: Int)

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

case class Message2(id: Int, contest: Int, team: Int, kind: String, data: JsValue) {
  def asKJson = Json.obj("kind" -> kind, "msgid" -> id, "data" -> data)
}

object Message2 {
  implicit val getResult = GetResult(r =>
    Message2(r.nextInt(), r.nextInt(), r.nextInt(), r.nextString(), Json.parse(r.nextString()))
  )
}

class StatusActor(db: JdbcBackend#DatabaseDef) extends Actor {
  import StatusActor._
  import context.dispatcher

  import scala.concurrent.duration._
  val tick =
    context.system.scheduler.schedule(0 seconds, 10 seconds, self, Tick)

  val refreshTick =
    context.system.scheduler.schedule(30 seconds, 30 seconds, self, RefreshTick)

  context.system.scheduler.scheduleOnce(0 seconds, self, Init)

  import com.github.nscala_time.time.Imports._

  val contestBroadcasts = mutable.Map[Int, (Enumerator[JsValue], Channel[JsValue])]()
  val contestStates = mutable.Map[Int, Contest]()
  val teamBroadcasts = mutable.Map[(Int, Int), (Enumerator[JsValue], Channel[JsValue])]()

  val unacked = mutable.Map[(Int, Int), mutable.Map[Int, Message2]]()

  private def newCBr(id: Int) =
    synchronized {
      contestBroadcasts.getOrElseUpdate(id, Concurrent.broadcast[JsValue])
    }

  private def newTeamBr(contest: Int, team: Int) =
    teamBroadcasts.getOrElseUpdate((contest, team), Concurrent.broadcast[JsValue])

  private def contestToJson(c: Contest) =
    Json.obj("kind" -> "contest", "data" -> Json.toJson(c))

  private def getUnacked(contest: Int, team: Int) =
    unacked.getOrElseUpdate((contest, team), mutable.Map[Int, Message2]())

  import slick.driver.MySQLDriver.api._

  private def pushPersistent(contest: Int, team: Int, kind: String, data: JsValue) =
    db.run(
      sqlu"""insert into Messages2 (Contest, Team, Kind, Value) values ($contest, $team, $kind, ${Json.stringify(data)})"""
        .andThen(sql"select last_insert_id()".as[Int]).withPinnedSession).map { iid =>
      for (id <- iid) {
        self ! Message2(id, contest, team, kind, data)
      }
    }

  private def loadPersistentMessages =
    db.run(
      sql"""select ID, Contest, Team, Kind, Value from Messages2 where Seen != 1""".as[Message2]
    ).map { msgs =>
      for(msg <- msgs) {
        self ! msg
      }
    }

  def receive = {
    case Init => {
      loadPersistentMessages
    }

    case finished: FinishedTesting => {
      Logger.info(s"FT: $finished")

      SubmitResult.annotateFinished(db, finished).map { annotated =>
        self ! annotated
      }

      sender ! ()
    }

    case Ack(loggedInTeam, msgid) => {
      getUnacked(loggedInTeam.contest.id, loggedInTeam.team.localId) -= msgid
      db.run(sqlu"update Messages2 set Seen = 1 where ID = $msgid")
    }

    case annotated: AnnoSubmit => {
      pushPersistent(annotated.contest, annotated.team, "submit", Json.toJson(annotated))
    }

    case evalDone: CustomTestResult => {
      pushPersistent(evalDone.contest, evalDone.team, "custom", Json.toJson(evalDone))
      sender ! ()
    }

    case msg2: Message2 => {
      val m = getUnacked(msg2.contest, msg2.team)
      m += (msg2.id -> msg2)
      newTeamBr(msg2.contest, msg2.team)._2.push(msg2.asKJson)
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

      val stored = getUnacked(loggedInTeam.contest.id, loggedInTeam.team.localId).map {
        case (msgid, msg) => msg.asKJson
      }

      val eStored = if (stored.isEmpty) Enumerator.empty[JsValue] else Enumerator.enumerate[JsValue](stored)

      sender ! Connected(result.andThen(eStored).andThen(br._1.interleave(tr._1)))
    }
  }

  @throws[Exception](classOf[Exception])
  override def postStop(): Unit = {
    Logger.debug(s"Disconnected")
    tick.cancel()
    super.postStop()
  }
}
