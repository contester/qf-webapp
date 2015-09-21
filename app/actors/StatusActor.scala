package actors

import akka.actor.{Actor, Props}
import controllers.{CustomTestResult, FinishedTesting}
import models._
import play.api.Logger
import play.api.libs.EventSource
import play.api.libs.EventSource.{EventNameExtractor, EventDataExtractor, EventIdExtractor, Event}
import play.api.libs.iteratee.Concurrent.Channel
import play.api.libs.iteratee.{Enumeratee, Concurrent, Enumerator}
import play.api.libs.json.{JsValue, Json, Writes}
import slick.jdbc.{GetResult, JdbcBackend}

import scala.collection.mutable
import scala.concurrent.ExecutionContext

object StatusActor {
  def props(db: JdbcBackend#DatabaseDef) = Props(classOf[StatusActor], db)

  case object Init
  case object Tick
  case object RefreshTick
  case class Join(loggedInTeam: LoggedInTeam)
  case class Connected(enumerator: Enumerator[JsValue])
  case class NewContestState(c: Contest)
  case class Ack(loggedInTeam: LoggedInTeam, msgid: Int)
  case class JoinAdmin(c: Int)
  case class AdminJoined(enumerator: Enumerator[AdminEvent])
  case class AdminEvent(contest: Option[Int], event: Option[String], data: JsValue)
  case class JoinUser(contest: Int, team: Int)
  case class UserJoined(enumerator: Enumerator[UserEvent])

  object AdminEvent {
    implicit val eventDataExtractor = EventDataExtractor[AdminEvent](x => Json.stringify(x.data))
    implicit val eventNameExtractor = EventNameExtractor[AdminEvent](_.event)
  }
}

case class Message2(id: Int, contest: Int, team: Int, kind: String, data: JsValue) {
  def asKJson = Json.obj("kind" -> kind, "contest" -> contest, "msgid" -> id, "data" -> data)
}

object Message2 {
  implicit val getResult = GetResult(r =>
    Message2(r.nextInt(), r.nextInt(), r.nextInt(), r.nextString(), Json.parse(r.nextString()))
  )
}

class StatusActor(db: JdbcBackend#DatabaseDef) extends Actor {
  import StatusActor._
  import context.dispatcher
  import scala.language.postfixOps

  import scala.concurrent.duration._
  val tick =
    context.system.scheduler.schedule(0 seconds, 10 seconds, self, Tick)

  val refreshTick =
    context.system.scheduler.schedule(30 seconds, 30 seconds, self, RefreshTick)

  context.system.scheduler.scheduleOnce(0 seconds, self, Init)

  import com.github.nscala_time.time.Imports._

  val (contestOut, contestChannel) = Concurrent.broadcast[Contest]
  val (userPingOut, userPingChannel) = Concurrent.broadcast[UserEvent]
  val userPing = UserEvent(None, None, Some("ping"), Json.obj())

  val contestStates = mutable.Map[Int, Contest]()
  val teamBroadcasts = mutable.Map[(Int, Int), (Enumerator[JsValue], Channel[JsValue])]()
  val (adminOut, adminChannel) = Concurrent.broadcast[AdminEvent]

  val unacked = mutable.Map[(Int, Int), mutable.Map[Int, Message2]]()

  private def newTeamBr(contest: Int, team: Int) =
    teamBroadcasts.getOrElseUpdate((contest, team), Concurrent.broadcast[JsValue])

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

  private def filterAdmin(contestId: Int)(implicit ec: ExecutionContext) = Enumeratee.filter[AdminEvent] {
    ev: AdminEvent => ev.contest.isEmpty || ev.contest.get == contestId
  }

  private def toUserEvent: Enumeratee[Contest, UserEvent] =
    Enumeratee.map { e => UserEvent(Some(e.id), None, Some("contest"), Json.toJson(e))}

  private def filterUser(contestId: Int, teamId: Int)(implicit ec: ExecutionContext) = Enumeratee.filter[UserEvent] {
    ev: UserEvent => ev.contest.isEmpty || (ev.contest.get == contestId && (ev.team.isEmpty || ev.team.get == teamId))
  }

  private def loadPersistentMessages =
    db.run(
      sql"""select ID, Contest, Team, Kind, Value from Messages2 where Seen != 1""".as[Message2]
    ).map { msgs =>
      for(msg <- msgs) {
        self ! msg
      }
    }

  private def toContestEvent: Enumeratee[Contest, AdminEvent] =
    Enumeratee.map { e => AdminEvent(Some(e.id), Some("contest"), Json.toJson(e))}

  def receive = {
    case Init => {
      loadPersistentMessages
    }

    case finished: FinishedTesting => {
      Logger.info(s"FT: $finished")

      SubmitResult.annotateFinished(db, finished).map { annotated =>
        self ! annotated
      }

      sender ! {}
    }

    case Ack(loggedInTeam, msgid) => {
      getUnacked(loggedInTeam.contest.id, loggedInTeam.team.localId) -= msgid
      db.run(sqlu"update Messages2 set Seen = 1 where ID = $msgid")
    }

    case annotated: AnnoSubmit => {
      adminChannel.push(AdminEvent(Some(annotated.contest), Some("submit"), Json.toJson(annotated)))
      pushPersistent(annotated.contest, annotated.team, "submit", Json.toJson(annotated))
    }

    case evalDone: CustomTestResult => {
      pushPersistent(evalDone.contest, evalDone.team, "custom", Json.toJson(evalDone))
      sender ! {}
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
            contestChannel.push(c)
          }
        }
        case None =>
          contestStates.put(c.id, c)
          contestChannel.push(c)
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
      userPingChannel.push(userPing)
    }

    case Join(loggedInTeam) => {
      val cid = loggedInTeam.contest.id

      val tr = newTeamBr(cid, loggedInTeam.team.localId)

      val stored = getUnacked(loggedInTeam.contest.id, loggedInTeam.team.localId).map {
        case (msgid, msg) => msg.asKJson
      }

      val eStored = if (stored.isEmpty) Enumerator.empty[JsValue] else Enumerator.enumerate[JsValue](stored)

      sender ! Connected(eStored.andThen(tr._1))
    }

    case JoinAdmin(c: Int) => {
      val result = contestStates.get(c) match {
        case Some(o) => Enumerator[Contest](o)
        case None => Enumerator.empty[Contest]
      }
      val res0 = result &> toContestEvent
      val br = contestOut &> toContestEvent &> filterAdmin(c)

      sender ! AdminJoined(res0.andThen(br.interleave(adminOut)))
    }

    case JoinUser(contest: Int, team: Int) => {
      val result = contestStates.get(contest) match {
        case Some(o) => Enumerator[Contest](o)
        case None => Enumerator.empty[Contest]
      }
      val res0 = result &> toUserEvent
      val br = contestOut &> toUserEvent &> filterUser(contest, team)
      sender ! UserJoined(res0.andThen(br.interleave(userPingOut)))
    }
  }

  @throws[Exception](classOf[Exception])
  override def postStop(): Unit = {
    Logger.debug(s"Disconnected")
    tick.cancel()
    super.postStop()
  }
}
