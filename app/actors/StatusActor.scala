package actors

import akka.actor.{Actor, Props}
import models.ContesterResults.{CustomTestResult, FinishedTesting}
import models._
import play.api.Logger
import play.api.libs.EventSource
import play.api.libs.EventSource.{EventNameExtractor, EventDataExtractor, EventIdExtractor, Event}
import play.api.libs.iteratee.Concurrent.Channel
import play.api.libs.iteratee.{Enumeratee, Concurrent, Enumerator}
import play.api.libs.json._
import slick.jdbc.{GetResult, JdbcBackend}

import scala.collection.mutable
import scala.concurrent.ExecutionContext

object StatusActor {
  def props(db: JdbcBackend#DatabaseDef) = Props(classOf[StatusActor], db)

  case object Init
  case object Tick
  case object RefreshTick
  case class NewContestState(c: Contest)
  case class Ack(loggedInTeam: LoggedInTeam, msgid: Int)
  case class JoinAdmin(c: Int)
  case class AdminJoined(enumerator: Enumerator[Event])
  case class JoinUser(contest: Int, team: Int)
  case class UserJoined(enumerator: Enumerator[Event])
  case class ClarificationRequested(contest: Int, clrId: Int)
  case class ClarificationAnswered(contest: Int, clrId: Int, teamId: Int, problem: String, text: String)
  case class ClarificationRequestsStoredState(values: Map[Int, Seq[Int]])

  private val userPing = Event("", None, Some("ping"))
}

case class Message2(id: Int, contest: Int, team: Int, kind: String, data: JsValue)

object Message2 {
  implicit val getResult = GetResult(r =>
    Message2(r.nextInt(), r.nextInt(), r.nextInt(), r.nextString(), Json.parse(r.nextString()))
  )

  implicit val eventNameExtractor = EventNameExtractor[Message2](x => Some(x.kind))
  implicit val eventDataExtractor = EventDataExtractor[Message2] { msg2 =>
    Json.stringify(Json.fromJson[JsObject](msg2.data).get + ("msgid" -> JsNumber(msg2.id)))
  }
}

class StatusActor(db: JdbcBackend#DatabaseDef) extends Actor {
  import StatusActor._
  import context.dispatcher
  import scala.language.postfixOps

  import scala.concurrent.duration._
  private val tick =
    context.system.scheduler.schedule(0 seconds, 10 seconds, self, Tick)

  private val refreshTick =
    context.system.scheduler.schedule(30 seconds, 30 seconds, self, RefreshTick)

  context.system.scheduler.scheduleOnce(0 seconds, self, Init)

  import com.github.nscala_time.time.Imports._

  private val (contestOut, contestChannel) = Concurrent.broadcast[Contest]
  private val (userPingOut, userPingChannel) = Concurrent.broadcast[Event]

  private val contestStates = mutable.Map[Int, Contest]()
  private val (msg2Out, msg2Channel) = Concurrent.broadcast[Message2]
  private val unacked = mutable.Map[(Int, Int), mutable.Map[Int, Message2]]()
  private val (submitOut, submitChannel) = Concurrent.broadcast[AnnoSubmit]
  private val pendingClarificationRequests = mutable.Map[Int, mutable.Set[Int]]().withDefaultValue(mutable.Set[Int]())
  private val (clrOut, clrChannel) = Concurrent.broadcast[ClarificationRequestState]

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

  private val contestToEvent: Enumeratee[Contest, Event] =
    Enumeratee.map { e => Event(Json.stringify(Json.toJson(e)), None, Some("contest"))}

  private def filterContest(contestId: Int) = Enumeratee.filter[Contest](_.id == contestId)

  private def filterMessage2(contest: Int, team: Int) = Enumeratee.filter[Message2] { msg =>
    msg.contest == contest && msg.team == team
  }

  private def filterSubmits(contestId: Int) = Enumeratee.filter[AnnoSubmit](_.contest == contestId)

  private def filterClarificationRequests(contest: Int) = Enumeratee.filter[ClarificationRequestState](_.contest == contest)

  private val submitToEvent: Enumeratee[AnnoSubmit, Event] =
    Enumeratee.map { e =>
      Event(Json.stringify(Json.toJson(e)), None, Some("submit"))
    }

  private def loadPersistentMessages =
    db.run(
      sql"""select ID, Contest, Team, Kind, Value from Messages2 where Seen != 1""".as[Message2]
    ).map { msgs =>
      for(msg <- msgs) {
        self ! msg
      }
    }

  private def loadClarificationRequestState =
    db.run(
      sql"""select Contest, ID from ClarificationRequests where not Answered""".as[(Int, Int)]
    ).map { msgs =>
      val grp = msgs.groupBy(_._1).mapValues(x => x.map(_._2))
      self ! ClarificationRequestsStoredState(grp)
    }

  def receive = {
    case Init => {
      loadPersistentMessages
      loadClarificationRequestState
    }

    case ClarificationRequestsStoredState(values) => {
      values.foreach {
        case (contest, pending) => {
          val pendingSet = mutable.Set[Int](pending:_*)
          pendingClarificationRequests.put(contest, pendingSet).foreach { old =>
            if (old != pendingSet) {
              clrChannel.push(ClarificationRequestState(contest, pending.length, false))
            }
          }
        }
      }
    }

    case ClarificationRequested(contest, clrId) => {
      pendingClarificationRequests(contest) += clrId
      clrChannel.push(ClarificationRequestState(contest, pendingClarificationRequests(contest).size, true))
    }

    case ClarificationAnswered(contest, clrId, teamId, problem, text) => {
      pendingClarificationRequests(contest) -= clrId
      clrChannel.push(ClarificationRequestState(contest, pendingClarificationRequests(contest).size, false))
      pushPersistent(contest, teamId, "clarificationAnswered", Json.obj("problem" -> problem, "text" -> text))
    }

    case finished: FinishedTesting => {
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
      submitChannel.push(annotated)
      pushPersistent(annotated.contest, annotated.team, "submit", Json.toJson(annotated))
    }

    case evalDone: CustomTestResult => {
      pushPersistent(evalDone.contest, evalDone.team, "custom", Json.toJson(evalDone))
      sender ! {}
    }

    case msg2: Message2 => {
      val m = getUnacked(msg2.contest, msg2.team)
      m += (msg2.id -> msg2)
      msg2Channel.push(msg2)
    }

    case NewContestState(c) => {
      val old = contestStates.get(c.id)
      if (old.isEmpty || old.get != c) {
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

    case JoinAdmin(c: Int) => {
      val contestEvents = Enumerator.enumerate(contestStates.get(c)).andThen(contestOut &> filterContest(c)) &> contestToEvent
      val clrEvents = Enumerator(ClarificationRequestState(c, pendingClarificationRequests(c).size, false))
        .andThen(clrOut &> filterClarificationRequests(c)) &> EventSource()

      sender ! AdminJoined(
        Enumerator.interleave(contestEvents,
        submitOut &> filterSubmits(c) &> submitToEvent,
        clrEvents,
        userPingOut
        ))
    }

    case JoinUser(contest: Int, team: Int) => {
      val stored = getUnacked(contest, team).map {
        case (msgid, msg) => msg
      }

      val eStored = Enumerator.enumerate(stored) &> EventSource()

      sender ! UserJoined(
        Enumerator.enumerate[Contest](contestStates.get(contest)).through(contestToEvent).andThen(eStored).andThen(
          Enumerator.interleave(contestOut &> filterContest(contest) &> contestToEvent,
            msg2Out &> filterMessage2(contest, team) &> EventSource(),
            userPingOut)))
    }
  }

  @throws[Exception](classOf[Exception])
  override def postStop(): Unit = {
    Logger.debug(s"Disconnected")
    tick.cancel()
    super.postStop()
  }
}
