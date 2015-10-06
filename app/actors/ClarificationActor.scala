package actors

import akka.actor.{ActorRef, Stash, Props, Actor}
import models.{ContestTeamIds, Clarification}
import play.api.Logger
import play.api.libs.EventSource
import play.api.libs.EventSource.{Event, EventDataExtractor, EventNameExtractor}
import play.api.libs.iteratee.{Enumerator, Concurrent, Enumeratee}
import play.api.libs.json.Json
import slick.jdbc.JdbcBackend

import scala.collection.mutable
import scala.concurrent.ExecutionContext

object ClarificationActor {
  def props(db: JdbcBackend#DatabaseDef) = Props(classOf[ClarificationActor], db)

  sealed trait Transit

  case class InitialState(state: Map[Int, Seq[Int]], seen: Map[ContestTeamIds, Seq[Int]])
  case class Posted(id: Int, contest: Int, problem: Option[String], text: String) extends Transit
  object Posted {
    implicit val format = Json.format[Posted]
    implicit val eventNameExtractor = EventNameExtractor[Posted](_ => Some("clarificationPosted"))
    implicit val eventDataExtractor = EventDataExtractor[Posted] { state =>
      Json.stringify(Json.toJson(state))
    }

    def filterContest(contestId: Int)(implicit ec: ExecutionContext) = Enumeratee.filter[Posted](_.contest == contestId)
  }

  case class Ack(ctid: ContestTeamIds) extends Transit
  case class Joined(ctid: ContestTeamIds, enumerator: Enumerator[Event], orig: ActorRef)
  case class Joining(ctid: ContestTeamIds, orig: ActorRef)

  case class ClarificationState(contest: Int, ids: Seq[Int])
  object ClarificationState {
    implicit val format = Json.format[ClarificationState]
    implicit val eventNameExtractor = EventNameExtractor[ClarificationState](_ => Some("clarificationState"))
    implicit val eventDataExtractor = EventDataExtractor[ClarificationState] { state =>
      Json.stringify(Json.toJson(state))
    }
  }
}

class ClarificationActor(db: JdbcBackend#DatabaseDef) extends Actor with Stash {
  import context.dispatcher
  import slick.driver.MySQLDriver.api._
  import ClarificationActor._

  private def loadState() = {
    db.run(
      sql"""select cl_id, cl_contest_idf, cl_task, cl_text, cl_date, cl_is_hidden from clarifications""".as[Clarification]
    ).zip(db.run(sql"select Contest, Team, Clarification from ClarificationsSeen".as[(Int, Int, Int)])).map {
      case (clarifications, clseen) =>
        Logger.info(s"Loading: $clarifications, $clseen")
        self ! InitialState(clarifications.groupBy(_.contest).mapValues(x => x.map(_.id)),
          clseen.map(x => ContestTeamIds(x._1, x._2) -> x._3).groupBy(_._1).mapValues(x => x.map(_._2)))
    }
  }

  private val issued = mutable.Map[Int, mutable.Set[Int]]()
  private val seen = mutable.Map[ContestTeamIds, mutable.Set[Int]]()

  private val (output, channel) = Concurrent.broadcast[Posted]

  @throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    loadState()
  }

  override def receive: Receive = {
    case InitialState(state, clseen) => {
      state.foreach {
        case (contest, pending) => {
          val issuedSet = mutable.Set[Int](pending:_*)
          issued.put(contest, issuedSet)
        }
      }
      clseen.foreach {
        case (ctid, s) => {
          seen.put(ctid, mutable.Set(s:_*))
        }
      }
      Logger.info(s"Unstashing and becoming")
      unstashAll()
      context.become(initialized)
    }

    case _ =>
      stash()
  }

  def initialized: Receive = {
    case p: Posted => {
      issued.getOrElseUpdate(p.contest, mutable.Set()).add(p.id)
      seen.foreach {
        case (ctid, s) => s.remove(p.id)
      }
      // db.run delete
      channel.push(p)
    }

    case Ack(ctid) => {
      val pending = issued.getOrElse(ctid.contestId, Set[Int]()) -- seen.getOrElse(ctid, Set[Int]())

      Logger.info(s"Acking $pending for $ctid")

      seen.getOrElse(ctid, mutable.Set()).++=(pending)
      for (id <- pending) {
        db.run(sqlu"insert into ClarificationsSeen (Contest, Team, Clarification) values (${ctid.contestId}, ${ctid.teamId}, $id)")
      }
    }

    case Joining(ctid, orig) => {
      val pending = issued.getOrElse(ctid.contestId, Set[Int]()) -- seen.getOrElse(ctid, Set[Int]())
      sender ! Joined(ctid, (Enumerator[ClarificationState](ClarificationState(ctid.contestId, pending.toSeq)) &> EventSource()).andThen(
        output &> Posted.filterContest(ctid.contestId) &> EventSource()
      ), orig)
    }
  }
}
