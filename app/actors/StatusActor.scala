package actors

import akka.NotUsed
import akka.actor.{Actor, Props, Stash}
import akka.stream.scaladsl.{BroadcastHub, Keep, Merge, Sink, Source}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import models.ContesterResults.{CustomTestResult, FinishedTesting}
import models._
import play.api.Logger
import play.api.libs.EventSource
import play.api.libs.EventSource.{Event, EventDataExtractor, EventNameExtractor}
import play.api.libs.json._
import slick.jdbc.{GetResult, JdbcBackend}
import utils.{Ask, Concur}

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{Success, Try}

object StatusActor {
  def props(db: JdbcBackend#DatabaseDef) = Props(classOf[StatusActor], db)

  case object Init
  case object Tick
  case object RefreshTick
  case class NewMultiContestState(cs: Iterable[Contest])
  case class Ack(loggedInTeam: LoggedInTeam, msgid: Int)
  case class JoinAdmin(c: Int)
  case class JoinUser(contest: Int, team: Int)
  case class UserJoined(enumerator: Source[Event, NotUsed])

  case object GetAllContests
  case class AllContests(contests: Seq[Contest])

  case class ClarificationRequested(contest: Int, clrId: Int)
  case class ClarificationAnswered(contest: Int, clrId: Int, teamId: Int, problem: String, text: String)
  case class ClarificationRequestsStoredState(values: Map[Int, Seq[Int]])

  case class ClarificationsInitialState(clr: Seq[Clarification], seen: Seq[MaxSeen])
  case class DeleteClarification(id: Int)
  case class GetVisibleClarifications(contestId: Int)

  case class ClarificationState(contest: Int, team: Int, unseen: Boolean)
  object ClarificationState {
    implicit val format = Json.format[ClarificationState]
    implicit val eventNameExtractor = EventNameExtractor[ClarificationState](_ => Some("clarificationState"))
    implicit val eventDataExtractor = EventDataExtractor[ClarificationState] { state =>
      Json.stringify(Json.toJson(state))
    }
  }

  case class ClarificationPosted(id: Int, contest: Int, updated: Boolean, problem: Option[String], text: String)
  object ClarificationPosted {
    implicit val format = Json.format[ClarificationPosted]
    implicit val eventNameExtractor = EventNameExtractor[ClarificationPosted](_ => Some("clarificationPosted"))
    implicit val eventDataExtractor = EventDataExtractor[ClarificationPosted] { state =>
      Json.stringify(Json.toJson(state))
    }
  }

  case class AckAllClarifications(contest: Int, team: Int)

  private val userPing = Event("", None, Some("ping"))

  case class StatusActorInitialState(msgs: Iterable[Message2],
                                     storedClarificationRequests: Map[Int, Seq[Int]],
                                     clarifications: ClarificationsInitialState)
}

class StatusActor(db: JdbcBackend#DatabaseDef) extends Actor with Stash {
  import StatusActor._
  import context.dispatcher

  import scala.language.postfixOps

  implicit val msg2eventNameExtractor = EventNameExtractor[Message2](x => Some(x.kind))
  implicit val msg2eventDataExtractor = EventDataExtractor[Message2] { msg2 =>
    Json.stringify(Json.fromJson[JsObject](msg2.data).get + ("msgid" -> JsNumber(msg2.id.getOrElse(0).toInt)))
  }


  @throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    self ! Init
  }


  private val tick = {
    import scala.concurrent.duration._
    context.system.scheduler.schedule(0 seconds, 10 seconds, self, Tick)
  }

  private val tickDuration = {
    import scala.concurrent.duration._
    30 seconds
  }

  private val (contestOut2, contestChannel2) = Concur.broadcast[Contest]()
  private val contestStates = mutable.Map[Int, Contest]()
  private val (msg2Out, msg2Channel) = Concur.broadcast[Message2]()
  private val unacked = mutable.Map[(Int, Int), mutable.Map[Int, Message2]]()
  private val (sub2Out, sub2Chan) = Concur.broadcast[AnnoSubmit]()
  private val pendingClarificationRequests = mutable.Map[Int, mutable.Set[Int]]().withDefaultValue(mutable.Set[Int]())
  private val (clr2Out, clr2Chan) = Concur.broadcast[ClarificationRequestState]()
  private val (clrPostOut, clrPostChannel) = Concur.broadcast[ClarificationPosted]()
  private val (subOut, subChan) = Concur.broadcast[FullyDescribedSubmit]()

  private val clarifications = {
    mutable.Map[Int, mutable.Map[Int, Clarification]]()
  }
  private val clarificationsSeen = {
    import com.github.nscala_time.time.Imports._

    mutable.Map[(Int, Int), DateTime]()
  }

  private def getUnacked(contest: Int, team: Int) =
    unacked.getOrElseUpdate((contest, team), mutable.Map[Int, Message2]())

  import slick.jdbc.MySQLProfile.api._

  private def pushPersistent(contest: Int, team: Int, kind: String, data: JsValue) =
    db.run(
      (SlickModel.messages2 returning SlickModel.messages2.map(_.id) into ((user, id) => user.copy(id=Some(id)))) += Message2(None, contest, team, kind, data, false))

  private def loadAll() = {
    val f =
      db.run(
        SlickModel.messages2.filter(!_.seen).result zip
        SlickModel.clarificationRequestsUnanswered.result zip
          SlickModel.clarifications.result zip
          SlickModel.clrSeen2.result
      )

    f.failed.foreach {
      case e => Logger.error("loading status actor:", e)
    }

    f.map {
      case (((msgs, clst), clrs), seen2) =>
        val clst2 = clst.groupBy(_._1).mapValues(x => x.map(_._2))
        val sis = StatusActorInitialState(msgs, clst2, ClarificationsInitialState(clrs, seen2))
        Logger.info(s"sis: $sis")
        sis
    }

  }

  private def catchMsg(msg2: Message2): Unit = {
    val m = getUnacked(msg2.contest, msg2.team)
    msg2.id.foreach(m+= _ -> msg2)
    msg2Channel.offer(msg2)
  }

  private def contestStreamSource(contest: Int): Source[Contest, NotUsed] =
    Source.apply[Contest](contestStates.get(contest).toList).concat(contestOut2.filter(_.id == contest))

  private def insertClarification(cl: Clarification) =
    for (id <- cl.id) {
      clarifications.getOrElseUpdate(cl.contest, mutable.Map[Int, Clarification]()).put(id, cl)
    }

  private def deleteClarification(id: Int) =
    clarifications.values.flatMap { m =>
      m.remove(id)
    }.headOption

  override def receive = {
    case Init => {
      Logger.info("LOADING STATUS ACTOR")
      loadAll.foreach { sis =>
        self ! sis
      }
    }

    case StatusActorInitialState(msgs, clrs, cls) => {
      Logger.info(s"STATUS ACTOR initial state received")
      clrs.foreach {
        case (contest, pending) => {
          Logger.info(s"initialState: $contest, $pending")
          val pendingSet = mutable.Set[Int](pending:_*)
          pendingClarificationRequests.put(contest, pendingSet).foreach { old =>
            if (old != pendingSet) {
              clr2Chan.offer(ClarificationRequestState(contest, pending.length, false))
            }
          }
        }
      }

      for (cl <- cls.clr) {
        insertClarification(cl)
      }
      for (s <- cls.seen) {
        clarificationsSeen.put((s.contest, s.team), s.timestamp)
      }

      for (msg <- msgs)
        catchMsg(msg)

      unstashAll()
      context.become(initialized)
    }

    case _ => stash()
  }

  def initialized: Receive = {
    case ClarificationRequested(contest, clrId) => {
      pendingClarificationRequests(contest) += clrId
      clr2Chan.offer(ClarificationRequestState(contest, pendingClarificationRequests(contest).size, true))
    }

    case ClarificationAnswered(contest, clrId, teamId, problem, text) => {
      pendingClarificationRequests(contest) -= clrId
      clr2Chan.offer(ClarificationRequestState(contest, pendingClarificationRequests(contest).size, false))
      pushPersistent(contest, teamId, "clarificationAnswered", Json.obj("problem" -> problem, "text" -> text))
    }

      // This handles both toggles and full edits.
    case cl: Clarification => {
      val orig = cl.id.flatMap(id => clarifications.get(cl.contest).flatMap(_.get(id)))
      val saved = sender()
      ClarificationModel.updateClarification(db, cl).map { opt =>
        val prevVisible = orig.map(!_.hidden).getOrElse(false)
        val next = opt.getOrElse(cl)
        val ifp = if(cl.problem.isEmpty) None else Some(cl.problem)
        if (!next.hidden)
          clrPostChannel.offer(ClarificationPosted(next.id.get, next.contest, prevVisible, ifp, next.text))
        next
      }.onComplete(Ask.respond(saved, _))
    }

    case DeleteClarification(clarificationId) => {
      val saved = sender()
      ClarificationModel.deleteClarification(db, clarificationId).map { _ =>
        deleteClarification(clarificationId)
      }.onComplete(Ask.respond(saved, _))
    }

    case GetVisibleClarifications(contestId) => {
      import com.github.nscala_time.time.Imports._
      val v = clarifications.get(contestId).map(_.values.filter(!_.hidden).toSeq.sortBy(_.arrived).reverse).getOrElse(Seq.empty)
      Ask.respond(sender(), Success(v))
    }

    case AckAllClarifications(contest, team) => {
      import com.github.nscala_time.time.Imports._
      import utils.Db._
      clarifications.get(contest).map { cmap =>
        cmap.values.map(_.arrived).max
      }.foreach { now =>
        clarificationsSeen.put((contest, team), now)
        db.run(sqlu"""replace ClrSeen2 (Contest, Team, MaxSeen) values ($contest, $team, ${now})""")
      }
    }

    case finished: FinishedTesting => {
      Logger.info(s"received finished: $finished")
      SubmitResult.annotateFinished(db, finished).map { annotated =>
        self ! annotated
      }

      sender() ! {}
    }

    case Ack(loggedInTeam, msgid) => {
      getUnacked(loggedInTeam.contest.id, loggedInTeam.team.localId) -= msgid
      val loc = for {c <- SlickModel.messages2 if c.id === msgid } yield c.seen
      val upd = loc.update(true)
      db.run(upd)
    }

    case GetAllContests => {
      sender() ! Success(AllContests(contestStates.values.toSeq))
    }

    case annotated: FullyDescribedSubmit => {
      Logger.info(s"received annotated: $annotated")

      subChan.offer(annotated)
      val a = AnnoSubmit(annotated.submit.submitId.id, annotated.submit.submitId.contestId,
        annotated.submit.submitId.teamId, annotated.submit.submitId.problem.id, annotated.result)
      sub2Chan.offer(a)
      pushPersistent(a.contest, a.team, "submit", Json.toJson(a))
    }

    case evalDone: CustomTestResult => {
      pushPersistent(evalDone.contest, evalDone.team, "custom", Json.toJson(evalDone))
      sender ! {}
    }

    case msg2: Message2 => catchMsg(msg2)

    case NewMultiContestState(cs) => {
      for (c <- cs) {
        val old = contestStates.get(c.id)
        if (old.isEmpty || old.get != c) {
          contestStates.put(c.id, c)
          contestChannel2.offer(c)
        }
      }
    }

    case Tick => {
      Logger.info(s"Tick!")
      db.run(Contests.getContests).map { contests =>
        self ! NewMultiContestState(contests)
      }
    }

    case JoinAdmin(c: Int) => {
      val enum = Try {
        val sev= sub2Out.filter(_.contest == c) via EventSource.flow
        val pings = Source.tick(tickDuration, tickDuration, userPing)
        val clars = Source.apply[ClarificationRequestState](Seq(ClarificationRequestState(c, pendingClarificationRequests(c).size, false)).toList)
            .concat(clr2Out.filter(_.contest == c)) via EventSource.flow
        Source.combine(
          contestStreamSource(c) via EventSource.flow,
          sev, pings, clars)(Merge(_))
      }
      sender ! enum
    }

    case JoinUser(contest: Int, team: Int) => {
      val stored = getUnacked(contest, team).map {
        case (msgid, msg) => msg
      }

      import com.github.nscala_time.time.Imports._

      val issued = clarifications.get(contest).flatMap { v =>
        if (v.isEmpty) None
        else Some(v.values.map(_.arrived).max)
      }

      val lastSeen = clarificationsSeen.get((contest, team))

      val unseen = issued match {
        case Some(ts) => lastSeen match {
          case Some(ls) => ls < ts
          case None => true
        }
        case None => false
      }

      val allSources = Source.combine(
        contestStreamSource(contest) via EventSource.flow,
        Source.apply(stored.toList) via EventSource.flow,
        Source.single(ClarificationState(contest, team, unseen)) via EventSource.flow,
        clrPostOut.filter(_.contest == contest) via EventSource.flow,
        msg2Out.filter(p => p.contest == contest && p.team == team) via EventSource.flow,
        Source.tick(tickDuration, tickDuration, userPing)
      )(Merge(_))

      sender ! UserJoined(allSources)
    }
  }

  @throws[Exception](classOf[Exception])
  override def postStop(): Unit = {
    Logger.debug(s"Disconnected")
    tick.cancel()
    super.postStop()
  }
}
