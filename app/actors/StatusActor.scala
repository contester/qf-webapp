package actors

import akka.NotUsed
import akka.actor.{Actor, Props, Stash}
import akka.stream.scaladsl.{BroadcastHub, Keep, Merge, Sink, Source}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import models.ContesterResults.{CustomTestResult, FinishedTesting}
import models._
import palevo.{ContestTimeTracker, CountdownConfig, EventBuilders, PContest, PalevoEvent, PalevoSink}
import org.stingray.contester.dbmodel.{Clarification, Contest, MaxSeen, Message2}
import play.api.{Logger, Logging}
import play.api.libs.EventSource
import play.api.libs.EventSource.{Event, EventDataExtractor, EventNameExtractor}
import play.api.libs.json._
import slick.jdbc.{GetResult, JdbcBackend}
import utils.{Ask, Concur}

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{Success, Try}
import org.joda.time.DateTime

object StatusActor {
  def props(db: JdbcBackend#DatabaseDef, palevo: PalevoSink, teamLookup: (Int, Int) => Future[Option[org.stingray.contester.dbmodel.Team]],
            liveHooks: live.LiveHooks, countdown: CountdownConfig) =
    Props(new StatusActor(db, palevo, teamLookup, liveHooks, countdown))

  /** With the start-up load replaced (tests); optionally a palevo sink and a clock for the round clock and the events. */
  def propsWith(db: JdbcBackend#DatabaseDef, initialLoad: () => Future[StatusActorInitialState],
                sink: PalevoSink = palevo.NoopPalevoSink, now: () => DateTime = () => DateTime.now,
                countdown: CountdownConfig = CountdownConfig.Default): Props =
    Props(new StatusActor(db, sink, (_, _) => Future.successful(None), live.NoopLiveHooks, countdown, Some(initialLoad), now))

  case object Init
  private case object InitFailed
  private case object InitRetryKey
  /**
    * A round changed (round_changed: its row, its problems, or its season's is_current): its new value. Contests are
    * loaded once at start-up; afterwards they change only through this and ContestRemoved, never by a rescan.
    */
  case class ContestChanged(c: Contest)
  /** A round vanished or became unusable (null times, not of the current season): it is dropped everywhere. */
  case class ContestRemoved(roundId: Int)
  /** The participants or credentials of a round changed in the database; handled by TeamStateActor, which reloads it. */
  case class TeamsChanged(roundId: Int)
  case class Ack(loggedInTeam: LoggedInTeam, msgid: Long)
  case class JoinAdmin(c: Int)
  case class JoinUser(contest: Int, team: Int)
  case class UserJoined(enumerator: Source[Event, NotUsed])

  case object GetAllContests
  case class AllContests(contests: Seq[Contest])

  case class GetSingleContest(id: Int)
  case class GetSingleContestResult(c: Option[Contest])

  case class ClarificationRequested(contest: Int, clrId: Long)
  case class ClarificationAnswered(contest: Int, clrId: Long, teamId: Int, problem: String, text: String)
  case class ClarificationRequestsStoredState(values: Map[Int, Seq[Long]])

  case class ClarificationsInitialState(clr: Seq[Clarification], seen: Seq[MaxSeen])
  case class DeleteClarification(id: Long)
  case class GetVisibleClarifications(contestId: Int)

  case class ClarificationState(contest: Int, team: Int, unseen: Boolean)
  object ClarificationState {
    implicit val format = Json.format[ClarificationState]
    implicit val eventNameExtractor = EventNameExtractor[ClarificationState](_ => Some("clarificationState"))
    implicit val eventDataExtractor = EventDataExtractor[ClarificationState] { state =>
      Json.stringify(Json.toJson(state))
    }
  }

  case class ClarificationPosted(id: Long, contest: Int, updated: Boolean, problem: Option[String], text: String)
  object ClarificationPosted {
    implicit val format = Json.format[ClarificationPosted]
    implicit val eventNameExtractor = EventNameExtractor[ClarificationPosted](_ => Some("clarificationPosted"))
    implicit val eventDataExtractor = EventDataExtractor[ClarificationPosted] { state =>
      Json.stringify(Json.toJson(state))
    }
  }

  case class AckAllClarifications(contest: Int, team: Int)

  private val userPing = Event("", None, Some("ping"))
  private val MinInitRetry = scala.concurrent.duration.FiniteDuration(1, "second")
  private val MaxInitRetry = scala.concurrent.duration.FiniteDuration(30, "seconds")

  private def roundJson(c: Contest): JsObject =
    Json.obj("round" -> c.id, "startTime" -> c.startTime.getMillis, "endTime" -> c.endTime.getMillis, "isFinished" -> c.isFinished)

  /**
    * The SSE event for a round edge: `round_started` / `round_ended` with the round id and its times (epoch millis).
    * Countdown edges are for palevo only and give no browser event.
    */
  def roundEvent(edge: RoundClock.RoundEdge, c: Contest): Option[Event] = {
    val name = edge match {
      case _: RoundClock.Started => Some("round_started")
      case _: RoundClock.Ended => Some("round_ended")
      case _: RoundClock.CountdownToStart | _: RoundClock.CountdownToEnd => None
    }
    name.map(n => Event(Json.stringify(roundJson(c)), None, Some(n)))
  }

  /** The SSE event for a round whose start or end moved: `round_rescheduled` with the new times. */
  def rescheduledEvent(c: Contest): Event = Event(Json.stringify(roundJson(c)), None, Some("round_rescheduled"))

  def rescheduled(old: Contest, updated: Contest): Boolean =
    old.startTime.getMillis != updated.startTime.getMillis || old.endTime.getMillis != updated.endTime.getMillis

  /** A round event for the streams of one round (its participants and admins). */
  private case class RoundPush(round: Int, event: Event)

  case class StatusActorInitialState(msgs: Iterable[Message2],
                                     storedClarificationRequests: Map[Int, Seq[Long]],
                                     clarifications: ClarificationsInitialState,
                                     contests: Iterable[Contest])
}

class StatusActor(db: JdbcBackend#DatabaseDef, palevo: PalevoSink,
                  teamLookup: (Int, Int) => Future[Option[org.stingray.contester.dbmodel.Team]],
                  liveHooks: live.LiveHooks,
                  // Palevo's countdown thresholds, run by the round clock.
                  countdown: CountdownConfig,
                  initialLoad: Option[() => Future[StatusActor.StatusActorInitialState]] = None,
                  now: () => DateTime = () => DateTime.now)
  extends Actor with Stash with akka.actor.Timers with Logging {
  import StatusActor._
  import context.dispatcher

  import scala.language.postfixOps

  implicit val msg2eventNameExtractor = EventNameExtractor[Message2](x => Some(x.kind))
  implicit val msg2eventDataExtractor = EventDataExtractor[Message2] { msg2 =>
    Json.stringify(Json.fromJson[JsObject](msg2.data).get + ("msgid" -> JsNumber(BigDecimal(msg2.id.getOrElse(0L)))))
  }

  @throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    self ! Init
  }


  private[this] var initRetry: scala.concurrent.duration.FiniteDuration = MinInitRetry

  private[this] val tickDuration = {
    import scala.concurrent.duration._
    30 seconds
  }

  private[this] val (contestOut2, contestChannel2) = Concur.broadcast[Contest]()
  private[this] val contestStates = mutable.Map[Int, Contest]()
  // The exact deadline timer for round starts, ends and palevo's countdowns; created once the contests are loaded.
  private[this] var roundClock: Option[akka.actor.ActorRef] = None
  private[this] val (roundOut, roundChannel) = Concur.broadcast[RoundPush]()
  private[this] val (msg2Out, msg2Channel) = Concur.broadcast[Message2]()
  private[this] val unacked = mutable.Map[(Int, Int), mutable.Map[Long, Message2]]()
  private[this] val (rowsOut, rowsChan) = Concur.broadcast[SubmitRows]()
  private[this] val pendingClarificationRequests = mutable.Map[Long, mutable.Set[Long]]().withDefaultValue(mutable.Set[Long]())
  private[this] val (clr2Out, clr2Chan) = Concur.broadcast[ClarificationRequestState]()
  private[this] val (clrPostOut, clrPostChannel) = Concur.broadcast[ClarificationPosted]()

  private[this] val clarifications = {
    mutable.Map[Int, mutable.Map[Long, Clarification]]()
  }
  private val clarificationsSeen = {
    import com.github.nscala_time.time.Imports._
    mutable.Map[(Int, Int), DateTime]()
  }

  private def getUnacked(contest: Int, team: Int) =
    unacked.getOrElseUpdate((contest, team), mutable.Map[Long, Message2]())

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel

  private def pushPersistent(contest: Int, team: Int, kind: String, data: JsValue) =
    db.run(
      SlickModel.addMessage(Message2(None, contest, team, kind, data, seen = false))).foreach { m2 =>
      self ! m2
    }

  /**
    * The result of a finished testing together with the rendered row of this attempt. If the row cannot be built, or
    * the database does not show this testing yet, the result alone is sent.
    */
  private def annotateFinished(finished: FinishedTesting): Future[(AnnoSubmit, Seq[SubmitRow])] = {
    val sub = finished.submit
    Submits.teamProblemSubmits(db, sub.contest, sub.team, sub.problem).flatMap { described =>
      described.find(_.submit.submitId.id == sub.id).filter(_.submit.testingId.contains(finished.testingId)) match {
        case Some(mine) =>
          // Only this attempt's row changed: there is no per-attempt score that depends on the earlier ones.
          val rows = Seq(SubmitRow(mine.submit.submitId.id, views.html.singlesubmit(mine).body))
          if (mine.details.isEmpty && finished.compiled && !mine.result.success)
            SubmitResult.annotateFinished2(db, finished).map(_ -> rows)
          else
            Future.successful(AnnoSubmit(sub.id, sub.contest, sub.team, sub.problem, mine.result) -> rows)
        case None =>
          SubmitResult.annotateFinished2(db, finished).map(_ -> Nil)
      }
    }.recoverWith {
      case e =>
        logger.error(s"rendering rows for submit ${sub.id}: $e")
        SubmitResult.annotateFinished2(db, finished).map(_ -> Nil)
    }
  }

  private def loadAll() = {
    val f =
      db.run(
        SlickModel.messages2Unseen.result zip
        SlickModel.clarificationRequestsUnanswered.result zip
          SlickModel.clarifications.result zip
          SlickModel.clrSeen2.result zip
          SlickModel.currentRounds.result.map(_.map(SlickModel.toContest))
      )

    f.map {
      case ((((msgs, clst), clrs), seen2), contests) =>
        val clst2 = clst.groupBy(_._1).mapValues(x => x.map(_._2))
        StatusActorInitialState(msgs, clst2, ClarificationsInitialState(clrs, seen2), contests)
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
      clarifications.getOrElseUpdate(cl.contest, mutable.Map[Long, Clarification]()).put(id, cl)
    }

  private def deleteClarification(id: Long) =
    clarifications.values.flatMap { m =>
      m.remove(id)
    }.headOption

  /** Tells palevo; a failure is logged, never thrown. */
  private def toPalevo(c: Contest, event: => Option[PalevoEvent]): Unit =
    Try(event).flatMap(e => Try(e.foreach(palevo.send))).failed.foreach(t =>
      logger.warn(s"palevo: cannot announce the times of round ${c.id}: $t"))

  /**
    * A round's new row: stored, streamed as the `contest` event, and pushed as `round_rescheduled` when its times moved
    * (palevo hears contest_rescheduled).
    */
  private def applyContest(c: Contest): Unit = {
    val old = contestStates.get(c.id)
    for (o <- old) toPalevo(c, ContestTimeTracker.rescheduled(o, c, now()))
    if (old.isEmpty || old.get != c) {
      contestStates.put(c.id, c)
      contestChannel2.offer(c)
    }
    for (o <- old if rescheduled(o, c))
      roundChannel.offer(RoundPush(c.id, rescheduledEvent(c)))
  }

  private def roundEvents(contest: Int): Source[Event, NotUsed] = roundOut.filter(_.round == contest).map(_.event)

  private def contestForPalevo(contest: Int): PContest =
    contestStates.get(contest).map(EventBuilders.contestOf).getOrElse(PContest(contest, ""))

  /** Looks up the request and its team, then tells palevo. Any failure is logged and the announcement skipped. */
  private def announceClarificationRequest(contest: Int, clrId: Long): Unit = {
    val pcontest = contestForPalevo(contest)
    val f = db.run(ClarificationModel.getClarificationReq(clrId).result.headOption).flatMap {
      case Some(req) =>
        teamLookup(contest, req.team).map { team =>
          palevo.send(EventBuilders.clarificationRequested(req, pcontest,
            EventBuilders.teamOrPlaceholder(req.team, team), com.github.nscala_time.time.Imports.DateTime.now))
        }
      case None =>
        Future.successful(logger.warn(s"palevo: clarification request $clrId not found"))
    }
    f.failed.foreach(e => logger.warn(s"palevo: cannot announce clarification request $clrId: $e"))
  }

  private def announceClarificationAnswer(contest: Int, clrId: Long): Unit = {
    val pcontest = contestForPalevo(contest)
    val f = db.run(ClarificationModel.getClarificationReq(clrId).result.headOption).flatMap {
      case Some(req) =>
        teamLookup(contest, req.team).map { team =>
          palevo.send(EventBuilders.clarificationAnswered(req, pcontest,
            EventBuilders.teamOrPlaceholder(req.team, team), com.github.nscala_time.time.Imports.DateTime.now))
        }
      case None =>
        Future.successful(logger.warn(s"palevo: clarification request $clrId not found"))
    }
    f.failed.foreach(e => logger.warn(s"palevo: cannot announce answer to clarification request $clrId: $e"))
  }

  override def receive = {
    case Init =>
      Future.fromTry(Try(initialLoad.fold(loadAll())(_.apply()))).flatten.onComplete {
        case Success(sis) => self ! sis
        case scala.util.Failure(e) =>
          logger.error(s"loading status actor: $e")
          self ! InitFailed
      }

    // Retry-on-failure of the one start-up load (1 s doubling to 30 s on one timer), never a poll.
    case InitFailed =>
      timers.startSingleTimer(InitRetryKey, Init, initRetry)
      initRetry = (initRetry * 2).min(MaxInitRetry)

    case StatusActorInitialState(msgs, clrs, cls, contests) => {
      clrs.foreach {
        case (contest, pending) => {
          val pendingSet = mutable.Set[Long](pending:_*)
          pendingClarificationRequests.put(contest, pendingSet).foreach { old =>
            if (old != pendingSet) {
              clr2Chan.offer(ClarificationRequestState(contest, pending.length, newRequest = false))
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

      for (c <- contests) {
        contestStates.put(c.id, c)
      }
      roundClock = Some(context.actorOf(RoundClockActor.props(contests.toSeq, Seq(self), now,
        ContestTimeTracker.countdownOf(countdown)), "round-clock"))

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
      clr2Chan.offer(ClarificationRequestState(contest, pendingClarificationRequests(contest).size, newRequest = true))
      announceClarificationRequest(contest, clrId)
    }

    case ClarificationAnswered(contest, clrId, teamId, problem, text) => {
      pendingClarificationRequests(contest) -= clrId
      clr2Chan.offer(ClarificationRequestState(contest, pendingClarificationRequests(contest).size, newRequest = false))
      logger.info(s"pushPersistent: contest $contest clarification $clrId team $teamId problem $problem")
      pushPersistent(contest, teamId, "clarificationAnswered", Json.obj("problem" -> problem, "text" -> text))
      announceClarificationAnswer(contest, clrId)
    }

      // This handles both toggles and full edits.
    case cl: Clarification => {
      val orig = cl.id.flatMap(id => clarifications.get(cl.contest).flatMap(_.get(id)))
      val saved = sender()

      val pcontest = contestForPalevo(cl.contest)

      val q = if (orig.isDefined) {
        (SlickModel.clarifications.filter(_.id === cl.id.get).update(cl).map(_ => cl))
      } else {
        SlickModel.insertClarification(cl)
      }

      db.run(q).map { next =>
        val prevVisible = orig.exists(!_.hidden)
        val ifp = if(cl.problem.isEmpty) None else Some(cl.problem)
        insertClarification(next)
        if (!next.hidden) {
          clrPostChannel.offer(ClarificationPosted(next.id.get, next.contest, prevVisible, ifp, next.text))
          palevo.send(EventBuilders.clarificationPosted(next.id.get, pcontest, prevVisible, next.problem, next.text,
            com.github.nscala_time.time.Imports.DateTime.now))
        }
        next
      }.onComplete(Ask.respond(saved, _))
    }

    case DeleteClarification(clarificationId) => {
      val saved = sender()
      db.run(SlickModel.clarifications.filter(_.id === clarificationId).delete).map { _ =>
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
      import com.github.tototoshi.slick.PostgresJodaSupport._

      clarifications.get(contest).map { cmap =>
        cmap.values.map(_.arrived).max
      }.foreach { now =>
        clarificationsSeen.put((contest, team), now)
        db.run(SlickModel.upsertMaxSeen(MaxSeen(contest, team, now)))
      }
    }

    case finished: FinishedTesting => {

      annotateFinished(finished).foreach {
        case (annotated, rows) =>
          if (rows.nonEmpty) self ! SubmitRows(annotated.contest, annotated.team, rows)
          self ! annotated
      }

      // Fire-and-forget; the hooks never throw, the Try is a second guard so the ack below is always sent.
      Try(liveHooks.finished(finished.submit.contest, finished.submit.id, finished.compiled, finished.points))

      sender() ! {}
    }

    case Ack(loggedInTeam, msgid) => {
      getUnacked(loggedInTeam.contest.id, loggedInTeam.team.id) -= msgid
      // only the identity's own message: another participant's message id updates nothing
      val loc = SlickModel.messageSeenByID((msgid, loggedInTeam.contest.id, loggedInTeam.team.id))
      val upd = loc.update(true)
      db.run(upd)
    }

    case GetAllContests => {
      sender() ! Success(AllContests(contestStates.values.toSeq))
    }

    case GetSingleContest(id) => {
      sender() ! Success(GetSingleContestResult(contestStates.get(id)))
    }

    case rows: SubmitRows => rowsChan.offer(rows)

    case annotated: AnnoSubmit => {
      logger.debug(s"received annotated: $annotated")

      pushPersistent(annotated.contest, annotated.team, "submit", Json.toJson(annotated))
    }

    case evalDone: CustomTestResult => {
      pushPersistent(evalDone.contest, evalDone.team, "custom", Json.toJson(evalDone))
      sender ! {}
    }

    case msg2: Message2 => catchMsg(msg2)

    // Applied in place (not re-sent to self), so a ContestRemoved right behind it can never be overtaken.
    case m @ ContestChanged(c) =>
      roundClock.foreach(_ ! m)
      applyContest(c)

    case m @ ContestRemoved(id) =>
      roundClock.foreach(_ ! m)
      if (contestStates.remove(id).isDefined) logger.info(s"round $id is gone or no longer usable; dropped")

    // Every edge goes to palevo (phase and countdown events); only the start and the end reach the browsers.
    case RoundClockActor.Fired(edge, c) =>
      toPalevo(c, ContestTimeTracker.onEdge(edge, c, now()))
      roundEvent(edge, c).foreach(e => roundChannel.offer(RoundPush(c.id, e)))

    case JoinAdmin(c: Int) => {
      import ContestWrites._
      val enum = Try {
        val pings = Source.tick(tickDuration, tickDuration, userPing) // SSE keep-alive; reads nothing
        val clars = Source.apply[ClarificationRequestState](Seq(ClarificationRequestState(c, pendingClarificationRequests(c).size, newRequest = false)).toList)
            .concat(clr2Out.filter(_.contest == c)) via EventSource.flow
        Source.combine(
          contestStreamSource(c) via EventSource.flow,
          pings, clars, roundEvents(c))(Merge(_))
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

      import ContestWrites._

      val allSources = Source.combine(
        contestStreamSource(contest) via EventSource.flow,
        Source.apply(stored.toList) via EventSource.flow,
        Source.single(ClarificationState(contest, team, unseen)) via EventSource.flow,
        clrPostOut.filter(_.contest == contest) via EventSource.flow,
        msg2Out.filter(p => p.contest == contest && p.team == team) via EventSource.flow,
        rowsOut.filter(p => p.contest == contest && p.team == team) via EventSource.flow,
        roundEvents(contest),
        // SSE keep-alive comment; it reads nothing.
        Source.tick(tickDuration, tickDuration, userPing)
      )(Merge(_))

      sender ! UserJoined(allSources)
    }
  }

  @throws[Exception](classOf[Exception])
  override def postStop(): Unit = {
    logger.debug(s"Disconnected")
    super.postStop()
  }
}
