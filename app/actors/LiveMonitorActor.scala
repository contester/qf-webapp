package actors

import java.io.File

import akka.NotUsed
import akka.actor.{Actor, ActorLogging, Props, Stash, Status, Timers}
import akka.stream.scaladsl.Source
import akka.stream.{BoundedSourceQueue, Materializer, QueueOfferResult}
import live._
import models.{ACM, ProblemClient, Submits}
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{Contest, Team}
import org.stingray.qf.models.TeamClient
import palevo.{ContestTimeTracker, CountdownConfig, PalevoSink, StandingsAnnouncer}
import play.api.libs.EventSource.Event
import slick.jdbc.JdbcBackend

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object LiveMonitorActor {
  // Events (fire-and-forget, see live.LiveHooks).
  case class Arrived(contest: Int, rec: SubmitRec)
  case class Finished(contest: Int, id: Int, compiled: Boolean, passed: Int, taken: Int)
  case class RejudgeRequested(id: Int)
  /** A team's details in a contest changed (not rated, name, school, number); `team` is the whole new value. */
  case class TeamChanged(contest: Int, team: Team)
  /** A contest row was changed (times, name, ...) by the application; `contest` is the whole new row. */
  case class ContestChanged(contest: Contest)

  // Each join is answered with a Source[Event, NotUsed] whose first element is the `board` event,
  // or with Status.Failure when the contest is unknown (or, for JoinPublic, not public).
  sealed trait Join
  case class JoinPublic(contest: Int) extends Join
  case class JoinTeam(contest: Int, team: Int) extends Join
  case class JoinAdmin(contest: Int, real: Boolean) extends Join

  /**
    * The board a join would start with, as a LiveBoard (for server-side rendering of the page), answered with
    * Status.Failure like the join. A team's own row shows its real cells, with the frozen rank and score.
    */
  case class Snapshot(join: Join)

  // Timer messages; public so tests can drive them with timers switched off.
  case object Tick
  case object StaticCheck
  case object CrossCheck
  case object PollContests

  case class ContestData(problems: Seq[String], teams: Seq[Team], submits: Seq[SubmitRec])

  trait ContestLoader {
    def load(contest: Contest)(implicit ec: ExecutionContext): Future[ContestData]

    /** The contest a submit belongs to, None if there is no such submit. A failure makes callers fall back to reloading every contest. */
    def contestOf(submitId: Int)(implicit ec: ExecutionContext): Future[Option[Int]] =
      Future.failed(new UnsupportedOperationException("contestOf"))
  }

  /** Wall clock for the boards and palevo; monotonic milliseconds for the static throttle and reload rate limit. */
  trait LiveClock {
    def now(): DateTime
    def millis(): Long
  }

  object SystemLiveClock extends LiveClock {
    override def now(): DateTime = DateTime.now
    override def millis(): Long = System.nanoTime() / 1000000
  }

  case class LiveTimings(// The monitor wakes at the next deadline (see rearm); this is the longest it sleeps, a safety net for clock jumps.
                         heartbeat: FiniteDuration = 30.seconds,
                         staticCheck: FiniteDuration = 1.second,
                         // Only a safety net for contests changed in the database behind the application's back; edits made through it
                         // arrive as ContestChanged.
                         contestPoll: FiniteDuration = 5.minutes,
                         reloadGap: FiniteDuration = 5.seconds,
                         ping: FiniteDuration = 30.seconds,
                         startupRetry: FiniteDuration = 10.seconds,
                         // How long a rejudge may stay in flight (its finish not yet heard) before the database wins.
                         rejudgeMaxAge: FiniteDuration = 30.minutes,
                         connectionBuffer: Int = 1024,
                         scheduled: Boolean = true)

  /** Problems and teams from the state actors, all submits of the contest (finished or not) from the database. */
  class DbContestLoader(db: JdbcBackend#DatabaseDef, teamClient: TeamClient, problemClient: ProblemClient)
    extends ContestLoader {
    private implicit val timeout: akka.util.Timeout = akka.util.Timeout(30.seconds)

    override def load(contest: Contest)(implicit ec: ExecutionContext): Future[ContestData] =
      problemClient.getProblems(contest.id)
        .zip(teamClient.getTeams(contest.id))
        .zip(db.run(Submits.getContestSubmits(contest.id)))
        .map { case ((problems, teams), submits) =>
          ContestData(problems.map(_.id), teams.values.toSeq,
            submits.map(s => SubmitRec.fromSubmit(Submits.upliftSub(s))))
        }

    override def contestOf(submitId: Int)(implicit ec: ExecutionContext): Future[Option[Int]] =
      db.run(Submits.contestOfSubmit(submitId))
  }

  def props(db: JdbcBackend#DatabaseDef, cfg: LiveMonitorConfig, staticLocation: Option[File],
            teamClient: TeamClient, problemClient: ProblemClient, contests: () => Future[Seq[Contest]],
            renderStatic: (Contest, LiveBoard) => String, palevo: PalevoSink,
            countdown: CountdownConfig = CountdownConfig.Default): Props =
    propsWith(cfg, staticLocation, contests, new DbContestLoader(db, teamClient, problemClient), renderStatic, palevo,
      SystemLiveClock, LiveTimings(), countdown)

  def propsWith(cfg: LiveMonitorConfig, staticLocation: Option[File], contests: () => Future[Seq[Contest]],
                loader: ContestLoader, renderStatic: (Contest, LiveBoard) => String, palevo: PalevoSink,
                clock: LiveClock, timings: LiveTimings, countdown: CountdownConfig = CountdownConfig.Default): Props =
    Props(new LiveMonitorActor(cfg, staticLocation, contests, loader, renderStatic, palevo, clock, timings, countdown))

  /**
    * How long to sleep until `deadline` (the earliest moment something can happen): a margin past it so the clock has
    * reached it, at least 50 ms, and never longer than the heartbeat (the wake is re-armed from there; Akka's scheduler
    * refuses delays of about 248 days and more). The heartbeat when there is no deadline.
    */
  def wakeDelay(deadline: Option[DateTime], now: DateTime, heartbeat: FiniteDuration): FiniteDuration =
    deadline.fold(heartbeat)(d => (d.getMillis - now.getMillis + 50).max(50L).millis.min(heartbeat))

  private case object Start
  private case object WakeKey
  private case class Started(result: Try[Seq[(Contest, Try[ContestData])]])
  private case class Loaded(contest: Int, result: Try[ContestData])
  private case class Added(contest: Contest, result: Try[ContestData])
  private case class RejudgeContest(id: Int, result: Try[Option[Int]])
  private case class ContestsPolled(contests: Seq[Contest], generation: Long)
  private case class StartReload(contest: Int)
  private case class Register(contest: Int, viewer: Viewer, queue: BoundedSourceQueue[Event])
  private case class Unregister(contest: Int, queue: BoundedSourceQueue[Event])

  private sealed trait Viewer
  private case object PublicViewer extends Viewer
  private case object AdminFreezingViewer extends Viewer
  private case object AdminRealViewer extends Viewer
  private case class TeamViewer(team: Int) extends Viewer

  private sealed trait Ev { def id: Int }
  private case class EvArrived(rec: SubmitRec) extends Ev { def id: Int = rec.id }
  private case class EvFinished(id: Int, compiled: Boolean, passed: Int, taken: Int) extends Ev
  // quiet: one of many reloads looking for an id no contest knew; a miss is not worth a warning per contest.
  private case class EvRejudge(id: Int, quiet: Boolean = false) extends Ev

  private val pingEvent = Event("", None, Some("ping"))
  private val emptyBoard = LiveBoard(Nil, Vector.empty, Map.empty)

  /** What a viewer without the real board sees before the start: the teams, in board order, and no problems; a not-rated team keeps rank 0. */
  private[actors] def teamsOnly(b: LiveBoard): LiveBoard =
    LiveBoard(Nil, b.rows.map(r => r.copy(score = ACM.Score(0, 0, 0), cells = Map.empty, rank = if (r.rank == 0) 0 else 1)), Map.empty)
  private val crossCheckReason = "cross-check"
}

/**
  * Owns one LiveContest per contest, applies submit events to it and streams the boards over SSE.
  *
  * Startup loads every contest before any event is handled (Stash). Each event is applied inside Try: a failure is
  * logged and the contest is rebuilt from the database (rate-limited). The database is read only at startup, in the
  * cross-check, for an event about a submit id the contest does not know, and after a failure.
  *
  * Streams are per-connection queues registered with this actor, so the first `board` event and the later updates
  * come from the same ordered sequence and nothing is lost between the snapshot and the subscription.
  */
class LiveMonitorActor(cfg: LiveMonitorConfig,
                       staticLocation: Option[File],
                       contestRows: () => Future[Seq[Contest]],
                       loader: LiveMonitorActor.ContestLoader,
                       renderStatic: (Contest, LiveBoard) => String,
                       palevoSink: PalevoSink,
                       clock: LiveMonitorActor.LiveClock,
                       timings: LiveMonitorActor.LiveTimings,
                       countdown: CountdownConfig) extends Actor with Stash with Timers with ActorLogging {
  import LiveMonitorActor._

  private implicit val ec: ExecutionContext = context.dispatcher
  private implicit val mat: Materializer = Materializer(context)

  private class Load(val reason: String, val waiting: Vector[Ev]) {
    // Events applied to the live state while this load runs; replayed onto the rebuilt state.
    var applied: Vector[Ev] = Vector.empty
    // Team changes that came in while this load ran; its team list may predate them.
    var teamChanges: Vector[Team] = Vector.empty
  }

  private class ContestState(var live: LiveContest, val throttle: StaticThrottle) {
    var loading: Option[Load] = None
    var reloadScheduled = false
    var lastLoadStart: Option[Long] = None
    // Events about ids the contest did not know, waiting for the next load.
    var waiting: Vector[Ev] = Vector.empty
    var dirty = false
    var staticFailing = false
    // Ids still unknown after a reload, or arrivals the contest does not accept: never reload for them again.
    val ignored: mutable.Set[Int] = mutable.Set.empty
  }

  private val contests = mutable.Map.empty[Int, ContestState]
  // Counts polls started and contest changes received. A poll answers with the number it started under, and
  // is not believed about a contest that was changed after that: its read of the database may predate the change.
  private var generation = 0L
  private val changedAt = mutable.Map.empty[Int, Long]
  private val adding = mutable.Set.empty[Int]
  private val subscribers = mutable.Map.empty[Int, mutable.Map[BoundedSourceQueue[Event], Viewer]]
  // Submit id -> monotonic millis of its rejudge request, until its finish arrives or the entry expires.
  private val rejudging = mutable.Map.empty[Int, Long]
  private val standings = new StandingsAnnouncer
  // The deadline the pending wake timer was armed for, if any.
  private var armedFor: Option[DateTime] = None
  // Phase, countdown and reschedule events for palevo; when the live monitor is on, only this actor produces them.
  private val timeTracker = new ContestTimeTracker(countdown)

  override def preStart(): Unit = {
    super.preStart()
    self ! Start
  }

  override def postStop(): Unit = {
    subscribers.values.foreach(_.keys.foreach(q => Try(q.complete())))
    super.postStop()
  }

  private def safeLoad(c: Contest): Future[ContestData] =
    Future.fromTry(Try(loader.load(c))).flatten

  override def receive: Receive = {
    case Start =>
      Future.fromTry(Try(contestRows())).flatten.flatMap { cs =>
        Future.sequence(cs.map(c => safeLoad(c).transform(r => Success(c -> r))))
      }.onComplete(r => self ! Started(r))

    case Started(Success(loaded)) =>
      val now = clock.now()
      loaded.foreach {
        case (c, Success(d)) => add(c, d, now)
        case (c, Failure(e)) => log.error(e, s"live monitor: cannot load contest ${c.id}; will retry on the next contest poll")
      }
      startTimers()
      rearm(now)
      unstashAll()
      context.become(running)

    case Started(Failure(e)) =>
      log.error(e, s"live monitor: cannot list contests; retrying in ${timings.startupRetry}")
      timers.startSingleTimer(Start, Start, timings.startupRetry)

    case _ => stash()
  }

  private def startTimers(): Unit =
    if (timings.scheduled) {
      timers.startTimerWithFixedDelay(Tick, Tick, timings.heartbeat)
      timers.startTimerWithFixedDelay(StaticCheck, StaticCheck, timings.staticCheck)
      timers.startTimerWithFixedDelay(PollContests, PollContests, timings.contestPoll)
      if (cfg.crosscheckInterval > Duration.Zero)
        timers.startTimerWithFixedDelay(CrossCheck, CrossCheck, cfg.crosscheckInterval)
    }

  /**
    * Adds a freshly loaded contest; its first palevo observation only records state. A contest whose data cannot be
    * built is skipped (the others keep working) and retried on the next contest poll.
    */
  private def add(c: Contest, d: ContestData, now: DateTime): Unit =
    Try(new LiveContest(c, d.problems, d.teams, d.submits, now)) match {
      case Success(lc) =>
        val st = new ContestState(lc, new StaticThrottle(cfg.staticInterval))
        contests(c.id) = st
        observeTime(c, now)
        guarded(c.id, st) {
          observePalevo(st)
          st.throttle.markDirty(clock.millis())
        }
      case Failure(e) =>
        log.error(e, s"live monitor: cannot build contest ${c.id}; will retry on the next contest poll")
    }

  private def youngRejudge(id: Int, ms: Long): Boolean =
    rejudging.get(id).exists(t => ms - t < timings.rejudgeMaxAge.toMillis)

  private def expireRejudges(): Unit = {
    val ms = clock.millis()
    rejudging.keys.toList.foreach(id => if (!youngRejudge(id, ms)) rejudging -= id)
  }

  def running: Receive = {
    case Arrived(cid, rec) => withContest(cid, "arrival")(st => applyEvent(cid, st, EvArrived(rec), mayReload = false))

    case Finished(cid, id, compiled, passed, taken) =>
      rejudging -= id
      withContest(cid, "finish")(st => applyEvent(cid, st, EvFinished(id, compiled, passed, taken), mayReload = true))

    case RejudgeRequested(id) => rejudge(id)

    case TeamChanged(cid, team) => withContest(cid, "team change")(st => teamChanged(cid, st, team))

    case j: Join => viewerOf(j) match {
      case Some((cid, viewer)) => join(cid, viewer)
      case None => sender() ! Status.Failure(notFound(j))
    }

    case Snapshot(j) =>
      val board = viewerOf(j).flatMap { case (cid, viewer) =>
        contests.get(cid).map(st => Try(snapshotBoard(st, viewer)))
      }
      sender() ! (board match {
        case Some(Success(b)) => b
        case Some(Failure(e)) => Status.Failure(e)
        case None => Status.Failure(notFound(j))
      })

    case Register(cid, viewer, q) => register(cid, viewer, q)
    case Unregister(cid, q) => unregister(cid, q)

    // The check against the clock: on the deadline timer, on the heartbeat, and from tests. Idempotent.
    case Tick =>
      expireRejudges()
      val now = clock.now()
      contests.foreach { case (cid, st) =>
        observeTime(st.live.contest, now)
        Try(st.live.onTick(now)) match {
          case Success(Some(d)) => changed(cid, st, d)
          case Success(None) => ()
          case Failure(e) => failed(cid, st, e)
        }
      }
      armedFor = None // the wake that brought us here, or the heartbeat: the next one is chosen afresh
      rearm(now)

    case StaticCheck => writeStatic()

    case CrossCheck =>
      if (cfg.crosscheckInterval > Duration.Zero)
        contests.foreach { case (cid, st) => if (st.loading.isEmpty) startLoad(cid, st, crossCheckReason) }

    case StartReload(cid) =>
      contests.get(cid).foreach { st =>
        st.reloadScheduled = false
        if (st.loading.isEmpty) startLoad(cid, st, "reload")
      }

    case Loaded(cid, result) => contests.get(cid).foreach(st => loaded(cid, st, result))

    case PollContests =>
      generation += 1
      val started = generation
      Future.fromTry(Try(contestRows())).flatten.onComplete {
        case Success(cs) => self ! ContestsPolled(cs, started)
        case Failure(e) => log.warning(s"live monitor: cannot poll contests: $e")
      }

    case RejudgeContest(id, result) => rejudgeContest(id, result)

    case ContestsPolled(cs, started) =>
      cs.filterNot(c => changedAt.get(c.id).exists(_ > started)).foreach(contestRow)
      rearm(clock.now())

    case ContestChanged(c) =>
      generation += 1
      changedAt.put(c.id, generation)
      contestRow(c)
      rearm(clock.now())

    case Added(c, result) =>
      adding -= c.id
      result match {
        case Success(d) =>
          if (!contests.contains(c.id)) add(c, d, clock.now())
          rearm(clock.now())
        case Failure(e) => log.warning(s"live monitor: cannot load new contest ${c.id}: $e")
      }
  }

  private def notFound(cid: Int) = new NoSuchElementException(s"live monitor: no contest $cid")

  private def contestOf(j: Join): Int = j match {
    case JoinPublic(c) => c
    case JoinTeam(c, _) => c
    case JoinAdmin(c, _) => c
  }

  private def notFound(j: Join): NoSuchElementException = notFound(contestOf(j))

  /** The contest and viewer of a join; None when it is not allowed (a contest that is not public). */
  private def viewerOf(j: Join): Option[(Int, Viewer)] = j match {
    case JoinPublic(cid) => if (cfg.publicContests(cid)) Some(cid -> PublicViewer) else None
    case JoinTeam(cid, team) => Some(cid -> TeamViewer(team))
    case JoinAdmin(cid, real) => Some(cid -> (if (real) AdminRealViewer else AdminFreezingViewer))
  }

  private def withContest(cid: Int, what: String)(f: ContestState => Unit): Unit =
    contests.get(cid) match {
      case Some(st) => f(st)
      case None => log.warning(s"live monitor: $what for unknown contest $cid ignored")
    }

  /** Runs f; on an exception logs it and rebuilds the contest from the database. */
  private def guarded(cid: Int, st: ContestState)(f: => Unit): Unit =
    Try(f).failed.foreach(e => failed(cid, st, e))

  private def failed(cid: Int, st: ContestState, e: Throwable): Unit = {
    log.error(e, s"live monitor: contest $cid: event failed, rebuilding from the database")
    st.dirty = true
    requestReload(cid, st)
  }

  private def applyTo(lc: LiveContest, ev: Ev, now: DateTime): Option[Delta] = ev match {
    case EvArrived(rec) => Some(lc.onArrived(rec, now))
    case EvFinished(id, compiled, passed, taken) => lc.onFinished(id, compiled, passed, taken, now)
    case EvRejudge(id, _) => lc.onRejudge(id, now)
  }

  private def accepts(lc: LiveContest, rec: SubmitRec): Boolean =
    lc.board(Real).problems.contains(rec.problem) && lc.board(Real).rowOf(rec.team).isDefined

  private def applyEvent(cid: Int, st: ContestState, ev: Ev, mayReload: Boolean): Unit = {
    val now = clock.now()
    ev match {
      case EvArrived(rec) if !accepts(st.live, rec) => st.ignored += rec.id
      case _ =>
    }
    Try(applyTo(st.live, ev, now)) match {
      case Success(Some(d)) =>
        st.loading.foreach(l => l.applied :+= ev)
        ev match {
          case EvRejudge(id, _) => rejudging(id) = clock.millis()
          case _ =>
        }
        changed(cid, st, d)
      case Success(None) =>
        if (st.ignored(ev.id)) log.debug(s"live monitor: contest $cid: ignoring submit ${ev.id}")
        else if (mayReload) {
          st.waiting :+= ev
          requestReload(cid, st)
        } else {
          ev match {
            case EvRejudge(_, true) => ()
            case _ => log.warning(s"live monitor: contest $cid: unknown submit ${ev.id} after reload, ignored")
          }
          st.ignored += ev.id
        }
      case Failure(e) => failed(cid, st, e)
    }
  }

  private def teamChanged(cid: Int, st: ContestState, team: Team): Unit = {
    st.loading.foreach(l => l.teamChanges :+= team)
    Try(st.live.onTeamChanged(team, clock.now())) match {
      case Success(d) => changed(cid, st, d)
      case Failure(e) => failed(cid, st, e)
    }
  }

  private def rejudge(id: Int): Unit = {
    val now = clock.now()
    val found = contests.iterator.map { case (cid, st) => (cid, st, Try(st.live.onRejudge(id, now))) }
      .find { case (_, _, r) => r != Success(None) }
    found match {
      case Some((cid, st, Success(Some(d)))) =>
        st.loading.foreach(l => l.applied :+= EvRejudge(id))
        rejudging(id) = clock.millis()
        changed(cid, st, d)
      case Some((cid, st, Failure(e))) => failed(cid, st, e)
      case _ =>
        if (contests.values.forall(_.ignored(id))) log.debug(s"live monitor: ignoring rejudge of submit $id")
        else {
          // No contest knows it: ask the database which contest owns it and reload only that one.
          Future.fromTry(Try(loader.contestOf(id))).flatten.onComplete(r => self ! RejudgeContest(id, r))
        }
    }
  }

  private def rejudgeContest(id: Int, result: Try[Option[Int]]): Unit = result match {
    case Success(Some(cid)) =>
      contests.get(cid) match {
        case Some(st) =>
          st.waiting :+= EvRejudge(id)
          requestReload(cid, st)
        case None => log.warning(s"live monitor: rejudge of submit $id, which belongs to unloaded contest $cid; ignored")
      }
    case Success(None) =>
      log.warning(s"live monitor: rejudge of submit $id, which is not in the database; ignored")
      contests.values.foreach(_.ignored += id)
    case Failure(e) =>
      log.warning(s"live monitor: cannot find the contest of submit $id ($e); reloading every contest")
      contests.foreach { case (cid, st) =>
        if (!st.ignored(id)) {
          st.waiting :+= EvRejudge(id, quiet = true)
          requestReload(cid, st)
        }
      }
  }

  private def requestReload(cid: Int, st: ContestState): Unit =
    if (st.loading.isEmpty && !st.reloadScheduled) {
      val wait = st.lastLoadStart.map(_ + timings.reloadGap.toMillis - clock.millis()).getOrElse(0L)
      if (wait <= 0) startLoad(cid, st, "reload")
      else {
        st.reloadScheduled = true
        timers.startSingleTimer(StartReload(cid), StartReload(cid), wait.millis)
      }
    }

  private def startLoad(cid: Int, st: ContestState, reason: String): Unit = {
    val l = new Load(reason, st.waiting)
    st.waiting = Vector.empty
    st.loading = Some(l)
    st.lastLoadStart = Some(clock.millis())
    safeLoad(st.live.contest).onComplete(r => self ! Loaded(cid, r))
  }

  private def loaded(cid: Int, st: ContestState, result: Try[ContestData]): Unit = st.loading.foreach { l =>
    st.loading = None
    result match {
      case Failure(e) =>
        log.warning(s"live monitor: contest $cid: ${l.reason} load failed: $e")
        retryLater(cid, st, l)
      case Success(data) =>
        if (rebuild(cid, st, l, data)) {
          // Unknown ids that this load was for: apply once, never reload for them again.
          l.waiting.foreach(ev => applyEvent(cid, st, ev, mayReload = false))
          // Unknown ids that came in while it ran: the new state may know them; otherwise reload again.
          val again = st.waiting
          st.waiting = Vector.empty
          again.foreach(ev => applyEvent(cid, st, ev, mayReload = true))
        } else retryLater(cid, st, l)
    }
  }

  /**
    * The load did not produce a new state: keep its events for the next load and, when something is waiting for it
    * (queued unknown ids or a dirty contest), ask for a reload (rate-limited). A plain cross-check that failed just
    * waits for the next cross-check.
    */
  private def retryLater(cid: Int, st: ContestState, l: Load): Unit = {
    st.waiting = l.waiting ++ st.waiting
    if (st.dirty || st.waiting.nonEmpty) requestReload(cid, st)
  }

  /** Builds the state from the database data and replaces the live one; false when it could not be built. */
  private def rebuild(cid: Int, st: ContestState, l: Load, data: ContestData): Boolean = {
    val now = clock.now()
    // While a rejudge runs the database still holds the old result (`tested` stays true), so a recent in-flight
    // rejudge keeps its pending state: it is neither reported as a mismatch nor reverted. An entry older than
    // rejudgeMaxAge is taken to have lost its finish: it is dropped and the database state wins.
    expireRejudges()
    val ms = clock.millis()
    val subs = data.submits.map(s => if (youngRejudge(s.id, ms)) s.copy(state = live.Pending) else s)
    val result = Try {
      val fresh = st.live.rebuilt(data.problems, data.teams, subs, now)
      l.applied.foreach { ev =>
        Try(applyTo(fresh, ev, now)).failed.foreach(e => log.warning(s"live monitor: contest $cid: replay of $ev failed: $e"))
      }
      l.teamChanges.foreach(t => Try(fresh.onTeamChanged(t, now)).failed.foreach(e =>
        log.warning(s"live monitor: contest $cid: replay of the change of team ${t.id} failed: $e")))
      fresh
    }
    result match {
      case Failure(e) =>
        log.error(e, s"live monitor: contest $cid: rebuild failed")
        st.dirty = true
        false
      case Success(fresh) =>
        val old = st.live
        val diffs = old.differences(fresh)
        if (diffs.nonEmpty) {
          val text = s"live monitor: ${l.reason} found differences in contest $cid: ${diffs.take(20).mkString("; ")}"
          if (l.reason == crossCheckReason) log.warning(text) else log.info(text)
        }
        val wasDirty = st.dirty
        st.live = fresh
        st.dirty = false
        if (diffs.nonEmpty || wasDirty)
          changed(cid, st, Delta(Seq(WholeOut(fresh.board(Freezing))), Seq(WholeOut(fresh.board(Real))), Nil),
            freezingChanged = old.board(Freezing) != fresh.board(Freezing))
        true
    }
  }

  private def contestRow(c: Contest): Unit = contests.get(c.id) match {
    case Some(st) =>
      if (st.live.contest != c) {
        val before = st.live.board(Freezing)
        val wasStarted = started(st.live, clock.now())
        Try(st.live.onContest(c, clock.now())) match {
          case Success(d) =>
            observeTime(c, clock.now())
            changed(c.id, st, d, freezingChanged = before != st.live.board(Freezing) || wasStarted != started(st.live, clock.now()))
          case Failure(e) => failed(c.id, st, e)
        }
      }
    case None =>
      if (!adding(c.id)) {
        adding += c.id
        safeLoad(c).onComplete(r => self ! Added(c, r))
      }
  }

  /** After a change: stream it, mark the static file and feed palevo. */
  private def changed(cid: Int, st: ContestState, d: Delta, freezingChanged: Boolean = true): Unit =
    guarded(cid, st) {
      publish(cid, st, d)
      if (d.freezing.nonEmpty && freezingChanged) st.throttle.markDirty(clock.millis())
      if (d.freezing.nonEmpty || d.real.nonEmpty) observePalevo(st)
    }

  /**
    * Sleeps until the earliest moment something about the contests' times can happen: a phase, a board boundary, a
    * countdown threshold after `after`. A reschedule or a new contest moves the deadlines, hence the calls after those.
    *
    * A pending wake that is for an earlier or the same deadline is left alone: replacing a timer drops its message
    * even when it is already due, and a stream of re-arms would postpone it for ever. At worst the pending wake is
    * for a deadline that no longer exists; it fires, finds nothing, and the next check arms the right one.
    */
  private def rearm(after: DateTime): Unit =
    if (timings.scheduled) {
      val next = contests.values.flatMap(st => ContestTimeTracker.nextDeadline(st.live.contest, after, countdown))
        .reduceOption((a, b) => if (a.isBefore(b)) a else b)
      val pendingIsEnough = armedFor.exists(a => next.forall(n => !a.isAfter(n)))
      if (!pendingIsEnough) {
        timers.startSingleTimer(WakeKey, Tick, wakeDelay(next, clock.now(), timings.heartbeat))
        armedFor = next
      }
    }

  /** Tells palevo what the contest's times did: a phase reached, a countdown threshold, a reschedule. Never throws. */
  private def observeTime(c: Contest, now: DateTime): Unit =
    Try(timeTracker.observe(c, now)) match {
      case Success(events) =>
        events.foreach(e => Try(palevoSink.send(e)).failed.foreach(t =>
          log.warning(s"live monitor: contest ${c.id}: cannot send ${e.eventType}: $t")))
      case Failure(t) => log.warning(s"live monitor: contest ${c.id}: cannot track its times: $t")
    }

  private def observePalevo(st: ContestState): Unit = {
    val lc = st.live
    standings.observe(lc.contest, PalevoView.of(lc.board(Freezing)), PalevoView.of(lc.board(Real)), clock.now())
      .foreach(palevoSink.send)
  }

  private def writeStatic(): Unit = for (dir <- staticLocation; (cid, st) <- contests if cid < 11) {
    val ms = clock.millis()
    if (st.throttle.due(ms))
      Try(StaticFiles.write(dir, cid, renderStatic(st.live.contest, visibleBoard(st.live, clock.now())))) match {
        case Success(_) =>
          st.throttle.written(ms)
          st.staticFailing = false
        case Failure(e) =>
          // Stays dirty and is retried on the next check; log once per failure streak.
          if (!st.staticFailing) log.error(e, s"live monitor: cannot write static file for contest $cid")
          st.staticFailing = true
      }
  }

  // ---- streams ----

  private def started(lc: LiveContest, now: DateTime): Boolean = !now.isBefore(lc.contest.startTime)

  private def freezeActive(lc: LiveContest, now: DateTime): Boolean = {
    val r = lc.rules
    r.freezeTime.isBefore(r.endTime) && !now.isBefore(r.freezeTime) && now.isBefore(r.exposeTime)
  }

  private def realCells(lc: LiveContest, team: Int): Map[String, LiveCell] =
    lc.board(Real).rowOf(team).map(_.cells).getOrElse(Map.empty)

  private def boardEvent(b: LiveBoard, own: Option[(Int, Map[String, LiveCell])] = None): Event =
    LiveJson.event("board", LiveJson.board(b, own))

  private def plainEvent(out: BoardOut): Event = out match {
    case WholeOut(b) => boardEvent(b)
    case RowOut(c) => LiveJson.event("row", LiveJson.row(c))
  }

  private def join(cid: Int, viewer: Viewer): Unit =
    if (!contests.contains(cid)) sender() ! Status.Failure(notFound(cid))
    else {
      val me = self
      val queueEc = ec
      val src: Source[Event, NotUsed] = Source.queue[Event](timings.connectionBuffer)
        .merge(Source.tick(timings.ping, timings.ping, pingEvent), eagerComplete = true)
        .watchTermination() { (q, done) =>
          me ! Register(cid, viewer, q)
          done.onComplete(_ => me ! Unregister(cid, q))(queueEc)
          NotUsed
        }
      sender() ! src
    }

  /** The board a viewer starts with, and the real cells of its own row to show instead of the frozen ones. */
  /** The freezing board once the contest has started; the bare team list before. */
  private def visibleBoard(lc: LiveContest, now: DateTime): LiveBoard =
    if (started(lc, now)) lc.board(Freezing) else teamsOnly(lc.board(Freezing))

  private def snapshotParts(st: ContestState, viewer: Viewer): (LiveBoard, Option[(Int, Map[String, LiveCell])]) = {
    val lc = st.live
    val now = clock.now()
    viewer match {
      case AdminRealViewer => (lc.board(Real), None)
      case AdminFreezingViewer => (visibleBoard(lc, now), None)
      case PublicViewer => (visibleBoard(lc, now), None)
      case TeamViewer(t) =>
        if (!started(lc, now)) (teamsOnly(lc.board(Freezing)), None)
        else (lc.board(Freezing), if (freezeActive(lc, now)) Some(t -> realCells(lc, t)) else None)
    }
  }

  private def snapshot(st: ContestState, viewer: Viewer): Event = {
    val (b, own) = snapshotParts(st, viewer)
    boardEvent(b, own)
  }

  private def snapshotBoard(st: ContestState, viewer: Viewer): LiveBoard = {
    val (b, own) = snapshotParts(st, viewer)
    own.fold(b) { case (team, cells) =>
      b.copy(rows = b.rows.map(r => if (r.team.id == team) r.copy(cells = cells) else r))
    }
  }

  private def register(cid: Int, viewer: Viewer, q: BoundedSourceQueue[Event]): Unit =
    contests.get(cid) match {
      case Some(st) =>
        val subs = subscribers.getOrElseUpdate(cid, mutable.Map.empty)
        subs(q) = viewer
        offer(cid, q, Try(snapshot(st, viewer)).getOrElse(boardEvent(emptyBoard)))
      case None => q.complete()
    }

  private def unregister(cid: Int, q: BoundedSourceQueue[Event]): Unit =
    subscribers.get(cid).foreach { subs =>
      subs -= q
      if (subs.isEmpty) subscribers -= cid
    }

  /** A connection that cannot keep up is closed; its browser reconnects and starts from a whole board. */
  private def offer(cid: Int, q: BoundedSourceQueue[Event], e: Event): Unit =
    q.offer(e) match {
      case QueueOfferResult.Enqueued => ()
      case QueueOfferResult.Dropped =>
        log.warning(s"live monitor: contest $cid: slow connection closed")
        q.complete()
        unregister(cid, q)
      case _ => unregister(cid, q)
    }

  private def publish(cid: Int, st: ContestState, d: Delta): Unit =
    subscribers.get(cid).filter(_.nonEmpty).foreach { subs =>
      val lc = st.live
      val now = clock.now()
      val isStarted = started(lc, now)
      val overlay = freezeActive(lc, now)
      lazy val plain = d.freezing.map(plainEvent)
      lazy val gated = if (isStarted) plain else d.freezing.collect { case WholeOut(b) => boardEvent(teamsOnly(b)) }
      lazy val real = d.real.map(plainEvent)

      def teamEvents(t: Int): Seq[Event] =
        if (!isStarted || !overlay) gated
        else {
          val own = realCells(lc, t)
          d.freezing.zip(plain).map {
            case (WholeOut(b), _) => boardEvent(b, Some(t -> own))
            case (RowOut(c), _) if c.team == t => LiveJson.event("row", LiveJson.row(c, Some(own)))
            case (_, e) => e
          } ++ d.ownCells.collect { case (`t`, cells) => LiveJson.event("cells", LiveJson.cells(t, cells)) }
        }

      subs.toList.foreach { case (q, viewer) =>
        val events = viewer match {
          case AdminRealViewer => real
          case AdminFreezingViewer => gated
          case PublicViewer => gated
          case TeamViewer(t) => teamEvents(t)
        }
        events.foreach(offer(cid, q, _))
      }
    }

}
