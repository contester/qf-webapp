package actors

import akka.NotUsed
import akka.actor.{Actor, ActorLogging, Props, Stash, Status, Timers}
import akka.stream.scaladsl.Source
import akka.stream.{BoundedSourceQueue, Materializer, QueueOfferResult}
import live._
import models.{ProblemColumn, Rounds, Submits}
import org.stingray.qf.actors.ProblemStateActor
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{Contest, Team}
import org.stingray.qf.models.TeamClient
import palevo.{PalevoSink, StandingsAnnouncer}
import play.api.libs.EventSource.Event
import play.api.libs.json.Json
import slick.jdbc.JdbcBackend

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object LiveMonitorActor {
  // Events (fire-and-forget, see live.LiveHooks).
  case class Arrived(contest: Int, rec: SubmitRec)
  /** A testing finished; `points` are the submit's points now (they replace the old ones). */
  case class Finished(contest: Int, id: Int, compiled: Boolean, points: Option[Double])
  case class RejudgeRequested(id: Int)
  /**
    * The round's participants changed (credential_changed: is_active; participant_changed: unrated, name, school;
    * round_changed). The team list is read again and each changed, new or vanished row is applied to the board.
    */
  case class TeamsChanged(contest: Int)
  /** A contest row changed (round_changed: times, name, ...); `contest` is the whole new row. A new id is loaded. */
  case class ContestChanged(contest: Contest)
  /** A round vanished or became unusable: dropped, and its streams end. */
  case class ContestRemoved(contest: Int)
  /**
    * Reload every contest held (the notification listener reconnected: anything of any round may have changed while
    * it was down). A contest that is loading reloads once more after the load, like a burst of round changes.
    */
  case object ReloadAll

  // Each join is answered with a Source[Event, NotUsed] whose first element is the `board` event (`ready` for the
  // status rows), or with Status.Failure when the contest is unknown. There are only admin joins: nobody else gets a
  // live board (contestants and the public see the board once the round is over, rendered from the database).
  sealed trait Join
  case class JoinAdmin(contest: Int) extends Join
  /**
    * The rows of the admin status page: a `ready` event first, then a `statusRow` event for every submit that arrives,
    * finishes or is rejudged. Every admin sees every submit.
    */
  case class JoinAdminStatus(contest: Int) extends Join

  /**
    * The board a join would start with, as a LiveBoard (for server-side rendering of the page), answered with
    * Status.Failure like the join.
    */
  case class Snapshot(join: Join)

  case class ContestData(problems: Seq[ProblemColumn], teams: Seq[Team], submits: Seq[SubmitRec])

  trait ContestLoader {
    def load(contest: Contest)(implicit ec: ExecutionContext): Future[ContestData]

    /** The round's participants as now stored. A failure makes the actor reload the whole contest instead. */
    def teams(contest: Int)(implicit ec: ExecutionContext): Future[Seq[Team]] =
      Future.failed(new UnsupportedOperationException("teams"))

    /** The contest a submit belongs to, None if there is no such submit. A failure makes callers fall back to reloading every contest. */
    def contestOf(submitId: Int)(implicit ec: ExecutionContext): Future[Option[Int]] =
      Future.failed(new UnsupportedOperationException("contestOf"))
  }

  /** A submit rendered for the status page: the row for an admin who can rejudge (`rw`) and for one who cannot (`ro`). */
  case class StatusRow(contest: Int, id: Int, team: Int, rw: String, ro: String)

  /** Renders a status page row from the database; None when there is no such submit. */
  trait StatusRows {
    /**
      * @param inTesting what the row says while it is being (re)tested: the database still holds the old verdict
      *                  of a rejudge, and has none yet for a fresh submit; false after a finish.
      */
    def render(id: Int, inTesting: Option[models.SubmitResult])(implicit ec: ExecutionContext): Future[Option[StatusRow]]
  }

  object NoStatusRows extends StatusRows {
    override def render(id: Int, inTesting: Option[models.SubmitResult])(implicit ec: ExecutionContext): Future[Option[StatusRow]] =
      Future.successful(None)
  }

  /**
    * Status page rows from the database. A finish can be heard before the tester's result is visible; the row is then
    * read again a few times, so that a finished submit is never shown as running.
    */
  class DbStatusRows(read: Int => Future[Option[models.AdminSubmitRow]], teamClient: TeamClient, scheduler: akka.actor.Scheduler,
                     retryAfter: FiniteDuration = 500.millis, retries: Int = 4,
                     messages: play.api.i18n.Messages = models.SubmitResult.fallbackMessages) extends StatusRows {
    private implicit val timeout: akka.util.Timeout = akka.util.Timeout(5.seconds)

    override def render(id: Int, inTesting: Option[models.SubmitResult])(implicit ec: ExecutionContext): Future[Option[StatusRow]] = {
      def attempt(left: Int): Future[Option[models.AdminSubmitRow]] = read(id).flatMap {
        case Some(r) if inTesting.isEmpty && !r.submit.finished && left > 0 =>
          akka.pattern.after(retryAfter, scheduler)(attempt(left - 1))
        case other => Future.successful(other)
      }

      attempt(retries).flatMap {
        case None => Future.successful(None)
        case Some(found) =>
          val r = inTesting.fold(found)(res => found.copy(result = res))
          val cid = r.submit.submitId.contestId
          teamClient.getTeams(cid).map { teams =>
            val body = (rejudge: Boolean) => views.html.admin.submitrow(r, teams, rejudge, linkTeam = true)(messages).body.trim
            Some(StatusRow(cid, r.submit.submitId.id, r.submit.submitId.teamId,
              body(true), body(false)))
          }
      }
    }
  }

  object DbStatusRows {
    def fromDb(db: JdbcBackend#DatabaseDef, teamClient: TeamClient, scheduler: akka.actor.Scheduler,
               messages: play.api.i18n.Messages = models.SubmitResult.fallbackMessages)(implicit ec: ExecutionContext): DbStatusRows =
      new DbStatusRows(id => models.AdminSubmits.row(db, id), teamClient, scheduler, messages = messages)
  }

  /** Wall clock for the boards and palevo; monotonic milliseconds for the reload rate limit and rejudge ages. */
  trait LiveClock {
    def now(): DateTime
    def millis(): Long
  }

  object SystemLiveClock extends LiveClock {
    override def now(): DateTime = DateTime.now
    override def millis(): Long = System.nanoTime() / 1000000
  }

  case class LiveTimings(// The shortest gap between two reloads of one contest after failures (a retry backoff, not a poll).
                         reloadGap: FiniteDuration = 5.seconds,
                         // SSE keep-alive comments.
                         ping: FiniteDuration = 30.seconds,
                         // Retry after the start-up listing of the contests failed.
                         startupRetry: FiniteDuration = 10.seconds,
                         // How long a rejudge may stay in flight (its finish not yet heard) before the database wins.
                         rejudgeMaxAge: FiniteDuration = 30.minutes,
                         connectionBuffer: Int = 1024)

  /**
    * Problems, participants and all submits of the contest (finished or not), straight from the database: a
    * round_changed or credential_changed reaches the problem and team state actors at the same time as this monitor,
    * and their own reloads may not have finished yet.
    */
  class DbContestLoader(db: JdbcBackend#DatabaseDef) extends ContestLoader {
    override def load(contest: Contest)(implicit ec: ExecutionContext): Future[ContestData] =
      ProblemStateActor.loadRound(db, contest.id)
        .zip(teams(contest.id))
        .zip(db.run(Submits.getContestSubmits(contest.id)))
        .map { case ((problems, teams), submits) =>
          ContestData(problems.map(p => ProblemColumn(p.id, p.name, p.maxPoints)), teams,
            submits.map(s => SubmitRec.fromSubmit(Submits.upliftSub(s))))
        }

    override def teams(contest: Int)(implicit ec: ExecutionContext): Future[Seq[Team]] =
      Rounds.teamsOfRound(db, contest)

    override def contestOf(submitId: Int)(implicit ec: ExecutionContext): Future[Option[Int]] =
      db.run(Submits.contestOfSubmit(submitId))
  }

  def props(db: JdbcBackend#DatabaseDef, contests: () => Future[Seq[Contest]], palevo: PalevoSink,
            statusRows: StatusRows = NoStatusRows): Props =
    propsWith(contests, new DbContestLoader(db), palevo, SystemLiveClock, LiveTimings(), statusRows)

  def propsWith(contests: () => Future[Seq[Contest]], loader: ContestLoader, palevo: PalevoSink,
                clock: LiveClock, timings: LiveTimings, statusRows: StatusRows = NoStatusRows): Props =
    Props(new LiveMonitorActor(contests, loader, palevo, clock, timings, statusRows))

  private case object Start
  private case class Started(result: Try[Seq[(Contest, Try[ContestData])]])
  private case class Loaded(contest: Int, result: Try[ContestData])
  private case class Added(contest: Contest, result: Try[ContestData])
  private case class RejudgeContest(id: Int, result: Try[Option[Int]])
  private case class TeamsLoaded(contest: Int, result: Try[Seq[Team]])
  private case class StartReload(contest: Int)
  private case class Register(contest: Int, viewer: Viewer, queue: BoundedSourceQueue[Event])
  private case class Unregister(contest: Int, queue: BoundedSourceQueue[Event])
  private case class StatusRowReady(seq: Long, id: Int, result: Try[Option[StatusRow]])

  private sealed trait Viewer
  private case object BoardViewer extends Viewer
  // Not a board: the rows of the status page.
  private case object StatusViewer extends Viewer

  private sealed trait Ev { def id: Int }
  private case class EvArrived(rec: SubmitRec) extends Ev { def id: Int = rec.id }
  private case class EvFinished(id: Int, compiled: Boolean, points: Option[Double]) extends Ev
  // quiet: one of many reloads looking for an id no contest knew; a miss is not worth a warning per contest.
  private case class EvRejudge(id: Int, quiet: Boolean = false) extends Ev

  private val pingEvent = Event("", None, Some("ping"))
  private val roundChangedReason = "round changed"
}

/**
  * Owns one LiveContest per contest, applies submit events to it and streams the points board to admins over SSE.
  * Nothing else gets a live board, and nothing is written to files.
  *
  * Startup loads every contest before any event is handled (Stash). Each event is applied inside Try: a failure is
  * logged and the contest is rebuilt from the database (rate-limited). The database is read only at startup, for a
  * new round (ContestChanged), for the team list on TeamsChanged, for an event about a submit id the contest does not
  * know, and after a failure. Nothing runs on an interval and the actor has no deadline timer of its own: the end of
  * a round reaches it as the round clock's `RoundClockActor.Fired` on the event stream. Its only timers are the reload
  * backoff and the start-up retry.
  *
  * Streams are per-connection queues registered with this actor, so the first `board` event and the later updates
  * come from the same ordered sequence and nothing is lost between the snapshot and the subscription.
  */
class LiveMonitorActor(contestRows: () => Future[Seq[Contest]],
                       loader: LiveMonitorActor.ContestLoader,
                       palevoSink: PalevoSink,
                       clock: LiveMonitorActor.LiveClock,
                       timings: LiveMonitorActor.LiveTimings,
                       statusRows: LiveMonitorActor.StatusRows) extends Actor with Stash with Timers with ActorLogging {
  import LiveMonitorActor._

  private implicit val ec: ExecutionContext = context.dispatcher
  private implicit val mat: Materializer = Materializer(context)

  private class Load(val reason: String, val waiting: Vector[Ev]) {
    // Events applied to the live state while this load runs; replayed onto the rebuilt state.
    var applied: Vector[Ev] = Vector.empty
    // A team list read while this load ran; the load's own list may predate it.
    var teams: Option[Seq[Team]] = None
  }

  private class ContestState(var live: LiveContest) {
    var loading: Option[Load] = None
    var reloadScheduled = false
    // A round_changed with an unchanged row (its problems changed) came in while a load ran: load once more after it.
    var reloadAgain = false
    var lastLoadStart: Option[Long] = None
    // Events about ids the contest did not know, waiting for the next load.
    var waiting: Vector[Ev] = Vector.empty
    var dirty = false
    // A team list read is running; another TeamsChanged came in meanwhile (read once more after it).
    var teamsLoading = false
    var teamsAgain = false
    // Ids still unknown after a reload, or arrivals the contest does not accept: never reload for them again.
    val ignored: mutable.Set[Int] = mutable.Set.empty
  }

  private val contests = mutable.Map.empty[Int, ContestState]
  private val adding = mutable.Set.empty[Int]
  private val subscribers = mutable.Map.empty[Int, mutable.Map[BoundedSourceQueue[Event], Viewer]]
  // Submit id -> monotonic millis of its rejudge request, until its finish arrives or the entry expires.
  private val rejudging = mutable.Map.empty[Int, Long]
  // Submit id -> the newest status row requested for it. A row read for an older event can come back after a newer
  // one (a quick finish after the arrival) and must not replace it.
  private val statusRequests = mutable.Map.empty[Int, Long]
  private var statusSeq = 0L
  private val standings = new StandingsAnnouncer

  override def preStart(): Unit = {
    super.preStart()
    // The round clock's edges (StatusActor's RoundClockActor): the end of a round makes its standings public.
    context.system.eventStream.subscribe(self, classOf[RoundClockActor.Fired])
    self ! Start
  }

  override def postStop(): Unit = {
    context.system.eventStream.unsubscribe(self)
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
        case (c, Failure(e)) => log.error(e, s"live monitor: cannot load contest ${c.id}; will retry on the next change of its round")
      }
      unstashAll()
      context.become(running)

    case Started(Failure(e)) =>
      // Retry-on-failure of the one start-up listing, not a poll.
      log.error(e, s"live monitor: cannot list contests; retrying in ${timings.startupRetry}")
      timers.startSingleTimer(Start, Start, timings.startupRetry)

    case _ => stash()
  }

  /**
    * Adds a freshly loaded contest; its first palevo observation only records state. A contest whose data cannot be
    * built is skipped (the others keep working) and retried on the next ContestChanged of its round.
    */
  private def add(c: Contest, d: ContestData, now: DateTime): Unit =
    Try(new LiveContest(c, d.problems, d.teams, d.submits)) match {
      case Success(lc) =>
        val st = new ContestState(lc)
        contests(c.id) = st
        guarded(c.id, st)(observePalevo(st))
      case Failure(e) =>
        log.error(e, s"live monitor: cannot build contest ${c.id}; will retry on the next change of its round")
    }

  private def youngRejudge(id: Int, ms: Long): Boolean =
    rejudging.get(id).exists(t => ms - t < timings.rejudgeMaxAge.toMillis)

  private def expireRejudges(): Unit = {
    val ms = clock.millis()
    rejudging.keys.toList.foreach(id => if (!youngRejudge(id, ms)) rejudging -= id)
  }

  def running: Receive = {
    case Arrived(cid, rec) =>
      withContest(cid, "arrival")(st => applyEvent(cid, st, EvArrived(rec), mayReload = false))
      requestStatusRow(cid, rec.id, Some(models.SubmitWaiting))

    case Finished(cid, id, compiled, points) =>
      rejudging -= id
      withContest(cid, "finish")(st => applyEvent(cid, st, EvFinished(id, compiled, points), mayReload = true))
      requestStatusRow(cid, id, None)

    case RejudgeRequested(id) =>
      rejudge(id)
      requestStatusRow(-1, id, Some(models.BeingRetestedResult))

    case StatusRowReady(seq, id, result) => statusRowReady(seq, id, result)

    // A round the monitor does not hold gets its teams when it is loaded.
    case TeamsChanged(cid) => contests.get(cid).foreach(st => readTeams(cid, st))

    case TeamsLoaded(cid, result) => contests.get(cid).foreach(st => teamsLoaded(cid, st, result))

    case j: Join => viewerOf(j) match {
      case Some((cid, viewer)) => join(cid, viewer)
      case None => sender() ! Status.Failure(notFound(j))
    }

    case Snapshot(j) =>
      val board = contests.get(contestOf(j)).map(st => Try(st.live.board))
      sender() ! (board match {
        case Some(Success(b)) => b
        case Some(Failure(e)) => Status.Failure(e)
        case None => Status.Failure(notFound(j))
      })

    case Register(cid, viewer, q) => register(cid, viewer, q)
    case Unregister(cid, q) => unregister(cid, q)

    // A round clock edge (no timer of this actor's own): at the end the final standings become public (see
    // StandingsAnnouncer). The clock's row is used, as this actor may not have heard of an early is_finished yet.
    // Idempotent: observing the same board twice says nothing new.
    case RoundClockActor.Fired(_, c) =>
      expireRejudges()
      contests.get(c.id).foreach(st => guarded(c.id, st)(observePalevo(st, c)))

    case StartReload(cid) =>
      contests.get(cid).foreach { st =>
        st.reloadScheduled = false
        if (st.loading.isEmpty) startLoad(cid, st, "reload")
      }

    case Loaded(cid, result) => contests.get(cid).foreach(st => loaded(cid, st, result))

    case RejudgeContest(id, result) => rejudgeContest(id, result)

    case ContestChanged(c) =>
      contestRow(c)

    case ReloadAll =>
      for ((cid, st) <- contests)
        if (st.loading.isEmpty) startLoad(cid, st, roundChangedReason) else st.reloadAgain = true

    case ContestRemoved(cid) =>
      adding -= cid
      if (contests.remove(cid).isDefined) log.info(s"live monitor: round $cid is gone or no longer usable; dropped")
      subscribers.remove(cid).foreach(_.keys.foreach(q => Try(q.complete())))

    case Added(c, result) =>
      // Not wanted any more when the round was removed while it loaded.
      val wanted = adding(c.id)
      adding -= c.id
      result match {
        case Success(d) =>
          if (wanted && !contests.contains(c.id)) add(c, d, clock.now())
        case Failure(e) => log.warning(s"live monitor: cannot load new contest ${c.id}: $e")
      }
  }

  private def notFound(cid: Int) = new NoSuchElementException(s"live monitor: no contest $cid")

  private def contestOf(j: Join): Int = j match {
    case JoinAdmin(c) => c
    case JoinAdminStatus(c) => c
  }

  private def notFound(j: Join): NoSuchElementException = notFound(contestOf(j))

  /** The contest and viewer of a join. */
  private def viewerOf(j: Join): Option[(Int, Viewer)] = j match {
    case JoinAdmin(cid) => Some(cid -> BoardViewer)
    case JoinAdminStatus(cid) => Some(cid -> StatusViewer)
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

  private def applyTo(lc: LiveContest, ev: Ev): Option[Delta] = ev match {
    case EvArrived(rec) => Some(lc.onArrived(rec))
    case EvFinished(id, compiled, points) => lc.onFinished(id, compiled, points)
    case EvRejudge(id, _) => lc.onRejudge(id)
  }

  private def applyEvent(cid: Int, st: ContestState, ev: Ev, mayReload: Boolean): Unit = {
    ev match {
      case EvArrived(rec) if !st.live.accepts(rec) => st.ignored += rec.id
      case _ =>
    }
    Try(applyTo(st.live, ev)) match {
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

  /** Reads the round's team list; a burst of TeamsChanged while a read runs gives one more read after it. */
  private def readTeams(cid: Int, st: ContestState): Unit =
    if (st.teamsLoading) st.teamsAgain = true
    else {
      st.teamsLoading = true
      Future.fromTry(Try(loader.teams(cid))).flatten.onComplete(r => self ! TeamsLoaded(cid, r))
    }

  private def teamsLoaded(cid: Int, st: ContestState, result: Try[Seq[Team]]): Unit = {
    st.teamsLoading = false
    result match {
      case Success(ts) =>
        st.loading.foreach(l => l.teams = Some(ts))
        Try(st.live.onTeams(ts)) match {
          case Success(d) => changed(cid, st, d)
          case Failure(e) => failed(cid, st, e)
        }
      case Failure(e) =>
        // The change must not be lost: the whole contest is read again (with the reload backoff).
        log.warning(s"live monitor: contest $cid: cannot read its teams ($e); reloading the contest")
        st.dirty = true
        requestReload(cid, st)
    }
    if (st.teamsAgain) {
      st.teamsAgain = false
      readTeams(cid, st)
    }
  }

  private def rejudge(id: Int): Unit = {
    val found = contests.iterator.map { case (cid, st) => (cid, st, Try(st.live.onRejudge(id))) }
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
    val again = st.reloadAgain
    st.reloadAgain = false
    result match {
      case Failure(e) =>
        log.warning(s"live monitor: contest $cid: ${l.reason} load failed: $e")
        // A failed round-change load must not lose the change: retried with the reload backoff.
        if (l.reason == roundChangedReason) st.dirty = true
        retryLater(cid, st, l)
      case Success(data) =>
        if (rebuild(cid, st, l, data)) {
          // Unknown ids that this load was for: apply once, never reload for them again.
          l.waiting.foreach(ev => applyEvent(cid, st, ev, mayReload = false))
          // Unknown ids that came in while it ran: the new state may know them; otherwise reload again.
          val waitingAgain = st.waiting
          st.waiting = Vector.empty
          waitingAgain.foreach(ev => applyEvent(cid, st, ev, mayReload = true))
        } else retryLater(cid, st, l)
    }
    if (again && st.loading.isEmpty) startLoad(cid, st, roundChangedReason)
  }

  /**
    * The load did not produce a new state: keep its events for the next load and, when something is waiting for it
    * (queued unknown ids or a dirty contest), ask for a reload (rate-limited).
    */
  private def retryLater(cid: Int, st: ContestState, l: Load): Unit = {
    st.waiting = l.waiting ++ st.waiting
    if (st.dirty || st.waiting.nonEmpty) requestReload(cid, st)
  }

  /** Builds the state from the database data and replaces the live one; false when it could not be built. */
  private def rebuild(cid: Int, st: ContestState, l: Load, data: ContestData): Boolean = {
    // While a rejudge runs the database still holds the old result (`tested` stays true), so a recent in-flight
    // rejudge keeps its pending state: it is neither reported as a mismatch nor reverted. An entry older than
    // rejudgeMaxAge is taken to have lost its finish: it is dropped and the database state wins.
    expireRejudges()
    val ms = clock.millis()
    val subs = data.submits.map(s => if (youngRejudge(s.id, ms)) s.copy(state = live.Pending) else s)
    val result = Try {
      val fresh = st.live.rebuilt(data.problems, data.teams, subs)
      l.applied.foreach { ev =>
        Try(applyTo(fresh, ev)).failed.foreach(e => log.warning(s"live monitor: contest $cid: replay of $ev failed: $e"))
      }
      l.teams.foreach(ts => Try(fresh.onTeams(ts)).failed.foreach(e =>
        log.warning(s"live monitor: contest $cid: replay of its team list failed: $e")))
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
        if (diffs.nonEmpty)
          log.info(s"live monitor: ${l.reason} found differences in contest $cid: ${diffs.take(20).mkString("; ")}")
        val wasDirty = st.dirty
        st.live = fresh
        st.dirty = false
        if (diffs.nonEmpty || wasDirty) changed(cid, st, Delta(Seq(WholeOut(fresh.board))))
        true
    }
  }

  /**
    * A round_changed. A changed row is applied in memory. An unchanged row means something else of the round changed
    * (its problems, teacher-web 00009): the contest is reloaded, and a burst of them while a load runs coalesces into
    * one more load after it.
    */
  private def contestRow(c: Contest): Unit = contests.get(c.id) match {
    case Some(st) if st.live.contest == c =>
      if (st.loading.isEmpty) startLoad(c.id, st, roundChangedReason) else st.reloadAgain = true
    case Some(st) =>
      Try(st.live.onContest(c)) match {
        case Success(d) =>
          changed(c.id, st, d)
          // A new end or is_finished may make the round over without changing the board.
          guarded(c.id, st)(observePalevo(st))
        case Failure(e) => failed(c.id, st, e)
      }
    case None =>
      if (!adding(c.id)) {
        adding += c.id
        safeLoad(c).onComplete(r => self ! Added(c, r))
      }
  }

  /** After a change: stream it to the admins and feed palevo. */
  private def changed(cid: Int, st: ContestState, d: Delta): Unit =
    guarded(cid, st) {
      publish(cid, st, d)
      if (d.nonEmpty) observePalevo(st)
    }

  /** Feeds the standings announcer the contest's board (jury events while it runs, the final standings once over). */
  private def observePalevo(st: ContestState, contest: Contest): Unit =
    standings.observe(contest, PalevoView.of(st.live.board), clock.now()).foreach(palevoSink.send)

  private def observePalevo(st: ContestState): Unit = observePalevo(st, st.live.contest)

  // ---- streams ----

  private def boardEvent(b: LiveBoard): Event = LiveJson.event("board", LiveJson.board(b))

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
        .merge(Source.tick(timings.ping, timings.ping, pingEvent), eagerComplete = true) // SSE keep-alive comments
        .watchTermination() { (q, done) =>
          me ! Register(cid, viewer, q)
          done.onComplete(_ => me ! Unregister(cid, q))(queueEc)
          NotUsed
        }
      sender() ! src
    }

  private def register(cid: Int, viewer: Viewer, q: BoundedSourceQueue[Event]): Unit =
    contests.get(cid) match {
      case Some(st) =>
        val subs = subscribers.getOrElseUpdate(cid, mutable.Map.empty)
        subs(q) = viewer
        viewer match {
          case StatusViewer => offer(cid, q, LiveJson.event("ready", Json.obj()))
          case BoardViewer => offer(cid, q, Try(boardEvent(st.live.board)).getOrElse(boardEvent(LiveBoard.empty)))
        }
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

  /**
    * Reads the submit's row for the status pages that are open and sends it on. `cid` -1: the contest is not known
    * (a rejudge request has only the id), any open status page will do to ask for.
    */
  private def requestStatusRow(cid: Int, id: Int, inTesting: Option[models.SubmitResult]): Unit =
    if (hasStatusViewers(cid)) {
      statusSeq += 1
      val seq = statusSeq
      statusRequests(id) = seq
      val me = self
      Future.fromTry(Try(statusRows.render(id, inTesting))).flatten.onComplete(r => me ! StatusRowReady(seq, id, r))
    }

  private def hasStatusViewers(cid: Int): Boolean =
    subscribers.iterator.exists { case (c, subs) => (cid < 0 || c == cid) && subs.valuesIterator.contains(StatusViewer) }

  private def statusRowReady(seq: Long, id: Int, result: Try[Option[StatusRow]]): Unit =
    if (statusRequests.get(id).contains(seq)) {
      statusRequests -= id
      result match {
        case Success(Some(row)) =>
          lazy val event = LiveJson.event("statusRow", Json.obj("id" -> row.id, "team" -> row.team, "rw" -> row.rw, "ro" -> row.ro))
          subscribers.get(row.contest).foreach(_.toList.foreach {
            case (q, StatusViewer) => offer(row.contest, q, event)
            case _ =>
          })
        case Success(None) => log.debug(s"live monitor: no status row for submit $id")
        case Failure(e) => log.warning(s"live monitor: cannot build the status row of submit $id: $e")
      }
    }

  private def publish(cid: Int, st: ContestState, d: Delta): Unit =
    subscribers.get(cid).filter(_.nonEmpty).foreach { subs =>
      lazy val events = d.board.map(plainEvent)
      subs.toList.foreach {
        case (q, BoardViewer) => events.foreach(offer(cid, q, _))
        case (_, StatusViewer) => ()
      }
    }
}
