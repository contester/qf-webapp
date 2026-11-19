package models

import actors.StatusActor
import akka.actor.{Actor, ActorRef, Props}
import models.ContesterResults.{CustomTestResult, FinishedTesting, SubmitObject}
import org.postgresql.PGConnection
import org.stingray.contester.dbmodel.Contest
import play.api.{Configuration, Logging}
import slick.jdbc.JdbcBackend

import java.sql.{Connection, DriverManager}
import java.util.Properties
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NonFatal

/**
  * Pushes the database's NOTIFY events (teacher-web triggers, migrations 00006, 00008 and 00009) to the actors.
  * `round_changed <round id>` fires for an insert, update or delete of the round, for any change of its `problems`
  * rows (the bridge's problem sync) and, for every round of the season, when `seasons.is_current` flips. The
  * listener re-reads the round: a current round with times becomes ContestChanged (status, teams, and the hooks,
  * which also reload the problems), anything else ContestRemoved.
  *
  * The actor owns one dedicated pgjdbc connection, opened with DriverManager outside the Slick pool, on a thread of
  * its own: `PGConnection.getNotifications(0)` blocks that thread until a notification arrives, so nothing wakes up
  * periodically. TCP keep-alive with short timings on both ends (see KeepAlive) makes the kernel notice a silently
  * dropped connection within about two minutes; the blocked read then fails. Stopping the actor
  * aborts the connection, which ends the blocked read and the thread.
  *
  * On a lost connection the thread reconnects with exponential backoff (1 s doubling to 30 s, reset by a successful
  * connect). That is the only timer. After every connect (the first one included) the listener catches up once, when
  * the targets ask for it (`Targets.catchUp`): notifications sent while it was disconnected are lost, so it runs the
  * `reconnected` hook (the application drops the whole public board cache and makes the live monitor reload every
  * contest), re-reads the current rounds and delivers each as a round_changed would (ContestChanged, TeamsChanged and
  * the hooks), and removes the rounds the status actor still knows that are gone. The catch-up is queued before any
  * notification of the new connection, so a later notification's round reload never gets overtaken by it.
  *
  * Each notification is turned into the message the actors already understand, one at a time in arrival order;
  * a malformed payload or a failed lookup is logged and skipped.
  */
object NotifyListener {
  /** The channels qf consumes; submit_new and eval_new are the bridge's. */
  val Channels: Seq[String] = Seq("testing_finished", "eval_finished", "round_changed", "credential_changed", "participant_changed")

  val DefaultApplicationName = "qf-notify-listener"

  /**
    * TCP keep-alive of the notification connection, in seconds: probes start after `idle` seconds of silence and
    * repeat every `interval`; after `count` unanswered ones the peer is dead. Set on both ends: on our socket through
    * KeepAliveSocketFactory (a dead server breaks the blocked read) and on the server through the `options` startup
    * parameter (its probes also keep NAT and firewall state alive, and a dead client is dropped).
    */
  case class KeepAlive(idle: Int = 60, interval: Int = 10, count: Int = 6) {
    def factoryArg: String = s"$idle,$interval,$count"
    def serverOptions: String = s"-c tcp_keepalives_idle=$idle -c tcp_keepalives_interval=$interval -c tcp_keepalives_count=$count"
  }

  case class ConnectionSettings(url: String, user: String, password: String, applicationName: String = DefaultApplicationName,
                                keepAlive: KeepAlive = KeepAlive())

  object ConnectionSettings {
    /**
      * The URL and credentials of play-slick's default database (`slick.dbs.default.db`, or its `properties.*`), and
      * the keep-alive from `notify.keepalive.idle|interval|count` (seconds; default 60, 10, 6).
      */
    def fromConfig(configuration: Configuration): ConnectionSettings = {
      val base = "slick.dbs.default.db"
      def opt(k: String) = configuration.getOptional[String](s"$base.$k").orElse(configuration.getOptional[String](s"$base.properties.$k"))
      val d = KeepAlive()
      def ka(k: String, default: Int) = configuration.getOptional[Int](s"notify.keepalive.$k").getOrElse(default)
      ConnectionSettings(opt("url").getOrElse(throw new IllegalArgumentException(s"$base.url is not set: the notification listener needs it")),
        opt("user").getOrElse(""), opt("password").getOrElse(""), DefaultApplicationName,
        KeepAlive(ka("idle", d.idle), ka("interval", d.interval), ka("count", d.count)))
    }
  }

  /**
    * The URL and properties to connect with. An `options` parameter already in the URL is taken out of it and
    * appended after the keep-alive settings (pgjdbc lets URL parameters override properties, and with `-c` the last
    * setting wins, so the operator's own settings are kept).
    */
  def connectProperties(settings: ConnectionSettings): (String, Properties) = {
    val (base, query) = settings.url.indexOf('?') match {
      case -1 => (settings.url, "")
      case i => (settings.url.substring(0, i), settings.url.substring(i + 1))
    }
    val params = query.split('&').filter(_.nonEmpty)
    def isOptions(p: String) = p.takeWhile(_ != '=') == "options"
    val theirs = params.filter(isOptions).map(p => java.net.URLDecoder.decode(p.dropWhile(_ != '=').drop(1), "UTF-8"))
    val rest = params.filterNot(isOptions)
    val url = if (rest.isEmpty) base else base + "?" + rest.mkString("&")
    val props = new Properties()
    props.setProperty("user", settings.user)
    props.setProperty("password", settings.password)
    props.setProperty("ApplicationName", settings.applicationName)
    props.setProperty("tcpKeepAlive", "true")
    props.setProperty("socketFactory", classOf[KeepAliveSocketFactory].getName)
    props.setProperty("socketFactoryArg", settings.keepAlive.factoryArg)
    props.setProperty("options", (settings.keepAlive.serverOptions +: theirs.toSeq).filter(_.trim.nonEmpty).mkString(" "))
    (url, props)
  }

  /**
    * Where the events go: `status` gets FinishedTesting, CustomTestResult, ContestChanged and ContestRemoved (it feeds
    * the round clock), `teams` gets TeamsChanged (also for every round_changed: a round that became usable needs its
    * teams, one that did not is dropped), and the hooks are told of a changed or removed round (the live monitor, the
    * problems, the public board cache) and of every TeamsChanged (the live monitor, the public board cache).
    */
  case class Targets(status: ActorRef, teams: ActorRef, contestChanged: Contest => Unit = _ => (),
                     contestRemoved: Int => Unit = _ => (), teamsChanged: Int => Unit = _ => (),
                     catchUp: Option[CatchUp] = None)

  /**
    * The catch-up after each (re)connect. `knownRounds`: the rounds the status actor holds (those not current any
    * more are removed); `reconnected`: runs first, to drop caches and reload the live monitor.
    */
  case class CatchUp(knownRounds: () => Future[Seq[Int]], reconnected: () => Unit)

  /** Published on the event stream each time the listener has (re)connected and issued its LISTENs. */
  case class Listening(applicationName: String)

  def props(settings: ConnectionSettings, db: JdbcBackend#DatabaseDef, targets: Targets): Props =
    Props(new NotifyListener(settings, db, targets))

  private case class Notified(channel: String, payload: String)
  private case object Connected

  /** What a notification asks for, from its channel and text payload; None for an unknown channel or a bad payload. */
  sealed trait Event
  case class TestingFinished(testingId: Long) extends Event
  case class EvalFinished(id: Long) extends Event
  case class RoundChanged(roundId: Int) extends Event
  case class CredentialChanged(roundId: Int, participantId: Int) extends Event
  case class ParticipantChanged(participantId: Int) extends Event

  def parse(channel: String, payload: String): Option[Event] = {
    def long = Try(payload.trim.toLong).toOption
    def int = Try(payload.trim.toInt).toOption
    channel match {
      case "testing_finished" => long.map(TestingFinished)
      case "eval_finished" => long.map(EvalFinished)
      case "round_changed" => int.map(RoundChanged)
      case "participant_changed" => int.map(ParticipantChanged)
      case "credential_changed" => payload.split(':') match {
        case Array(r, p) => for (ri <- Try(r.trim.toInt).toOption; pi <- Try(p.trim.toInt).toOption) yield CredentialChanged(ri, pi)
        case _ => None
      }
      case _ => None
    }
  }

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._

  /** The finished testing as the status actor takes it; None when unknown or not the submit's current testing (stale). */
  def finishedTesting(db: JdbcBackend#DatabaseDef, testingId: Long)(implicit ec: ExecutionContext): Future[Option[FinishedTesting]] =
    db.run(sql"""select s.id, s.participant_id, s.round_id, s.problem_id, s.compiled, s.passed, s.taken, s.points
                   from testings t join submits s on s.id = t.submit_id
                  where t.id = $testingId and s.testing_id = t.id""".as[(Long, Int, Int, String, Boolean, Int, Int, Option[Double])].headOption)
      .map(_.map { case (submit, team, round, problem, compiled, passed, taken, points) =>
        FinishedTesting(SubmitObject(submit.toInt, team, round, problem), testingId.toInt, compiled, passed, taken, points)
      })

  def customTestResult(db: JdbcBackend#DatabaseDef, id: Long)(implicit ec: ExecutionContext): Future[Option[CustomTestResult]] =
    db.run(sql"select round_id, participant_id from custom_tests where id = $id".as[(Int, Int)].headOption)
      .map(_.map { case (round, team) => CustomTestResult(id.toInt, round, team) })

  /** The current-season rounds the participant has a credential for. */
  def roundsOfParticipant(db: JdbcBackend#DatabaseDef, participantId: Int): Future[Seq[Int]] =
    db.run(sql"""select distinct rc.round_id from round_credentials rc
                   join contest_rounds r on r.id = rc.round_id join seasons s on s.id = r.season_id
                  where rc.participant_id = $participantId and s.is_current order by 1""".as[Int])
}

class NotifyListener(settings: NotifyListener.ConnectionSettings, db: JdbcBackend#DatabaseDef,
                     targets: NotifyListener.Targets) extends Actor with Logging {
  import NotifyListener._
  import context.dispatcher

  private[this] val eventStream = context.system.eventStream // the reader thread must not touch `context`
  @volatile private[this] var running = true
  @volatile private[this] var connection: Option[Connection] = None
  private[this] val reader = new Thread(() => readLoop(), s"notify-listener-${settings.applicationName}")
  // Notifications are handled one after another, so a later round reload never overtakes an earlier one.
  private[this] var last: Future[Unit] = Future.unit

  override def preStart(): Unit = {
    reader.setDaemon(true)
    reader.start()
  }

  override def postStop(): Unit = {
    running = false
    connection.foreach(abort)
    reader.interrupt()
  }

  private def abort(c: Connection): Unit =
    Try(c.abort(r => r.run())).failed.foreach(e => logger.warn(s"closing the notification connection: $e"))

  private def connect(): Connection = {
    val (url, props) = connectProperties(settings)
    val c = DriverManager.getConnection(url, props)
    try {
      c.setAutoCommit(true)
      val st = c.createStatement()
      try Channels.foreach(ch => st.execute(s"LISTEN $ch")) finally st.close()
      c
    } catch {
      case NonFatal(e) =>
        Try(c.close())
        throw e
    }
  }

  private val minBackoffMs = 1000L
  private val maxBackoffMs = 30000L

  private def readLoop(): Unit = {
    var backoff = minBackoffMs
    while (running) {
      var c: Option[Connection] = None
      try {
        c = Some(connect())
        connection = c
        if (!running) c.foreach(abort)
        else {
          backoff = minBackoffMs
          logger.info(s"listening on ${Channels.mkString(", ")}")
          self ! Connected // ahead of this connection's notifications
          eventStream.publish(Listening(settings.applicationName))
          val pg = c.get.unwrap(classOf[PGConnection])
          while (running) {
            val ns = pg.getNotifications(0) // blocks until something arrives or the connection breaks
            if (ns != null) ns.foreach(n => self ! Notified(n.getName, n.getParameter))
          }
        }
      } catch {
        case NonFatal(e) if running => logger.warn(s"notification connection lost: $e; reconnecting in ${backoff} ms")
        case NonFatal(_) =>
      } finally {
        connection = None
        c.foreach(x => Try(x.close()))
      }
      if (running) {
        try Thread.sleep(backoff) catch { case _: InterruptedException => }
        backoff = math.min(backoff * 2, maxBackoffMs)
      }
    }
  }

  override def receive: Receive = {
    case Notified(channel, payload) =>
      last = last.flatMap(_ => handle(channel, payload)).recover {
        case NonFatal(e) => logger.error(s"handling $channel '$payload': $e")
      }
    case Connected =>
      targets.catchUp.foreach { cu =>
        last = last.flatMap(_ => catchUp(cu)).recover {
          case NonFatal(e) => logger.error(s"catching up after a reconnect: $e")
        }
      }
    case () => // the status actor's acknowledgement of a result; nothing waits for it
  }

  private def catchUp(cu: CatchUp): Future[Unit] = {
    Try(cu.reconnected()).failed.foreach(e => logger.warn(s"reconnect hook: $e"))
    val known = Future.fromTry(Try(cu.knownRounds())).flatten.recover {
      case NonFatal(e) =>
        logger.warn(s"rounds known to the status actor: $e")
        Seq.empty[Int]
    }
    for (rounds <- Rounds.currentRounds(db); held <- known) yield {
      logger.info(s"catching up after (re)connect: ${rounds.size} current rounds")
      rounds.foreach(roundChanged)
      (held.toSet -- rounds.map(_.id)).toSeq.sorted.foreach(roundRemoved)
    }
  }

  private def roundChanged(c: Contest): Unit = {
    targets.status.tell(StatusActor.ContestChanged(c), self)
    teamsChanged(c.id)
    Try(targets.contestChanged(c)).failed.foreach(e => logger.warn(s"contest change hook for round ${c.id}: $e"))
  }

  private def roundRemoved(id: Int): Unit = {
    targets.status.tell(StatusActor.ContestRemoved(id), self)
    teamsChanged(id)
    Try(targets.contestRemoved(id)).failed.foreach(e => logger.warn(s"contest removal hook for round $id: $e"))
  }

  private def teamsChanged(round: Int): Unit = {
    targets.teams.tell(StatusActor.TeamsChanged(round), self)
    Try(targets.teamsChanged(round)).failed.foreach(e => logger.warn(s"teams change hook for round $round: $e"))
  }

  private def handle(channel: String, payload: String): Future[Unit] =
    parse(channel, payload) match {
      case None =>
        logger.warn(s"ignoring malformed notification $channel '$payload'")
        Future.unit
      case Some(TestingFinished(id)) =>
        finishedTesting(db, id).map {
          case Some(f) =>
            logger.info(s"testing finished: $f")
            targets.status.tell(f, self)
          case None => logger.info(s"testing $id is unknown or not its submit's current testing; ignored")
        }
      case Some(EvalFinished(id)) =>
        customTestResult(db, id).map {
          case Some(r) =>
            logger.info(s"custom test finished: $r")
            targets.status.tell(r, self)
          case None => logger.info(s"custom test $id is unknown; ignored")
        }
      case Some(RoundChanged(id)) =>
        Rounds.currentRound(db, id).map {
          case Some(c) => roundChanged(c)
          case None =>
            // Gone, of another season, or without times: such a round is unusable and is dropped everywhere.
            logger.info(s"round $id is not a current round with times; removing it")
            roundRemoved(id)
        }
      case Some(CredentialChanged(round, _)) =>
        teamsChanged(round)
        Future.unit
      case Some(ParticipantChanged(participant)) =>
        roundsOfParticipant(db, participant).map(_.foreach(teamsChanged))
    }
}
