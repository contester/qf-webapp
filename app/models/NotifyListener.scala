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
  * Pushes the database's NOTIFY events (teacher-web triggers, migration 00006) to the actors.
  *
  * The actor owns one dedicated pgjdbc connection, opened with DriverManager outside the Slick pool, on a thread of
  * its own: `PGConnection.getNotifications(0)` blocks that thread until a notification arrives, so nothing wakes up
  * periodically. The connection has TCP keep-alive on, so a dead peer is noticed by the kernel. Stopping the actor
  * aborts the connection, which ends the blocked read and the thread.
  *
  * On a lost connection the thread reconnects with exponential backoff (1 s doubling to 30 s, reset by a successful
  * connect). That is the only timer. There is no recovery query after a reconnect: the database is the truth and every
  * page reads it, so a notification missed while disconnected only delays a live update until the page is reloaded.
  *
  * Each notification is turned into the message the actors already understand, one at a time in arrival order;
  * a malformed payload or a failed lookup is logged and skipped.
  */
object NotifyListener {
  /** The channels qf consumes; submit_new and eval_new are the bridge's. */
  val Channels: Seq[String] = Seq("testing_finished", "eval_finished", "round_changed", "credential_changed", "participant_changed")

  val DefaultApplicationName = "qf-notify-listener"

  case class ConnectionSettings(url: String, user: String, password: String, applicationName: String = DefaultApplicationName)

  object ConnectionSettings {
    /** The URL and credentials of play-slick's default database (`slick.dbs.default.db`). */
    def fromConfig(configuration: Configuration): ConnectionSettings = {
      val c = configuration.get[Configuration]("slick.dbs.default.db")
      def opt(k: String) = c.getOptional[String](k).orElse(c.getOptional[String](s"properties.$k"))
      ConnectionSettings(opt("url").getOrElse(throw new IllegalArgumentException("slick.dbs.default.db.url is not set")),
        opt("user").getOrElse(""), opt("password").getOrElse(""))
    }
  }

  /**
    * Where the events go: `status` gets FinishedTesting, CustomTestResult and ContestChanged (the messages
    * it already handles), `teams` gets TeamsChanged, and `contestChanged` is also told of a changed round (the live monitor).
    */
  case class Targets(status: ActorRef, teams: ActorRef, contestChanged: Contest => Unit = _ => ())

  /** Published on the event stream each time the listener has (re)connected and issued its LISTENs. */
  case class Listening(applicationName: String)

  def props(settings: ConnectionSettings, db: JdbcBackend#DatabaseDef, targets: Targets): Props =
    Props(new NotifyListener(settings, db, targets))

  private case class Notified(channel: String, payload: String)

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
    db.run(sql"""select s.id, s.participant_id, s.round_id, s.problem_id, s.compiled, s.passed, s.taken
                   from testings t join submits s on s.id = t.submit_id
                  where t.id = $testingId and s.testing_id = t.id""".as[(Long, Int, Int, String, Boolean, Int, Int)].headOption)
      .map(_.map { case (submit, team, round, problem, compiled, passed, taken) =>
        FinishedTesting(SubmitObject(submit.toInt, team, round, problem), testingId.toInt, compiled, passed, taken)
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
    val props = new Properties()
    props.setProperty("user", settings.user)
    props.setProperty("password", settings.password)
    props.setProperty("ApplicationName", settings.applicationName)
    props.setProperty("tcpKeepAlive", "true")
    val c = DriverManager.getConnection(settings.url, props)
    c.setAutoCommit(true)
    val st = c.createStatement()
    try Channels.foreach(ch => st.execute(s"LISTEN $ch")) finally st.close()
    c
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
    case () => // the status actor's acknowledgement of a result; nothing waits for it
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
          case Some(c) =>
            targets.status.tell(StatusActor.ContestChanged(c), self)
            Try(targets.contestChanged(c)).failed.foreach(e => logger.warn(s"contest change hook for round $id: $e"))
          case None =>
            // Gone, of another season, or without times: such a round is unusable, so nobody is told.
            logger.info(s"round $id is not a current round with times; ignored")
        }
      case Some(CredentialChanged(round, _)) =>
        targets.teams.tell(StatusActor.TeamsChanged(round), self)
        Future.unit
      case Some(ParticipantChanged(participant)) =>
        roundsOfParticipant(db, participant).map(_.foreach(r => targets.teams.tell(StatusActor.TeamsChanged(r), self)))
    }
}
