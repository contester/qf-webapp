package actors

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardCopyOption}

import akka.actor.{Actor, Props}
import models._
import org.apache.commons.io.FileUtils
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest
import play.api.i18n.{Lang, MessagesApi, MessagesImpl}
import slick.jdbc.JdbcBackend

import scala.concurrent.Future

/**
  * The fallback static board writer, run only when the live monitor is off: writes `<round>.html` with the points
  * board into monitor.static_location, and only for rounds that are over, since the files are public.
  * The pages do not ask this actor; they render the board on demand (BoardSource).
  * No polling: the files are written once at start-up and again for a round when the round clock says it ended
  * (RoundClockActor.Fired on the event stream). TODO(task17): re-render on testing_finished / credential and
  * participant changes too; until then a result judged after the end reaches the file only at the next restart.
  * TODO(task18): palevo standings were fed from here (ACM status); with the live monitor off they are not sent now.
  */
object MonitorActor {
  def props(db: JdbcBackend#DatabaseDef, staticLocation: Option[String], messagesApi: MessagesApi) =
    Props(new MonitorActor(db, staticLocation, messagesApi))

  case class State(boards: Seq[(Contest, MonitorPoints)])
  /** Writes the files of every round that is over (start-up). */
  case object Refresh
}

class MonitorActor(db: JdbcBackend#DatabaseDef, staticLocation: Option[String], messagesApi: MessagesApi) extends Actor {
  import MonitorActor._
  import context.dispatcher

  private implicit val messages: MessagesImpl = MessagesImpl(Lang("ru"), messagesApi)

  private val staticLocationFile = staticLocation.map(x => new File(x))

  override def preStart(): Unit = {
    super.preStart()
    context.system.eventStream.subscribe(self, classOf[RoundClockActor.Fired])
    self ! Refresh
  }

  override def postStop(): Unit = {
    context.system.eventStream.unsubscribe(self)
    super.postStop()
  }

  private def loadBoards(rounds: Future[Seq[Contest]]): Unit = {
    val now = DateTime.now
    val fu = rounds.flatMap { rs =>
      Future.sequence(rs.filter(Visibility.canSeeBoard(_, Anonymous, now)).map { r =>
        MonitorPoints.load(db, r.id).map(r -> _)
      })
    }
    fu.foreach(st => self ! State(st))
    fu.failed.foreach(e => play.api.Logger(getClass).error(s"writing the static boards: $e"))
  }

  private def write(contest: Contest, board: MonitorPoints): Unit =
    for (location <- staticLocationFile) {
      val tmpFile = new File(location, s"${contest.id}.html.new")
      FileUtils.writeStringToFile(tmpFile, Compressor(views.html.staticmonitor(contest, board).body), StandardCharsets.UTF_8)
      Files.move(tmpFile.toPath, new File(location, s"${contest.id}.html").toPath, StandardCopyOption.ATOMIC_MOVE)
    }

  override def receive: Receive = {
    case Refresh => loadBoards(Rounds.currentRounds(db))
    case RoundClockActor.Fired(_: RoundClock.Ended, round) => loadBoards(Future.successful(Seq(round)))
    case State(boards) => boards.foreach { case (c, b) => write(c, b) }
  }
}
