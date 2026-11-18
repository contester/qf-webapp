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
  * TODO(task18): palevo standings were fed from here (ACM status); with the live monitor off they are not sent now.
  */
object MonitorActor {
  def props(db: JdbcBackend#DatabaseDef, staticLocation: Option[String], messagesApi: MessagesApi) =
    Props(new MonitorActor(db, staticLocation, messagesApi))

  case class State(boards: Seq[(Contest, MonitorPoints)])
  case object Refresh
}

class MonitorActor(db: JdbcBackend#DatabaseDef, staticLocation: Option[String], messagesApi: MessagesApi) extends Actor {
  import MonitorActor._
  import context.dispatcher

  import scala.concurrent.duration._
  import scala.language.postfixOps

  private implicit val messages: MessagesImpl = MessagesImpl(Lang("ru"), messagesApi)

  private val staticLocationFile = staticLocation.map(x => new File(x))

  override def preStart(): Unit = {
    super.preStart()
    self ! Refresh
  }

  private def loadBoards(): Unit = {
    val now = DateTime.now
    val fu = Rounds.currentRounds(db).flatMap { rounds =>
      Future.sequence(rounds.filter(Visibility.canSeeBoard(_, Anonymous, now)).map { r =>
        MonitorPoints.load(db, r.id).map(r -> _)
      })
    }
    fu.foreach(st => self ! State(st))
    // TODO(task16): periodic refresh is polling; re-render on testing_finished/round_changed notifications instead.
    fu.onComplete(_ => context.system.scheduler.scheduleOnce(20 seconds, self, Refresh))
  }

  private def write(contest: Contest, board: MonitorPoints): Unit =
    for (location <- staticLocationFile) {
      val tmpFile = new File(location, s"${contest.id}.html.new")
      FileUtils.writeStringToFile(tmpFile, Compressor(views.html.staticmonitor(contest, board).body), StandardCharsets.UTF_8)
      Files.move(tmpFile.toPath, new File(location, s"${contest.id}.html").toPath, StandardCopyOption.ATOMIC_MOVE)
    }

  override def receive: Receive = {
    case Refresh => loadBoards()
    case State(boards) => boards.foreach { case (c, b) => write(c, b) }
  }
}
