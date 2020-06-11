package actors

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardCopyOption}

import akka.actor.{Actor, Props, Stash}
import models._
import org.apache.commons.io.FileUtils
import org.stingray.qf.models.TeamClient
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import slick.jdbc.JdbcBackend

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object MonitorActor {
  def props(db: JdbcBackend#DatabaseDef, staticLocation: Option[String],
            teamClient: TeamClient, problemClient: ProblemClient, messagesApi: MessagesApi) =
    Props(new MonitorActor(db, staticLocation, teamClient, problemClient, messagesApi))

  case class Get(contest: Int)
  case object Start
  case class State(monitors: Seq[StoredContestStatus])
  case object Refresh
}

class MonitorActor(db: JdbcBackend#DatabaseDef,
                   staticLocation: Option[String],
                   teamClient: TeamClient,
                   problemClient: ProblemClient,
                   messagesApi: MessagesApi) extends Actor with Stash {
  import MonitorActor._
  import context.dispatcher

  import scala.concurrent.duration._
  import scala.language.postfixOps

  private implicit val messages = MessagesImpl(Lang("en"), messagesApi)

  private val staticLocationFile = staticLocation.map(x => new File(x))

  @throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    self ! Refresh
  }

  private implicit val standardTimeout: akka.util.Timeout = {
    import scala.concurrent.duration._
    Duration(5, SECONDS)
  }

  private def getContestMonitor(contest: Contest)(implicit ec: ExecutionContext) = {
    val cid = contest.id

    problemClient.getProblems(cid)
      .zip(teamClient.getTeams(cid))
      .zip(db.run(Submits.getContestSubmits(cid)))
      .map {
      case ((problems, teams), submits) =>
        val calcStatus: (Seq[Submit]) => AnyStatus with Product with Serializable =
            ACM.calculateStatus(problems, teams.values.toSeq, _)

        val subs0 = submits.filter(_.finished).map(Submits.upliftSub(_))

        StoredContestStatus(contest, calcStatus(subs0.filter(!_.afterFreeze)), calcStatus(subs0))
    }
  }

  private def loadMonitors()(implicit ec: ExecutionContext) = {
    import utils.MyPostgresProfile.api._
    val fu = db.run(SlickModel.contests.result).flatMap { contests =>
      Future.sequence(contests.map(getContestMonitor))
    }
    fu.foreach(st => self ! State(st))
    fu.onComplete { _ =>
      context.system.scheduler.scheduleOnce(20 seconds, self, Refresh)
    }
    fu
  }

  private val monitors = mutable.HashMap[Int, StoredContestStatus]()

  private def updateMonitors(state: Seq[StoredContestStatus]) = {
    state.map(_.contest.id).foreach(monitors.remove)
    state.foreach { st =>
      monitors.put(st.contest.id, st)
      if (st.contest.id <11) {
        for (location <- staticLocationFile) {
          val tmpFile = new File(location, s"${st.contest.id}.html.new")
          FileUtils.writeStringToFile(tmpFile,
            Compressor(views.html.staticmonitor(st.contest, st.monitor(false).status).body), StandardCharsets.UTF_8)
          Files.move(tmpFile.toPath, new File(location, s"${st.contest.id}.html").toPath, StandardCopyOption.ATOMIC_MOVE)
        }
      }
    }
  }

  def initialized: Receive = {
    case Refresh => loadMonitors()
    case State(state) => updateMonitors(state)
    case Get(id) => sender ! Try(monitors.get(id))
  }

  override def receive: Receive = {
    case Refresh => loadMonitors()
    case State(state) => {
      updateMonitors(state)
      unstashAll()
      context.become(initialized)
    }
    case _ =>
      stash()
  }
}
