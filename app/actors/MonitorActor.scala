package actors

import java.io.File

import akka.actor.{Stash, Actor, Props}
import models._
import org.apache.commons.io.{Charsets, FileUtils}
import play.twirl.api.HtmlFormat
import slick.jdbc.JdbcBackend

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}

object MonitorActor {
  def props(db: JdbcBackend#DatabaseDef, staticLocation: Option[String]) = Props(classOf[MonitorActor], db, staticLocation)

  case class Get(contest: Int)
  case object Start
  case class State(monitors: Seq[StoredContestStatus])
  case object Refresh
}

class MonitorActor(db: JdbcBackend#DatabaseDef, staticLocation: Option[String]) extends Actor with Stash {
  import MonitorActor._
  import context.dispatcher

  import scala.concurrent.duration._
  import scala.language.postfixOps

  @throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    self ! Refresh
  }

  private def getContestMonitor(contest: Contest)(implicit ec: ExecutionContext) = {
    val cid = contest.id

    db.run(Contests.getProblems(cid))
      .zip(db.run(Contests.getTeams(cid)))
      .zip(db.run(Submits.getContestSubmits(cid)))
      .map {
      case ((problems, teams), submits) =>
        val calcStatus: (Seq[Submit]) => AnyStatus with Product with Serializable =
          if (contest.schoolMode)
            School.calculateStatus(problems, teams, _)
          else
            ACM.calculateStatus(problems, teams, _)

        StoredContestStatus(contest, calcStatus(submits.filter(!_.afterFreeze)), calcStatus(submits))
    }
  }

  private def loadMonitors()(implicit ec: ExecutionContext) = {
    val fu = db.run(Contests.getContests).flatMap { contests =>
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
      for (location <- staticLocation) {
        FileUtils.writeStringToFile(new File(s"/ssd/s/qq/${st.contest.id}.html"),
          Compressor(views.html.staticmonitor(st.contest, st.monitor(false).status).body), Charsets.UTF_8)
      }
    }
  }

  def initialized: Receive = {
    case Refresh => loadMonitors()
    case State(state) => updateMonitors(state)
    case Get(id) => sender ! monitors.get(id)
  }

  override def receive: Receive = {
    case Refresh => loadMonitors()
    case State(state) => {
      updateMonitors(state)
      unstashAll()
      context.become(initialized)
    }
    case Get(id) =>
      stash()
  }
}
