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
  def props(db: JdbcBackend#DatabaseDef) = Props(classOf[MonitorActor], db)

  case class Get(contest: Int)
  case object Refresh
  case class ValidContests(contests: Set[Int])
}

class MonitorActor(db: JdbcBackend#DatabaseDef) extends Actor {
  import MonitorActor._
  import context.dispatcher

  import scala.concurrent.duration._
  import scala.language.postfixOps

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

  private val firstTime = mutable.HashMap[Int, Promise[Option[StoredContestStatus]]]()
  private val monitors = mutable.HashMap[Int, StoredContestStatus]()
  private var validContests: Option[ValidContests] = None

  override def receive: Receive = {
    case Refresh => {
      db.run(Contests.getContests).flatMap { contests =>
        self ! ValidContests(contests.map(_.id).toSet)

        Future.sequence(contests.map(getContestMonitor(_)).map { f =>
          f.foreach { v =>
            self ! v
          }
          f
        })
      }.onComplete { _ =>
        context.system.scheduler.scheduleOnce(20 seconds, self, Refresh)
      }
    }

    case c: StoredContestStatus => {
      firstTime.remove(c.contest.id).foreach { f =>
        f.success(Some(c))
      }
      monitors.put(c.contest.id, c)

      FileUtils.writeStringToFile(new File(s"/ssd/s/qq/${c.contest.id}.html"),
        Compressor(views.html.staticmonitor(c.contest, c.monitor(false).status).body), Charsets.UTF_8)
    }

    case v: ValidContests => {
      validContests = Some(v)
      firstTime.keys.filterNot(v.contests).foreach { id =>
        firstTime.remove(id).foreach { f =>
          f.success(None)
        }
      }
    }

    case Get(id) => {
      import akka.pattern.pipe
      monitors.get(id) match {
        case Some(s) => sender() ! Some(s)
        case None =>
          validContests match {
            case Some(contests) if contests.contests(id) =>
              firstTimeFuture(id).pipeTo(sender())
            case None =>
              firstTimeFuture(id).pipeTo(sender())
            case _ =>
              sender ! None
          }
      }
    }
  }

  private def firstTimeFuture(id: Int) =
    firstTime.getOrElseUpdate(id, Promise[Option[StoredContestStatus]]()).future

  context.system.scheduler.scheduleOnce(0 seconds, self, Refresh)
}
