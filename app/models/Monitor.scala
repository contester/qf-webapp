package models

import java.util.concurrent.TimeUnit
import javax.inject.Inject

import org.jboss.netty.util.{Timeout, TimerTask, HashedWheelTimer}
import play.api.db.slick.DatabaseConfigProvider
import play.api.mvc.{Action, Controller}
import slick.backend.DatabaseConfig
import slick.dbio.DBIO
import slick.driver.JdbcProfile
import slick.jdbc.GetResult
import views.html

import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.{ExecutionContext, Future}

class Status(val problems: Seq[String], val rows: Seq[Row])
class Team(val id: Int, val name: String)
class Row(val team: Team, val rank: Int, val solved: Int, val time: Int, val cells: Map[String, Cell])
class Cell(val problem: String, val success: Boolean, val failedAttempts: Int, val time: Int) {

  def timeStr = "%02d:%02d".format(time / 3600, (time / 60) % 60)

  def toShort =
    (if (success) "+" else "-") +
      (if (failedAttempts > 0) failedAttempts.toInt else "") +
      (if (success) " " + timeStr else "")
}

class Monitor (dbConfig: DatabaseConfig[JdbcProfile]) {
  import dbConfig.driver.api._

  val db = dbConfig.db

  case class Submit(id: Int, team: Int, problem: String, arrived: Int, success: Boolean, afterFreeze: Boolean)

  implicit val getSubmitResult = GetResult(r => Submit(r.nextInt(), r.nextInt(), r.nextString().toUpperCase,
    r.nextInt(), r.nextBoolean(), r.nextBoolean()))

  def getContestProblems(contest: Int) =
    sql"""select ID from Problems where Contest = $contest""".as[String]

  def getContestSubmits(contest: Int) =
    sql"""select Submits.ID, Team, Task, unix_timestamp(Submits.Arrived) - unix_timestamp(Contests.Start) as Arrived0,
           Submits.Passed = Submits.Taken and Submits.Passed > 0, Submits.Arrived > Contests.End from Contests, Submits where
           Contests.ID = $contest and
           Contests.ID = Submits.Contest and Submits.Finished and Submits.Compiled order by Arrived0""".as[Submit]

  def getContestTeams(contest: Int) =
    sql"""select Participants.LocalID, Schools.Name, Teams.Num, Teams.Name from Participants, Schools, Teams where
         Participants.Contest = $contest and Teams.ID = Participants.Team and Schools.ID = Teams.School
       """.as[(Int, String, Option[Int], Option[String])]

  def teamToTeam(v: (Int, String, Option[Int], Option[String])): Team =
    new Team(v._1, v._2 + v._3.map("#" + _.toString).getOrElse("") + v._4.map(": " + _).getOrElse(""))

  def getTimeAndSuccess(submits: Seq[Submit]): Option[Cell] = {
    val xs = submits.sortBy(_.arrived).zipWithIndex

    xs.find(_._1.success).map { m =>
      new Cell(m._1.problem, true, m._2, m._1.arrived)
    }.orElse(xs.lastOption.map { m =>
      new Cell(m._1.problem, false, m._2 + 1, 0)
    })
  }

  def calculateStatus(problems: Seq[String], teams: Seq[Team], submits: Seq[Submit]) = {
    import scala.collection.JavaConversions.asJavaIterable

    val rows = submits.groupBy(_.team).map {
      case (teamId, s0) =>
        val cells = s0.groupBy(_.problem).map {
          case (problemId, s1) =>
            getTimeAndSuccess(s1)
        }.flatten.toSeq

        teamId -> cells
    }
    val teamRows = teams.map { team =>
      val cells = rows.getOrElse(team.id, Seq())

      new Row(team, 0, cells.count(_.success),
        cells.filter(_.success).map(x => (x.time / 60) + x.failedAttempts * 20).sum,
        cells.map(x => x.problem -> x).toMap)

    }.toSeq.sortBy(x => (-x.solved, -x.time, x.team.name))


    new Status(problems, teamRows)
  }

  def getStatus(db: Database, contest: Int)(implicit ec: ExecutionContext): Future[(Status, Status)] = {
    db.run(getContestProblems(contest)).flatMap { problems =>
      db.run(getContestTeams(contest)).flatMap { teams =>
        val teams0 = teams.map(teamToTeam)
        db.run(getContestSubmits(contest)).map { submits =>
          (calculateStatus(problems, teams0, submits.filter(!_.afterFreeze)), calculateStatus(problems, teams0, submits))
        }
      }
    }
  }

  val contestMonitors = {
    import scala.collection.JavaConverters._
    import java.util.concurrent.ConcurrentHashMap
    new ConcurrentHashMap[Int, (Status, Status)]().asScala
  }

  val getContests = sql"select ID, SchoolMode from Contests".as[(Int, Boolean)]

  def getContestState(contest: Int, schoolMode: Boolean) =
    getStatus(db, contest)

  def rebuildMonitors: Future[Unit] =
    dbConfig.db.run(getContests).flatMap { contests =>
      println(contests)
      Future.sequence(contests.map(x => getContestState(x._1, x._2).map(y => (x._1, y))))
    }.map { statuses =>
      println(statuses)
      statuses.foreach {
        case (contestId, contestStatus) =>
          contestMonitors.put(contestId, contestStatus)
      }
    }

  var done = false

  val timer = new HashedWheelTimer()

  def rebuildMonitorsLoop: Unit =
    rebuildMonitors.onComplete { result =>
      println(result)
      if (!done)
        timer.newTimeout(new TimerTask {
          override def run(timeout: Timeout): Unit = rebuildMonitorsLoop
        }, 20, TimeUnit.SECONDS)
    }
}