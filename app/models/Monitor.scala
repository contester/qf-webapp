package models

import java.util.concurrent.TimeUnit
import javax.inject.{Singleton, Inject}

import models.Foo.{RankedRow, MonitorRow}
import org.jboss.netty.util.{Timeout, TimerTask, HashedWheelTimer}
import play.api.db.slick.DatabaseConfigProvider
import play.api.inject.ApplicationLifecycle
import play.api.mvc.{Action, Controller}
import play.twirl.api.Html
import slick.backend.DatabaseConfig
import slick.dbio.DBIO
import slick.driver.JdbcProfile
import slick.jdbc.{JdbcBackend, GetResult}
import spire.math.{FixedPoint, FixedScale, Rational}
import views.html

import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.{Promise, ExecutionContext, Future}

trait AnyStatus {
  def problems: Seq[String]
}

object Foo {
  trait MonitorRow[ScoreType, CellType] {
    def team: LocalTeam
    def score: ScoreType
    def cells: Map[String, CellType]
  }

  case class RankedRow[ScoreType, CellType](rank: Int, team: LocalTeam, score: ScoreType, cells: Map[String, CellType]) {
    def rankStr =
      if (rank == 0) "*" else rank.toString
  }

  type RankState[ScoreType, CellType] = (Seq[RankedRow[ScoreType, CellType]], Int)

  def pullRank[ScoreType, CellType](state: RankState[ScoreType, CellType], next: MonitorRow[ScoreType, CellType]): RankState[ScoreType, CellType] = {
    val position = state._2

    val nextR =
      if (next.team.notRated)
        (0, position)
      else state._1.lastOption.map { lastRanked =>
        if (lastRanked.score == next.score)
          (lastRanked.rank, position + 1)
        else
          (position + 1, position + 1)
      }.getOrElse(1, 1)

    (state._1 :+ new RankedRow(nextR._1, next.team, next.score, next.cells), nextR._2)
  }


  def rank[ScoreType, CellType](rows: Seq[MonitorRow[ScoreType, CellType]]): Seq[RankedRow[ScoreType, CellType]] =
    rows.foldLeft((Seq[RankedRow[ScoreType, CellType]](), 0))(pullRank)._1

  case class SomeRow[ScoreType, CellType](team: LocalTeam, score: ScoreType,
                                                                cells: Map[String, CellType]) extends MonitorRow[ScoreType, CellType]

  def groupAndRank[ScoreType, CellType](teams: Seq[LocalTeam], submits: Seq[Submit],
                                                               getCell: (Seq[Submit]) => CellType,
                                                               getScore: (Seq[CellType]) => ScoreType)(implicit ord: Ordering[ScoreType]): Seq[RankedRow[ScoreType, CellType]] = {
    val rows = submits.groupBy(_.teamId).map {
      case (teamId, s0) =>
        val cells: Map[String, CellType] = s0.groupBy(_.problem).map {
          case (problemId, s1) =>
            problemId -> getCell(s1)
        }

        teamId -> cells
    }.toMap

    val teamRows = teams.map { team =>
      val cells = rows.getOrElse(team.localId, Seq()).toMap
      val score = getScore(cells.values.toSeq)

      SomeRow(team, score, cells)
    }.sortBy(_.score)

    Foo.rank(teamRows)
  }


}

object School {
  case class Status(problems: Seq[String], rows: Seq[RankedRow[Rational, SchoolCell]]) extends AnyStatus

  object Cell {
    def apply(submits: Seq[Submit]): SchoolCell =
      Submits.indexSubmits(submits, SchoolCell.empty).lastOption.map(_.score).getOrElse(SchoolCell.empty)
  }

  object Score {
    def apply(cells: Seq[SchoolCell]): Rational = {
      //import spire.implicits._
      cells.map(_.score).foldLeft(Rational(0))(_+_)
    }
  }

    def calculateStatus(problems: Seq[Problem], teams: Seq[LocalTeam], submits: Seq[Submit]): Status = {
      implicit val ord = Ordering[Rational].reverse
      Status(problems.map(_.id), Foo.groupAndRank(teams, submits, Cell(_), Score(_)))
    }
}

object ACM {
  case class Status(val problems: Seq[String], val rows: Seq[RankedRow[Score, ACMCell]]) extends AnyStatus

  case class Score(val solved: Int, val penalty: Int) extends Ordered[Score] {
    override def compare(that: Score): Int = {
      val r = that.solved.compare(solved)
      if (r == 0) {
        penalty.compare(that.penalty)
      } else r
    }
  }

  def getCell(submits: Seq[Submit]): ACMCell =
    Submits.indexSubmits(submits, ACMCell.empty).lastOption.map(_.score).getOrElse(ACMCell.empty)

  def cellFold(state: Score, cell: ACMCell) =
    if (cell.fullSolution)
        Score(state.solved + 1, state.penalty + cell.score)
    else
        state

  def getScore(cells: Seq[ACMCell]) =
    cells.foldLeft(Score(0, 0))(cellFold)

  def calculateStatus(problems: Seq[Problem], teams: Seq[LocalTeam], submits: Seq[Submit]) =
    Status(problems.map(_.id), Foo.groupAndRank(teams, submits, getCell(_), getScore(_)))
}

@Singleton
class Monitor @Inject() (dbConfigProvider: DatabaseConfigProvider, lifecycle: ApplicationLifecycle) {
  val dbConfig = dbConfigProvider.get[JdbcProfile]
  import dbConfig.driver.api._

  lifecycle.addStopHook { () =>
    done = true
    Future.successful()
  }

  val db = dbConfig.db

  implicit val getProblem = GetResult(r => Problem(r.nextString().toUpperCase, r.nextString(), r.nextInt(), r.nextInt()))

  def getContestProblems(contest: Int) =
    sql"""select ID, Name, Tests, Rating from Problems where Contest = $contest order by ID""".as[Problem]

  implicit val getLocalTeam = GetResult(r =>
    LocalTeam(r.nextInt(), r.nextString(), r.nextIntOption(), r.nextString(), r.nextBoolean()))


  def getContestTeams(contest: Int) =
    sql"""select Participants.LocalID, Schools.Name, Teams.Num, Teams.Name, Participants.NotRated from Participants, Schools, Teams where
         Participants.Contest = $contest and Teams.ID = Participants.Team and Schools.ID = Teams.School
       """.as[LocalTeam]

  def getStatus(db: Database, contest: Int, schoolMode: Boolean)(implicit ec: ExecutionContext): Future[(AnyStatus, AnyStatus)] = {
    db.run(getContestProblems(contest)).flatMap { problems =>
      db.run(getContestTeams(contest)).flatMap { teams =>
        db.run(Submits.getContestSubmits(contest)).map { submits =>
          val sub0 = submits.filter(!_.afterFreeze)
          if (schoolMode)
            (School.calculateStatus(problems, teams, sub0), School.calculateStatus(problems, teams, submits))
          else
            (ACM.calculateStatus(problems, teams, sub0),
              ACM.calculateStatus(problems, teams, submits))
        }
      }
    }
  }

  val contestMonitorsFu = {
    import scala.collection.JavaConverters._
    import java.util.concurrent.ConcurrentHashMap
    new ConcurrentHashMap[Int, Promise[(AnyStatus, AnyStatus)]]().asScala
  }

  def getMonitor(id: Int): Future[(AnyStatus, AnyStatus)] = newOrUpdatedPromise(id).future

  private def newOrUpdatedPromise(id: Int) = {
    val np = Promise[(AnyStatus, AnyStatus)]
    contestMonitorsFu.putIfAbsent(id, np).getOrElse(np)
  }

  import com.github.nscala_time.time.Imports._

  implicit val convertContests = GetResult(r => Contest(r.nextInt(), r.nextString(), r.nextBoolean(),
    new DateTime(r.nextTimestamp()), new DateTime(r.nextTimestamp()), new DateTime(r.nextTimestamp()),
    new DateTime(r.nextTimestamp())))


  val getContests =
    sql"""select ID, Name, SchoolMode, Start, End, Finish, Expose from Contests""".as[Contest]

  def getContestState(contest: Int, schoolMode: Boolean) =
    getStatus(db, contest, schoolMode)

  def rebuildMonitors: Future[Unit] =
    dbConfig.db.run(getContests).flatMap { contests =>
      Future.sequence(contests.map(x => getContestState(x.id, x.schoolMode).map(y => (x.id, y))))
    }.map { statuses =>
      statuses.foreach {
        case (contestId, contestStatus) =>
          val m = newOrUpdatedPromise(contestId)
            if (m.isCompleted) {
              contestMonitorsFu.put(contestId, Promise.successful(contestStatus))
            } else {
              m.success(contestStatus)
            }
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

  rebuildMonitorsLoop
}