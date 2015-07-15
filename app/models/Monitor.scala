package models

import java.util.concurrent.TimeUnit
import javax.inject.Inject

import models.Foo.{RankedRow, MonitorRow}
import org.jboss.netty.util.{Timeout, TimerTask, HashedWheelTimer}
import play.api.db.slick.DatabaseConfigProvider
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


  def rank[ScoreType <: Ordered[ScoreType], CellType](rows: Seq[MonitorRow[ScoreType, CellType]]): Seq[RankedRow[ScoreType, CellType]] =
    rows.foldLeft((Seq[RankedRow[ScoreType, CellType]](), 0))(pullRank)._1
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

  case class Row(team: LocalTeam, score: Rational, cells: Map[String, SchoolCell]) extends MonitorRow[Rational, SchoolCell]

    def calculateStatus(problems: Seq[(String, Int)], teams: Seq[LocalTeam], submits: Seq[Submit]): Status = {
      import scala.collection.JavaConversions.asJavaIterable

      val rows = submits.groupBy(_.teamId).map {
        case (teamId, s0) =>
          val cells: Map[String, SchoolCell] = s0.groupBy(_.problem).map {
            case (problemId, s1) =>
              problemId -> Cell(s1)
          }

          teamId -> cells
      }.toMap

      val teamRows = teams.map { team =>
        val cells = rows.getOrElse(team.localId, Seq()).toMap
        val score = Score(cells.values.toSeq)

        new Row(team, score, cells)
      }.sortBy(_.score).reverse

      val rankedRows = Foo.rank(teamRows)

      new Status(problems.map(_._1), rankedRows)
    }

}

object ACM {
  case class Status(val problems: Seq[String], val rows: Seq[RankedRow[Score, Cell]]) extends AnyStatus

  case class Row(val team: LocalTeam, val score: Score, val cells: Map[String, Cell]) extends MonitorRow[Score, Cell]

  trait Cell {
    def success: Boolean = false
    def toShort: String
    def timeStr: String = ""
  }

  case class Success(val failedAttempts: Seq[Int], val time: Int) extends Cell {
    override def success: Boolean = true

    override def toShort: String = "+" + (if (!failedAttempts.isEmpty) failedAttempts.length.toString else "")
    override def timeStr = "%02d:%02d".format(time / 3600, (time / 60) % 60)
  }

  case class Failure(val failedAttempts: Seq[Int]) extends Cell {
    override def toShort: String = if (!failedAttempts.isEmpty) {"-" + failedAttempts.length.toString} else ""
  }

  case class Score(val solved: Int, val penalty: Int) extends Ordered[Score] {
    override def compare(that: Score): Int = {
      val r = that.solved.compare(solved)
      if (r == 0) {
        penalty.compare(that.penalty)
      } else r
    }

    override def equals(obj: scala.Any): Boolean =
      obj match {
        case other: Score => solved == other.solved && penalty == other.penalty
        case _ => super.equals(obj)
      }

  }

  def submitFold(state: Cell, submit: Submit): Cell =
    state match {
      case Failure(failedAttempts) =>
        if (submit.success)
          new Success(failedAttempts.filter(_ < submit.arrivedSeconds), submit.arrivedSeconds)
        else
          new Failure(failedAttempts :+ submit.arrivedSeconds)
      case Success(failedAttempts, arrived) if (arrived > submit.arrivedSeconds) =>
        if (submit.success)
          new Success(failedAttempts.filter(_ < submit.arrivedSeconds), submit.arrivedSeconds)
        else
          new Success(failedAttempts :+ submit.arrivedSeconds, arrived)
      case x: Cell =>
        x
    }

  def getCell(submits: Seq[Submit]): Cell =
    submits.foldLeft((new Failure(Nil)).asInstanceOf[Cell])(submitFold)

  def cellFold(state: Score, cell: Cell) =
    cell match {
      case Success(failedAttempts, time) =>
        new Score(state.solved + 1, state.penalty + (time / 60) + failedAttempts.length * 20)
      case _ =>
        state
    }

  def getScore(cells: Seq[Cell]) =
    cells.foldLeft(new Score(0, 0))(cellFold)

  def calculateStatus(problems: Seq[String], teams: Seq[LocalTeam], submits: Seq[Submit]) = {
    import scala.collection.JavaConversions.asJavaIterable

    val rows = submits.groupBy(_.teamId).map {
      case (teamId, s0) =>
      val cells: Map[String, Cell] = s0.groupBy(_.problem).map {
                  case (problemId, s1) =>
                      problemId -> getCell(s1)
                }

              teamId -> cells
          }

    val teamRows = teams.map { team =>
      val cells = rows.getOrElse(team.localId, Seq()).toMap
      val score = getScore(cells.values.toSeq)

      new Row(team, score, cells)
    }.sortBy(_.score)

    val rankedRows = Foo.rank(teamRows)

    new Status(problems, rankedRows)
  }
}

class Monitor (dbConfig: DatabaseConfig[JdbcProfile]) {
  import dbConfig.driver.api._

  val db = dbConfig.db

  def getContestProblems(contest: Int) =
    sql"""select ID, Rating from Problems where Contest = $contest""".as[(String, Int)]

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
            (ACM.calculateStatus(problems.map(_._1), teams, sub0),
              ACM.calculateStatus(problems.map(_._1), teams, submits))
        }
      }
    }
  }

  val contestMonitors = {
    import scala.collection.JavaConverters._
    import java.util.concurrent.ConcurrentHashMap
    new ConcurrentHashMap[Int, (AnyStatus, AnyStatus)]().asScala
  }

  val contestMonitorsFu = {
    import scala.collection.JavaConverters._
    import java.util.concurrent.ConcurrentHashMap
    new ConcurrentHashMap[Int, Promise[(AnyStatus, AnyStatus)]]().asScala
  }


  val getContests = sql"select ID, SchoolMode from Contests".as[(Int, Boolean)]

  def getContestState(contest: Int, schoolMode: Boolean) =
    getStatus(db, contest, schoolMode)

  def rebuildMonitors: Future[Unit] =
    dbConfig.db.run(getContests).flatMap { contests =>
      Future.sequence(contests.map(x => getContestState(x._1, x._2).map(y => (x._1, y))))
    }.map { statuses =>
      statuses.foreach {
        case (contestId, contestStatus) =>
          contestMonitors.put(contestId, contestStatus)
          val np = Promise[(AnyStatus, AnyStatus)]
          val m = contestMonitorsFu.putIfAbsent(contestId,np).getOrElse(np)
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
      if (!done)
        timer.newTimeout(new TimerTask {
          override def run(timeout: Timeout): Unit = rebuildMonitorsLoop
        }, 20, TimeUnit.SECONDS)
    }
}