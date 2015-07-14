package models

import java.util.concurrent.TimeUnit
import javax.inject.Inject

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

case class Submit(id: Int, team: Int, problem: String, arrived: Int, passed: Int, taken: Int, afterFreeze: Boolean) {
  def success = taken > 0 && passed == taken
}

case class Team(val id: Int, val name: String, val notRated: Boolean)

trait AnyStatus {
  def problems: Seq[String]
}

/*
case class RankedRow[A <: AScore, C <: ACell](rank: Int, team: Team, score: A, cells: Map[String, C]) {
  def rankStr =
    if (rank == 0) "*" else rank.toString
}
*/

object School {
  implicit val scale = FixedScale(100)
  /*
  case class Score(val score: Rational) extends Ordered[Score] {
    override def compare(that: Score): Int =
      that.score.compare(score)

    def toShort = f"$score%.2f"
    override def equals(obj: scala.Any): Boolean =
      obj match {
        case other: Score => score == other.score
        case _ => super.equals(obj)
      }
  }*/

  case class Status(problems: Seq[String], rows: Seq[RankedRow]) extends AnyStatus
  case class Cell(val score: Rational, val full: Boolean) {
    def toShort = rationalToScoreStr(score)
  }

  object Cell {
    def calculate(base: Int, fraction: Rational, failedAttempts: Int): Rational = {
      val tops = {
        val x = base - failedAttempts
        if (x < 20)
          20
        else
          x
      }
      if (fraction == 1)
        tops
      else {
        val x = tops * 2 * fraction / 3
        if (x < 5)
          0.0
        else
          x
      }
    }

    def submitFraction(s: Submit): Rational =
      if (s.taken == 0)
        Rational(0)
      else
        Rational(s.passed,s.taken)

    def apply(submits: Seq[Submit]): Cell = {
      val maxScore = submits.sortBy(_.arrived).zipWithIndex.map(x => calculate(30,submitFraction(x._1), x._2)).max
      new Cell(maxScore, submits.exists(submitFraction(_) == 1))
    }
  }


  object Score {
    def apply(cells: Seq[Cell]): Rational = {
      import spire.implicits._
      cells.map(_.score).foldLeft(Rational(0))(_+_)
    }
  }

  def rationalToScoreStr(r: Rational) =
    if (r.isWhole()) r
    else FixedPoint(r).toString(scale)

  case class Row(team: Team, score: Rational, cells: Map[String, Cell])
  case class RankedRow(rank: Int, team: Team, score: Rational, cells: Map[String, Cell]) {

    def rankStr =
      if (rank == 0) "*" else rank.toString

    def scoreStr = rationalToScoreStr(score)
  }

    def calculateStatus(problems: Seq[(String, Int)], teams: Seq[Team], submits: Seq[Submit]): Status = {
      import scala.collection.JavaConversions.asJavaIterable

      val rows = submits.groupBy(_.team).map {
        case (teamId, s0) =>
          val cells: Map[String, Cell] = s0.groupBy(_.problem).map {
            case (problemId, s1) =>
              problemId -> Cell(s1)
          }

          teamId -> cells
      }.toMap

      val teamRows = teams.map { team =>
        val cells = rows.getOrElse(team.id, Seq()).toMap
        val score = Score(cells.values.toSeq)

        new Row(team, score, cells)
      }.sortBy(_.score).reverse

      val rankedRows = teamRows.foldLeft((Seq[RankedRow](), 0))(pullRank)._1

      new Status(problems.map(_._1), rankedRows)
    }

  type RankState = (Seq[RankedRow], Int)

  def pullRank(state: RankState, next: Row): RankState = {
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

}

object ACM {
  case class Status(val problems: Seq[String], val rows: Seq[RankedRow]) extends AnyStatus

  case class RankedRow(rank: Int, team: Team, score: Score, cells: Map[String, Cell]) {
    def rankStr =
      if (rank == 0) "*" else rank.toString

  }
  case class Row(val team: Team, val score: Score, val cells: Map[String, Cell])

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
          new Success(failedAttempts.filter(_ < submit.arrived), submit.arrived)
        else
          new Failure(failedAttempts :+ submit.arrived)
      case Success(failedAttempts, arrived) if (arrived > submit.arrived) =>
        if (submit.success)
          new Success(failedAttempts.filter(_ < submit.arrived), submit.arrived)
        else
          new Success(failedAttempts :+ submit.arrived, arrived)
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

  type RankState = (Seq[RankedRow], Int)

  def pullRank(state: RankState, next: Row): RankState = {
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

  def calculateStatus(problems: Seq[String], teams: Seq[Team], submits: Seq[Submit]) = {
    import scala.collection.JavaConversions.asJavaIterable

    val rows = submits.groupBy(_.team).map {
      case (teamId, s0) =>
      val cells: Map[String, Cell] = s0.groupBy(_.problem).map {
                  case (problemId, s1) =>
                      problemId -> getCell(s1)
                }

              teamId -> cells
          }

    val teamRows = teams.map { team =>
      val cells = rows.getOrElse(team.id, Seq()).toMap
      val score = getScore(cells.values.toSeq)

      new Row(team, score, cells)
    }.sortBy(_.score)

    val rankedRows = teamRows.foldLeft((Seq[RankedRow](), 0))(pullRank)._1

    new Status(problems, rankedRows)
  }
}

class Monitor (dbConfig: DatabaseConfig[JdbcProfile]) {
  import dbConfig.driver.api._

  val db = dbConfig.db

  implicit val getSubmitResult = GetResult(r => Submit(r.nextInt(), r.nextInt(), r.nextString().toUpperCase,
    r.nextInt(), r.nextInt(), r.nextInt(), r.nextBoolean()))

  def getContestProblems(contest: Int) =
    sql"""select ID, Rating from Problems where Contest = $contest""".as[(String, Int)]

  def getContestSubmits(contest: Int) =
    sql"""select Submits.ID, Team, Task, unix_timestamp(Submits.Arrived) - unix_timestamp(Contests.Start) as Arrived0,
           Submits.Passed, Submits.Taken, Submits.Arrived > Contests.Finish from Contests, Submits where
           Contests.ID = $contest and Submits.Arrived < Contests.End and Submits.Arrived >= Contests.Start and
           Contests.ID = Submits.Contest and Submits.Finished and Submits.Compiled order by Arrived0""".as[Submit]

  def getContestTeams(contest: Int) =
    sql"""select Participants.LocalID, Schools.Name, Teams.Num, Teams.Name, Participants.NotRated from Participants, Schools, Teams where
         Participants.Contest = $contest and Teams.ID = Participants.Team and Schools.ID = Teams.School
       """.as[(Int, String, Option[Int], Option[String], Boolean)]

  def teamToTeam(v: (Int, String, Option[Int], Option[String], Boolean)): Team =
    new Team(v._1, v._2 + v._3.map("#" + _.toString).getOrElse("") + v._4.map(": " + _).getOrElse(""), v._5)


  def getStatus(db: Database, contest: Int, schoolMode: Boolean)(implicit ec: ExecutionContext): Future[(AnyStatus, AnyStatus)] = {
    db.run(getContestProblems(contest)).flatMap { problems =>
      db.run(getContestTeams(contest)).flatMap { teams =>
        val teams0 = teams.map(teamToTeam)
        db.run(getContestSubmits(contest)).map { submits =>
          val sub0 = submits.filter(!_.afterFreeze)
          if (schoolMode)
            (School.calculateStatus(problems, teams0, sub0), School.calculateStatus(problems, teams0, submits))
          else
            (ACM.calculateStatus(problems.map(_._1), teams0, sub0),
              ACM.calculateStatus(problems.map(_._1), teams0, submits))
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