package models

import javax.inject.{Inject, Singleton}
import actors.MonitorActor
import akka.actor.ActorSystem
import models.Foo.RankedRow
import org.stingray.qf.actors.{ProblemStateActor, TeamStateActor}
import org.stingray.qf.models.TeamClient
import play.api.Configuration
import play.api.db.slick.DatabaseConfigProvider
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import spire.math.Rational
import utils.Ask

import scala.concurrent.{ExecutionContext, Future}

case class ContestMonitor(contest: Contest, status: AnyStatus)

trait AnyStatus {
  def problems: Seq[String]
  def anyRows: Seq[AnyRankedRow]

  def solvedProblems: Map[String, Int] =
    anyRows.flatMap { row =>
      row.anyCells.filter(_._2.fullSolution).keys
    }.groupBy(x => x).mapValues(_.size)
}

trait AnyRankedRow {
  def rank: Int
  def team: Team

  def anyScore: Any
  def anyCells: Map[String, ProblemCell]

  def rankStr =
    if (rank == 0) "*" else rank.toString
}

object Foo {
  trait MonitorRow[ScoreType, CellType] {
    def team: Team
    def score: ScoreType
    def cells: Map[String, CellType]
  }

  case class RankedRow[ScoreType, CellType <: ProblemCell](rank: Int, team: Team, score: ScoreType, cells: Map[String, CellType]) extends AnyRankedRow {
    override def anyScore: Any = score

    override def anyCells: Map[String, ProblemCell] = cells
  }

  type RankState[ScoreType, CellType <: ProblemCell] = (Seq[RankedRow[ScoreType, CellType]], Int)

  def pullRank[ScoreType, CellType <: ProblemCell](state: RankState[ScoreType, CellType], next: MonitorRow[ScoreType, CellType]): RankState[ScoreType, CellType] = {
    val position = state._2

    val nextR =
      if (next.team.notRated)
        (0, position)
      else state._1.lastOption.map { lastRanked =>
        if (lastRanked.score == next.score)
          (lastRanked.rank, position + 1)
        else
          (position + 1, position + 1)
      }.getOrElse((1, 1))

    (state._1 :+ RankedRow(nextR._1, next.team, next.score, next.cells), nextR._2)
  }


  def rank[ScoreType, CellType <: ProblemCell](rows: Seq[MonitorRow[ScoreType, CellType]]): Seq[RankedRow[ScoreType, CellType]] =
    rows.foldLeft((Seq[RankedRow[ScoreType, CellType]](), 0))(pullRank)._1

  case class SomeRow[ScoreType, CellType](team: Team, score: ScoreType,
                                                                cells: Map[String, CellType]) extends MonitorRow[ScoreType, CellType]

  def groupAndRank[ScoreType, CellType <: ProblemCell](teams: Seq[Team], submits: Seq[Submit],
                                                               getCell: (Seq[Submit]) => CellType,
                                                               getScore: (Seq[CellType]) => ScoreType)(implicit ord: Ordering[ScoreType]): Seq[RankedRow[ScoreType, CellType]] = {
    val rows = submits.groupBy(_.submitId.teamId).map {
      case (teamId, s0) =>
        val cells: Map[String, CellType] = s0.groupBy(_.submitId.problem.id).map {
          case (problemId, s1) =>
            problemId -> getCell(s1)
        }

        teamId -> cells
    }

    val teamRows = teams.map { team =>
      val cells = rows.getOrElse(team.id, Seq()).toMap
      val score = getScore(cells.values.toSeq)

      SomeRow(team, score, cells)
    }.sortBy(_.score)

    Foo.rank(teamRows)
  }


}

object MonitorSchool {
  case class Status(problems: Seq[String], rows: Seq[RankedRow[Rational, SchoolCell]]) extends AnyStatus {
    override def anyRows: Seq[AnyRankedRow] = rows
  }

  object Cell {
    def apply(submits: Seq[Submit]): SchoolCell =
      Submits.indexAndScoreGrouped(submits, SchoolCell.empty, SchoolScorer)._1
  }

  object Score {
    def apply(cells: Seq[SchoolCell]): Rational = {
      //import spire.implicits._
      cells.map(_.score).foldLeft(Rational(0))(_+_)
    }
  }

    def calculateStatus(problems: Seq[Problem], teams: Seq[Team], submits: Seq[Submit]): Status = {
      implicit val ord = Ordering[Rational].reverse
      Status(problems.map(_.id), Foo.groupAndRank(teams, submits, Cell(_), Score(_)))
    }
}

object ACM {
  case class Status(val problems: Seq[String], val rows: Seq[RankedRow[Score, ACMCell]]) extends AnyStatus {
    override def anyRows: Seq[AnyRankedRow] = rows
  }

  case class Score(solved: Int, penalty: Int, lastSubmitPenalty: Int) extends Ordered[Score] {
    override def compare(that: Score): Int = {
      val r = that.solved.compare(solved)
      if (r == 0) {
        val r2 = penalty.compare(that.penalty)
        if (r2 == 0) {
          lastSubmitPenalty.compare(that.lastSubmitPenalty)
        } else r2
      } else r
    }
  }

  def getCell(submits: Seq[Submit]): ACMCell =
    Submits.indexAndScoreGrouped(submits, ACMCell.empty, ACMScorer)._1

  def cellFold(state: Score, cell: ACMCell) =
    if (cell.fullSolution)
        Score(state.solved + 1, state.penalty + cell.score, state.lastSubmitPenalty.max(cell.score))
    else
        state

  def getScore(cells: Seq[ACMCell]) =
    cells.foldLeft(Score(0, 0, 0))(cellFold)

  def calculateStatus(problems: Seq[Problem], teams: Seq[Team], submits: Seq[Submit]) =
    Status(problems.map(_.id), Foo.groupAndRank(teams, submits, getCell(_), getScore(_)))
}

case class StoredContestStatus(contest: Contest, frozen: AnyStatus, exposed: AnyStatus) {
  private[this] def status(overrideFreeze: Boolean) =
    if (contest.frozen && !overrideFreeze)
      frozen
    else
      exposed

  def monitor(overrideFreeze: Boolean) =
    ContestMonitor(contest, status(overrideFreeze))
}

class Monitor(dbConfig: DatabaseConfig[JdbcProfile], system: ActorSystem, configuration: Configuration) {
  private val db = dbConfig.db
  private val teamStateActor = system.actorOf(TeamStateActor.props(db), "team-state-actor")
  val teamClient = new TeamClient(teamStateActor)
  private val problemStateActor = system.actorOf(ProblemStateActor.props(db), "problem-state-actor")
  val problemClient = new ProblemClient(problemStateActor)

  private[this] val monitorActor = system.actorOf(MonitorActor.props(
    db, configuration.getString("monitor.static_location"),
    teamClient, problemClient), "monitor-actor")

  private implicit val monitorTimeout: akka.util.Timeout = {
    import scala.concurrent.duration._
    Duration(30, SECONDS)
  }

  def getMonitor(id: Int, overrideFreeze: Boolean)(implicit ec: ExecutionContext): Future[Option[ContestMonitor]] = {
    Ask[Option[StoredContestStatus]](monitorActor, MonitorActor.Get(id))
      .map(_.map(_.monitor(overrideFreeze)))
  }
}