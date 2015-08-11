package models

import java.util.concurrent.TimeUnit
import javax.inject.{Singleton, Inject}

import actors.{MonitorActor, StatusActor}
import akka.actor.ActorSystem
import models.Foo.{RankedRow, MonitorRow}
import org.jboss.netty.util.{Timeout, TimerTask, HashedWheelTimer}
import play.api.Logger
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

case class ContestMonitor(contest: Contest, status: AnyStatus)

trait AnyStatus {
  def problems: Seq[String]
  def anyRows: Seq[AnyRankedRow]
}

trait AnyRankedRow {
  def rank: Int
  def team: LocalTeam

  def anyScore: Any
  def anyCells: Map[String, Any]

  def rankStr =
    if (rank == 0) "*" else rank.toString
}

object Foo {
  trait MonitorRow[ScoreType, CellType] {
    def team: LocalTeam
    def score: ScoreType
    def cells: Map[String, CellType]
  }

  case class RankedRow[ScoreType, CellType](rank: Int, team: LocalTeam, score: ScoreType, cells: Map[String, CellType]) extends AnyRankedRow {
    override def anyScore: Any = score

    override def anyCells: Map[String, Any] = cells
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
    val rows = submits.groupBy(_.submitId.teamId).map {
      case (teamId, s0) =>
        val cells: Map[String, CellType] = s0.groupBy(_.submitId.problem.id).map {
          case (problemId, s1) =>
            problemId -> getCell(s1)
        }

        teamId -> cells
    }

    val teamRows = teams.map { team =>
      val cells = rows.getOrElse(team.localId, Seq()).toMap
      val score = getScore(cells.values.toSeq)

      SomeRow(team, score, cells)
    }.sortBy(_.score)

    Foo.rank(teamRows)
  }


}

object School {
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

    def calculateStatus(problems: Seq[Problem], teams: Seq[LocalTeam], submits: Seq[Submit]): Status = {
      implicit val ord = Ordering[Rational].reverse
      Status(problems.map(_.id), Foo.groupAndRank(teams, submits, Cell(_), Score(_)))
    }
}

object ACM {
  case class Status(val problems: Seq[String], val rows: Seq[RankedRow[Score, ACMCell]]) extends AnyStatus {
    override def anyRows: Seq[AnyRankedRow] = rows
  }

  case class Score(val solved: Int, val penalty: Int) extends Ordered[Score] {
    override def compare(that: Score): Int = {
      val r = that.solved.compare(solved)
      if (r == 0) {
        penalty.compare(that.penalty)
      } else r
    }
  }

  def getCell(submits: Seq[Submit]): ACMCell =
    Submits.indexAndScoreGrouped(submits, ACMCell.empty, ACMScorer)._1

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

case class StoredContestStatus(contest: Contest, frozen: AnyStatus, exposed: AnyStatus) {
  private[this] def status(overrideFreeze: Boolean) =
    if (contest.frozen && !overrideFreeze)
      frozen
    else
      exposed

  def monitor(overrideFreeze: Boolean) =
    ContestMonitor(contest, status(overrideFreeze))
}

@Singleton
class Monitor @Inject() (dbConfigProvider: DatabaseConfigProvider, system: ActorSystem, lifecycle: ApplicationLifecycle) {
  val dbConfig = dbConfigProvider.get[JdbcProfile]
  import dbConfig.driver.api._

  val db = dbConfig.db

  val monitorActor = system.actorOf(MonitorActor.props(db), "monitor-actor")

  def getMonitor(id: Int, overrideFreeze: Boolean): Future[Option[ContestMonitor]] = {
    import akka.pattern.ask
    import scala.concurrent.duration._

    monitorActor.ask(MonitorActor.Get(id))(30 seconds).mapTo[Option[StoredContestStatus]]
      .map(_.map(_.monitor(overrideFreeze)))
  }
}