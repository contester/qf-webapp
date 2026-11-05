package models

import actors.MonitorActor
import akka.actor.{ActorRef, ActorSystem}
import models.Foo.RankedRow
import org.stingray.contester.dbmodel.{Contest, Problem, Team}
import org.stingray.qf.actors.{ProblemStateActor, TeamStateActor}
import org.stingray.qf.models.TeamClient
import play.api.Configuration
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.MessagesApi
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import spire.math.Rational
import utils.Ask

import scala.concurrent.{ExecutionContext, Future}
import scala.math.Ordered.orderingToOrdered

case class ContestMonitor(contest: Contest, status: AnyStatus)

case class MonitorDisplayOptions(admin: Boolean, teamEditor: Boolean)

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

  def rowLess[S](a: (Team, S), b: (Team, S))(implicit ord: Ordering[S]): Boolean =
    ord.compare(a._2, b._2) match {
      case c if c < 0 => true
      case c if c > 0 => false
      case _ =>
        if ((!a._1.notRated) && b._1.notRated) true
        else if (a._1.notRated && !b._1.notRated) false
        else a._1.teamFullName < b._1.teamFullName
    }

  // Ranks for rows already sorted by rowLess: ties share a rank, the next rank skips; not rated is 0.
  def ranks[S](sorted: Seq[(Team, S)]): Seq[Int] = {
    var position = 0
    var last: Option[(Int, S)] = None
    sorted.map { case (team, score) =>
      val r =
        if (team.notRated) 0
        else {
          val shared = last.collect { case (lr, ls) if ls == score => lr }
          position += 1
          shared.getOrElse(position)
        }
      last = Some((r, score))
      r
    }
  }

  private case class SomeRow[ScoreType, CellType](team: Team, score: ScoreType,
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
    }

    val sorted = teamRows.sortWith((a, b) => rowLess((a.team, a.score), (b.team, b.score)))
    sorted.zip(ranks(sorted.map(r => (r.team, r.score)))).map {
      case (r, rank) => RankedRow(rank, r.team, r.score, r.cells)
    }
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
  case class Status(problems: Seq[String], rows: Seq[RankedRow[Score, ACMCell]]) extends AnyStatus {
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

  private def getCell(submits: Seq[Submit]): ACMCell =
    Submits.indexAndScoreGrouped(submits, ACMCell.empty, ACMScorer)._1

  private def cellFold(state: Score, cell: ACMCell) =
    if (cell.fullSolution)
        Score(state.solved + 1, state.penalty + cell.score, state.lastSubmitPenalty.max(cell.score))
    else
        state

  private def getScore(cells: Seq[ACMCell]) =
    cells.foldLeft(Score(0, 0, 0))(cellFold)

  def calculateStatus(problems: Seq[Problem], teams: Seq[Team], submits: Seq[Submit]): Status =
    Status(problems.map(_.id), Foo.groupAndRank(teams, submits, getCell, getScore))
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

class Monitor(dbConfig: DatabaseConfig[JdbcProfile], system: ActorSystem, configuration: Configuration,
              messagesApi: MessagesApi, statusActorModel: StatusActorModel, palevoSink: palevo.PalevoSink,
              liveConfig: live.LiveMonitorConfig) {
  private val db = dbConfig.db

  // Not started when the live monitor is on: it writes the static files and feeds palevo instead.
  private[this] val monitorActor: Option[ActorRef] =
    if (live.LiveWiring.startOldMonitor(liveConfig))
      Some(system.actorOf(MonitorActor.props(
        db, configuration.getOptional[String]("monitor.static_location"),
        statusActorModel.teamClient, statusActorModel.problemClient, messagesApi, palevoSink), "monitor-actor"))
    else None

  private implicit val monitorTimeout: akka.util.Timeout = {
    import scala.concurrent.duration._
    Duration(30, SECONDS)
  }

  def getMonitor(id: Int, overrideFreeze: Boolean)(implicit ec: ExecutionContext): Future[Option[ContestMonitor]] = {
    monitorActor match {
      case Some(actor) =>
        Ask[Option[StoredContestStatus]](actor, MonitorActor.Get(id))
          .map(_.map(_.monitor(overrideFreeze)))
      case None => Future.successful(None)
    }
  }
}