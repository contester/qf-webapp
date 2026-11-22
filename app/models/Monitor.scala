package models

import akka.actor.ActorSystem
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{Contest, Disqualified, NotRated, SlickModel, Team}
import slick.basic.DatabaseConfig
import slick.jdbc.{JdbcBackend, JdbcProfile}

import scala.concurrent.{ExecutionContext, Future}

/**
  * One team's result on one problem. `best`: the most points of the team's finished, compiled submits whose points
  * are set (None when there is none); `attempts`: all the team's submits for the problem, in flight ones included;
  * `full`: best is the problem's maximum, and that maximum is above 0.
  */
case class PointsCell(best: Option[Double], attempts: Int, full: Boolean)

object PointsCell {
  val empty: PointsCell = PointsCell(None, 0, full = false)
}

object PointsScorer {
  /**
    * The cell of one team's submits for one problem. A submit's points are those of its current testing: a rejudge
    * writes new points into the same submits row (old and new are never added), and while a testing is in flight
    * the points are NULL, so such a submit counts as an attempt but not for the best.
    */
  def cell(submits: Seq[Submit], maxPoints: Double): PointsCell = {
    val scored = submits.filter(s => s.finished && s.compiled).flatMap(_.points)
    val best = if (scored.isEmpty) None else Some(scored.max)
    PointsCell(best, submits.size, full = maxPoints > 0 && best.contains(maxPoints))
  }
}

/** A team and its cells, one per problem in the board's problem order. */
case class TeamRow(team: Team, cells: Seq[PointsCell])

/** A row of the board as shown: the place (None for an unrated team), the team, its cells and total. */
case class RankedRow(place: Option[Int], team: Team, cells: Seq[PointsCell], total: Double)

object Board {
  /** The sum of the cells' best (a cell without one counts 0), added exactly so that equal totals compare equal. */
  def total(cells: Seq[PointsCell]): Double =
    cells.flatMap(_.best).map(BigDecimal(_)).sum.toDouble

  /**
    * Ranked by total descending; equal totals share a place and the next place skips (1, 1, 3); equal totals are
    * ordered by team number. Not rated teams follow the ranked ones, ordered the same way, with no place and without
    * affecting anyone's place. Disqualified teams are left out.
    */
  def rank(rows: Seq[TeamRow]): Seq[RankedRow] = {
    val withTotals = rows.filterNot(_.team.unrated.contains(Disqualified))
      .map(r => RankedRow(None, r.team, r.cells, total(r.cells)))
      .sortBy(r => (-r.total, r.team.number))
    val (unrated, rated) = withTotals.partition(_.team.unrated.contains(NotRated))
    val placed = rated.zipWithIndex.foldLeft(Vector.empty[RankedRow]) { case (acc, (r, i)) =>
      val place = acc.lastOption.filter(_.total == r.total).flatMap(_.place).getOrElse(i + 1)
      acc :+ r.copy(place = Some(place))
    }
    placed ++ unrated
  }
}

/** A problem column of the board. */
case class ProblemColumn(id: String, name: String, maxPoints: Double)

/** The points board of a round: its problems and its ranked rows. */
case class MonitorPoints(problems: Seq[ProblemColumn], rows: Seq[RankedRow])

object MonitorPoints {
  /** The board from the round's problems, participants and submits; submits of other teams are ignored. */
  def build(problems: Seq[ProblemColumn], teams: Seq[Team], submits: Seq[Submit]): MonitorPoints = {
    val byTeamProblem = submits.groupBy(s => (s.submitId.teamId, s.submitId.problem.id))
    val rows = teams.map { t =>
      TeamRow(t, problems.map(p => PointsScorer.cell(byTeamProblem.getOrElse((t.participantId, p.id), Nil), p.maxPoints)))
    }
    MonitorPoints(problems, Board.rank(rows))
  }

  /** Loads the board of a round from the database: three queries, problems ordered by id. */
  def load(db: JdbcBackend#DatabaseDef, roundId: Int)(implicit ec: ExecutionContext): Future[MonitorPoints] = {
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    val problems = db.run(SlickModel.problems.filter(_.contestID === roundId).sortBy(_.id).result)
      .map(_.map(p => ProblemColumn(p.id, p.name, p.maxPoints)))
    val teams = Rounds.teamsOfRound(db, roundId)
    val submits = db.run(Submits.getContestSubmits(roundId)).map(_.map(Submits.upliftSub))
    for {
      p <- problems
      t <- teams
      s <- submits
    } yield build(p, t, s)
  }
}

/** Who looks at a scoreboard. */
sealed trait Viewer
case object AdminViewer extends Viewer
case class TeamViewer(participantId: Int, roundId: Int) extends Viewer
case object Anonymous extends Viewer

object Visibility {
  /** A round is over once it is finished or its end has come (ends_at inclusive). */
  def isOver(round: Contest, now: DateTime): Boolean = round.isFinished || !now.isBefore(round.endTime)

  /** Admins always see the board; public and contestant scoreboards live in teacher-web. */
  def canSeeBoard(round: Contest, viewer: Viewer, now: DateTime): Boolean = viewer match {
    case AdminViewer => true
    case _ => false
  }
}

/** Where the monitor pages read rounds and boards from; functions so that the controllers can be tested without a DB. */
case class BoardSource(round: Int => Future[Option[Contest]], board: Int => Future[MonitorPoints])

object BoardSource {
  /** Rounds as currently stored (so is_finished is never stale) and boards rendered on demand. */
  def fromDb(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): BoardSource =
    BoardSource(id => Rounds.currentRound(db, id), id => MonitorPoints.load(db, id))
}

/** The boards of the monitor pages, rendered on demand from the database. */
class Monitor(dbConfig: DatabaseConfig[JdbcProfile], system: ActorSystem) {
  val source: BoardSource = BoardSource.fromDb(dbConfig.db)(system.dispatcher)
}
