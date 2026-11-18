package models

import org.stingray.contester.dbmodel.{Problem, Team}

import scala.math.Ordered.orderingToOrdered

// TODO(task17): the ACM board below is used only by the live monitor (app/live/*, LiveMonitorActor) and palevo's
// standings (palevo/StandingsView). The pages use the points board (Monitor.scala). Task 17 rewrites the live cells
// over PointsCell and deletes this file together with ACMCell/ACMScorer.

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
    if (team.disqualified) "DSQ" else if (rank == 0) "*" else rank.toString
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
      if (!team.notRated) last = Some((r, score))
      r
    }
  }

  private case class SomeRow[ScoreType, CellType](team: Team, score: ScoreType,
                                                  cells: Map[String, CellType]) extends MonitorRow[ScoreType, CellType]

  def groupAndRank[ScoreType, CellType <: ProblemCell](teams: Seq[Team], submits: Seq[Submit],
                                                       getCell: (Seq[Submit]) => CellType,
                                                       getScore: (Seq[CellType]) => ScoreType)(implicit ord: Ordering[ScoreType]): Seq[Foo.RankedRow[ScoreType, CellType]] = {
    val rows = submits.groupBy(_.submitId.teamId).map {
      case (teamId, s0) =>
        val cells: Map[String, CellType] = s0.groupBy(_.submitId.problem.id).map {
          case (problemId, s1) =>
            problemId -> getCell(s1)
        }

        teamId -> cells
    }

    // A disabled team is on no board: it neither shows nor affects the ranks.
    val teamRows = teams.filterNot(_.disabled).map { team =>
      val cells = rows.getOrElse(team.id, Seq()).toMap
      val score = getScore(cells.values.toSeq)

      SomeRow(team, score, cells)
    }

    val sorted = teamRows.sortWith((a, b) => rowLess((a.team, a.score), (b.team, b.score)))
    sorted.zip(ranks(sorted.map(r => (r.team, r.score)))).map {
      case (r, rank) => Foo.RankedRow(rank, r.team, r.score, r.cells)
    }
  }


}

object ACM {
  case class Status(problems: Seq[String], rows: Seq[Foo.RankedRow[Score, ACMCell]]) extends AnyStatus {
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
