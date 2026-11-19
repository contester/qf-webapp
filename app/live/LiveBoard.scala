package live

import models.{Board, ProblemColumn, TeamRow}
import org.stingray.contester.dbmodel.Team

/** A row of the live board: the team, its non-empty cells by problem id, its total and its place (None: not rated). */
case class LiveRow(team: Team, cells: Map[String, LiveCell], total: Double, place: Option[Int]) {
  def cell(problem: String): LiveCell = cells.getOrElse(problem, LiveCell.Empty)
}

/** One row replaced in place: it now stands at `position`; `ranks` are the places of rankFrom until rankFrom + ranks.size. */
case class RowChange(team: Int, position: Int, row: LiveRow, rankFrom: Int, ranks: Seq[Option[Int]])

/** What a change of the board sends to its viewers: one row, or (a row added or removed) the whole board. */
sealed trait BoardOut
case class RowOut(change: RowChange) extends BoardOut
case class WholeOut(board: LiveBoard) extends BoardOut

/**
  * The points board as the admin live monitor shows it, ranked by Board.rank: total descending, shared places with a
  * skip (1, 1, 3), not rated teams after the ranked ones without a place, disqualified teams left out.
  */
case class LiveBoard(problems: Seq[ProblemColumn], rows: Vector[LiveRow]) {
  def rowOf(teamId: Int): Option[LiveRow] = rows.find(_.team.id == teamId)

  /** The team's row with these cells (a new team is added; a disqualified one leaves the board), re-ranked. */
  def update(team: Team, cells: Map[String, LiveCell]): (LiveBoard, Option[BoardOut]) =
    if (rowOf(team.id).exists(r => r.cells == cells && r.team == team)) (this, None)
    else changeTo(LiveBoard.of(problems, rows.filterNot(_.team.id == team.id).map(r => r.team -> r.cells) :+ (team -> cells)), team.id)

  /** The board without this team (its credential is gone). */
  def remove(teamId: Int): (LiveBoard, Option[BoardOut]) =
    if (rowOf(teamId).isEmpty) (this, None)
    else changeTo(LiveBoard.of(problems, rows.filterNot(_.team.id == teamId).map(r => r.team -> r.cells)), teamId)

  private def changeTo(next: LiveBoard, teamId: Int): (LiveBoard, Option[BoardOut]) = {
    val oldPos = rows.indexWhere(_.team.id == teamId)
    val newPos = next.rows.indexWhere(_.team.id == teamId)
    if (next == this) (this, None)
    else if (oldPos < 0 || newPos < 0 || next.rows.size != rows.size) (next, Some(WholeOut(next)))
    else {
      val differing = next.rows.indices.filter(i => rows(i).place != next.rows(i).place)
      val from = (Seq(newPos, oldPos) ++ differing.headOption).min
      val to = (Seq(newPos, oldPos) ++ differing.lastOption).max
      (next, Some(RowOut(RowChange(teamId, newPos, next.rows(newPos), from, next.rows.slice(from, to + 1).map(_.place)))))
    }
  }
}

object LiveBoard {
  val empty: LiveBoard = LiveBoard(Nil, Vector.empty)

  /** The board of these teams and their cells, ranked by Board.rank. */
  def of(problems: Seq[ProblemColumn], teams: Seq[(Team, Map[String, LiveCell])]): LiveBoard = {
    val cells = teams.map { case (t, c) => t.id -> c.filterNot(_._2.isEmpty) }.toMap
    val ranked = Board.rank(teams.map { case (t, _) =>
      TeamRow(t, problems.map(p => cells(t.id).getOrElse(p.id, LiveCell.Empty).points))
    })
    LiveBoard(problems, ranked.map(r => LiveRow(r.team, cells(r.team.id), r.total, r.place)).toVector)
  }

  def build(problems: Seq[ProblemColumn], teams: Seq[Team], cells: Map[Int, Map[String, LiveCell]]): LiveBoard =
    of(problems, teams.map(t => t -> cells.getOrElse(t.id, Map.empty[String, LiveCell])))
}
