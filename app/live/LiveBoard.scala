package live

import models.{ACM, ACMCell, Foo}
import org.stingray.contester.dbmodel.Team

case class LiveRow(team: Team, score: ACM.Score, cells: Map[String, LiveCell], rank: Int)

// ranks covers positions rankFrom until rankFrom + ranks.size; solved holds only changed header counts.
case class RowChange(team: Int, position: Int, row: LiveRow, rankFrom: Int, ranks: Seq[Int], solved: Map[String, Int])

case class LiveBoard(problems: Seq[String], rows: Vector[LiveRow], solvedCounts: Map[String, Int]) {
  def rowOf(teamId: Int): Option[LiveRow] = rows.find(_.team.id == teamId)

  def update(team: Team, cells: Map[String, LiveCell]): (LiveBoard, Option[RowChange]) = {
    val oldPos = rows.indexWhere(_.team.id == team.id)
    val old = if (oldPos >= 0) Some(rows(oldPos)) else None
    if (old.exists(_.cells == cells)) (this, None)
    else {
      val score = LiveBoard.scoreOf(cells.values)
      val without = if (oldPos >= 0) rows.patch(oldPos, Nil, 1) else rows
      val newPos = {
        val i = without.indexWhere(r => Foo.rowLess((team, score), (r.team, r.score)))
        if (i < 0) without.size else i
      }
      val sortedPairs = without.patch(newPos, Seq(LiveRow(team, score, cells, 0)), 0)
      val newRanks = Foo.ranks(sortedPairs.map(r => (r.team, r.score)))
      val newRows = sortedPairs.zip(newRanks).map { case (r, k) => r.copy(rank = k) }

      val oldRanks = rows.map(_.rank)
      val differing = newRows.indices.filter(i => oldRanks.lift(i) != Some(newRows(i).rank))
      val from = (Seq(newPos, if (oldPos >= 0) oldPos else newPos) ++ differing.headOption).min
      val to = (Seq(newPos, oldPos) ++ differing.lastOption).max
      val newCounts = LiveBoard.countsOf(problems, newRows)
      val moved = newCounts.filter { case (p, n) => solvedCounts.get(p) != Some(n) }
      val row = newRows(newPos)
      (LiveBoard(problems, newRows, newCounts),
        Some(RowChange(team.id, newPos, row, from, newRows.slice(from, to + 1).map(_.rank), moved)))
    }
  }
}

object LiveBoard {
  def scoreOf(cells: Iterable[LiveCell]): ACM.Score =
    cells.foldLeft(ACM.Score(0, 0, 0)) {
      case (s, LiveCell.Solved(attempts, seconds)) =>
        val c = ACMCell(attempts, seconds, fullSolution = true).score
        ACM.Score(s.solved + 1, s.penalty + c, s.lastSubmitPenalty.max(c))
      case (s, _) => s
    }

  private def countsOf(problems: Seq[String], rows: Seq[LiveRow]): Map[String, Int] =
    problems.map(p => p -> rows.count(_.cells.get(p).exists(_.solved))).toMap

  def build(problems: Seq[String], teams: Seq[Team], cells: Map[Int, Map[String, LiveCell]]): LiveBoard = {
    val scored = teams.map { t =>
      val c = cells.getOrElse(t.id, Map.empty[String, LiveCell])
      LiveRow(t, scoreOf(c.values), c, 0)
    }.sortWith((a, b) => Foo.rowLess((a.team, a.score), (b.team, b.score)))
    val rs = Foo.ranks(scored.map(r => (r.team, r.score)))
    val rows = scored.zip(rs).map { case (r, k) => r.copy(rank = k) }.toVector
    LiveBoard(problems, rows, countsOf(problems, rows))
  }
}
