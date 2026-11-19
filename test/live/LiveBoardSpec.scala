package live

import models.{Board, ProblemColumn, TeamRow}
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Disqualified, NotRated, Team}

/** The live board ranks by Board.rank and reports a change of one row as a row with the places that moved. */
class LiveBoardSpec extends AnyFunSuite {
  private val problems = Seq(ProblemColumn("A", "a", 10), ProblemColumn("B", "b", 20))

  private def team(id: Int, notRated: Boolean = false): Team =
    Team(id, id, s"T$id", f"School$id%02d", "City", if (notRated) Some(NotRated) else None)

  private def c(p: Double, max: Double = 10): LiveCell = LiveCell(Some(p), 1, full = p == max, 0, None)

  private def board(rows: (Team, Map[String, LiveCell])*): LiveBoard = LiveBoard.of(problems, rows)

  test("build ranks exactly as Board.rank: shared places 1, 1, 3, not rated last without a place, disqualified out") {
    val ts = Seq(team(1), team(2), team(3), team(4, notRated = true), team(5).copy(unrated = Some(Disqualified)))
    val cells = Map(1 -> Map("A" -> c(10)), 2 -> Map("B" -> c(10, 20)), 3 -> Map("A" -> c(5)), 4 -> Map("A" -> c(10), "B" -> c(20, 20)),
      5 -> Map("B" -> c(20, 20)))
    val b = LiveBoard.build(problems, ts, cells)
    assert(b.rows.map(r => (r.team.id, r.place, r.total)) ==
      Seq((1, Some(1), 10.0), (2, Some(1), 10.0), (3, Some(3), 5.0), (4, None, 30.0)))
    val expected = Board.rank(ts.map(t => TeamRow(t, problems.map(p => cells.getOrElse(t.id, Map.empty[String, LiveCell]).getOrElse(p.id, LiveCell.Empty).points))))
    assert(b.rows.map(r => (r.place, r.team, r.total)) == expected.map(r => (r.place, r.team, r.total)))
  }

  test("empty cells are not stored") {
    val b = board(team(1) -> Map("A" -> LiveCell.Empty, "B" -> c(3, 20)))
    assert(b.rowOf(1).get.cells.keySet == Set("B"))
  }

  test("update moving a team up reports the new position and the places that moved") {
    val b = board(team(1) -> Map("A" -> c(10)), team(2) -> Map("A" -> c(5)), team(3) -> Map.empty)
    val (nb, out) = b.update(team(3), Map("A" -> c(10), "B" -> c(5, 20)))
    assert(nb.rows.map(_.team.id) == Seq(3, 1, 2))
    out match {
      case Some(RowOut(ch)) =>
        assert(ch.team == 3 && ch.position == 0 && ch.rankFrom == 0 && ch.ranks == Seq(Some(1), Some(2), Some(3)))
        assert(ch.row.total == 15.0)
      case other => fail(other.toString)
    }
  }

  test("update that changes a cell but not the order yields a one-element range") {
    val b = board(team(1) -> Map("A" -> c(10)), team(2) -> Map("A" -> c(5)))
    val (_, out) = b.update(team(1), Map("A" -> c(10), "B" -> LiveCell(None, 1, full = false, 1, None)))
    out match {
      case Some(RowOut(ch)) => assert(ch.position == 0 && ch.rankFrom == 0 && ch.ranks == Seq(Some(1)))
      case other => fail(other.toString)
    }
  }

  test("update with unchanged cells and team yields None") {
    val b = board(team(1) -> Map("A" -> c(10)))
    assert(b.update(team(1), Map("A" -> c(10))) == ((b, None)))
  }

  test("a tie that forms shares the place: the row and the places around it") {
    val b = board(team(1) -> Map("A" -> c(10)), team(2) -> Map("A" -> c(5)))
    val (nb, out) = b.update(team(2), Map("A" -> c(10)))
    assert(nb.rows.map(_.place) == Seq(Some(1), Some(1)))
    out match {
      case Some(RowOut(ch)) => assert(ch.position == 1 && ch.ranks == Seq(Some(1)) && ch.rankFrom == 1)
      case other => fail(other.toString)
    }
  }

  test("a team that becomes not rated drops to the tail without a place; the next rated team takes place 1") {
    val b = board(team(1) -> Map("A" -> c(10)), team(2) -> Map("A" -> c(5)))
    val (nb, out) = b.update(team(1, notRated = true), Map("A" -> c(10)))
    assert(nb.rows.map(r => (r.team.id, r.place)) == Seq((2, Some(1)), (1, None)))
    out match {
      case Some(RowOut(ch)) => assert(ch.position == 1 && ch.rankFrom == 0 && ch.ranks == Seq(Some(1), None))
      case other => fail(other.toString)
    }
  }

  test("a disqualified or removed team leaves as a whole board; a new team arrives as a whole board") {
    val b = board(team(1) -> Map("A" -> c(10)), team(2) -> Map("A" -> c(5)))
    val (dq, outDq) = b.update(team(1).copy(unrated = Some(Disqualified)), Map("A" -> c(10)))
    assert(dq.rows.map(_.team.id) == Seq(2) && outDq == Some(WholeOut(dq)))
    val (rm, outRm) = b.remove(2)
    assert(rm.rows.map(_.team.id) == Seq(1) && outRm == Some(WholeOut(rm)))
    assert(b.remove(9) == ((b, None)))
    val (add, outAdd) = b.update(team(3), Map.empty)
    assert(add.rows.map(_.team.id) == Seq(1, 2, 3) && outAdd == Some(WholeOut(add)))
  }
}
