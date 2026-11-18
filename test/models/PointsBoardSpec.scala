package models

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Disqualified, NotRated, Team}

/** The points cell of one team and problem, and the ranking of the board's rows. */
class PointsBoardSpec extends AnyFunSuite {
  private val t0 = new DateTime(2030, 1, 1, 12, 0)

  private def submit(id: Int, points: Option[Double], finished: Boolean = true, compiled: Boolean = true,
                     team: Int = 1, problem: String = "A", seconds: Int = 0): Submit =
    Submit(SubmitId(id, Arrived(t0.plusSeconds(seconds), seconds, afterFreeze = true), team, 7, RatedProblem(problem, 30), "cpp"),
      finished, compiled, 0, 0, None, points, 20)

  test("an empty cell: no submits, no best, not full") {
    assert(PointsScorer.cell(Nil, 20) == PointsCell(None, 0, full = false))
  }

  test("best is the maximum over finished, compiled submits with points, not their sum") {
    val c = PointsScorer.cell(Seq(submit(1, Some(5)), submit(2, Some(12.5)), submit(3, Some(7))), 20)
    assert(c == PointsCell(Some(12.5), 3, full = false))
  }

  test("a rejudge replaces the same row's points: only the current value counts") {
    val before = submit(1, Some(18))
    val after = before.copy(points = Some(4)) // the same submits row after a new testing
    assert(PointsScorer.cell(Seq(before), 20).best == Some(18))
    assert(PointsScorer.cell(Seq(after), 20) == PointsCell(Some(4), 1, full = false))
  }

  test("a submit with NULL points (testing in flight or being retested) is ignored for best but counts as an attempt") {
    val inFlight = submit(2, None, finished = false)
    val finishedNull = submit(3, None) // finished and compiled, points not yet written
    assert(PointsScorer.cell(Seq(submit(1, Some(6)), inFlight, finishedNull), 20) == PointsCell(Some(6), 3, full = false))
    assert(PointsScorer.cell(Seq(inFlight), 20) == PointsCell(None, 1, full = false))
  }

  test("unfinished or not compiled submits never give the best, even if they carry points") {
    assert(PointsScorer.cell(Seq(submit(1, Some(9), finished = false)), 20).best.isEmpty)
    assert(PointsScorer.cell(Seq(submit(1, Some(9), compiled = false)), 20).best.isEmpty)
  }

  test("full exactly at max_points, never for a problem worth 0") {
    assert(PointsScorer.cell(Seq(submit(1, Some(20))), 20).full)
    assert(!PointsScorer.cell(Seq(submit(1, Some(19.99))), 20).full)
    assert(!PointsScorer.cell(Seq(submit(1, Some(0))), 0).full)
    assert(PointsScorer.cell(Seq(submit(1, Some(0))), 0).best == Some(0))
  }

  private def team(n: Int, unrated: Option[org.stingray.contester.dbmodel.Unrated] = None) =
    Team(100 + n, n, s"T$n", "School", "City", unrated)

  private def cell(p: Double) = PointsCell(Some(p), 1, full = false)
  private val none = PointsCell(None, 0, full = false)

  private def row(n: Int, cells: Seq[PointsCell], unrated: Option[org.stingray.contester.dbmodel.Unrated] = None) =
    TeamRow(team(n, unrated), cells)

  test("total is the sum of the cells' best; a cell without best counts 0") {
    assert(Board.total(Seq(cell(20), none, cell(12.5))) == 32.5)
    assert(Board.total(Seq(cell(0.1), cell(0.2))) == 0.3)
    assert(Board.total(Nil) == 0)
  }

  test("ranked by total descending; equal totals share a place and the next place skips") {
    val ranked = Board.rank(Seq(
      row(3, Seq(cell(30), none)),
      row(2, Seq(cell(25), cell(25))),
      row(1, Seq(cell(50), none))))
    assert(ranked.map(r => (r.place, r.team.number, r.total)) ==
      Seq((Some(1), 1, 50.0), (Some(1), 2, 50.0), (Some(3), 3, 30.0)))
    assert(ranked.head.cells == Seq(cell(50), none))
  }

  test("within equal totals the order is by team number ascending") {
    val ranked = Board.rank(Seq(row(9, Seq(cell(10))), row(4, Seq(cell(10))), row(6, Seq(cell(10))), row(1, Seq(cell(11)))))
    assert(ranked.map(r => (r.place, r.team.number)) == Seq((Some(1), 1), (Some(2), 4), (Some(2), 6), (Some(2), 9)))
  }

  test("NotRated rows come after the ranked ones without a place and do not move anyone's place") {
    val ranked = Board.rank(Seq(
      row(1, Seq(cell(10))),
      row(2, Seq(cell(99)), Some(NotRated)),
      row(3, Seq(cell(20))),
      row(4, Seq(cell(5)), Some(NotRated))))
    assert(ranked.map(r => (r.place, r.team.number)) == Seq((Some(1), 3), (Some(2), 1), (None, 2), (None, 4)))
  }

  test("Disqualified rows are dropped") {
    val ranked = Board.rank(Seq(row(1, Seq(cell(10))), row(2, Seq(cell(99)), Some(Disqualified))))
    assert(ranked.map(_.team.number) == Seq(1))
    assert(ranked.head.place == Some(1))
  }

  test("an empty board ranks to nothing") {
    assert(Board.rank(Nil) == Nil)
  }

  test("all-zero totals share place 1") {
    val ranked = Board.rank(Seq(row(2, Seq(none)), row(1, Seq(cell(0))), row(3, Nil)))
    assert(ranked.map(r => (r.place, r.team.number, r.total)) == Seq((Some(1), 1, 0.0), (Some(1), 2, 0.0), (Some(1), 3, 0.0)))
  }

  test("MonitorPoints.build: cells per problem in order, unknown teams' submits ignored, ranked") {
    val problems = Seq(ProblemColumn("A", "Alpha", 20), ProblemColumn("B", "Beta", 25))
    val teams = Seq(team(1), team(2))
    val subs = Seq(
      submit(1, Some(20), team = 101, problem = "A"),
      submit(2, Some(10), team = 101, problem = "B"),
      submit(3, Some(5), team = 102, problem = "B"),
      submit(4, Some(25), team = 999, problem = "B"))
    val m = MonitorPoints.build(problems, teams, subs)
    assert(m.problems == problems)
    assert(m.rows.map(r => (r.place, r.team.number, r.total)) == Seq((Some(1), 1, 30.0), (Some(2), 2, 5.0)))
    assert(m.rows.head.cells == Seq(PointsCell(Some(20), 1, full = true), PointsCell(Some(10), 1, full = false)))
    assert(m.rows(1).cells == Seq(PointsCell(None, 0, full = false), PointsCell(Some(5), 1, full = false)))
  }
}
