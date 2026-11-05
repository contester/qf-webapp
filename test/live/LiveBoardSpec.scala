package live

import models.{ACM, Foo}
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{LocalTeam, Problem, Team}

class LiveBoardSpec extends AnyFunSuite {
  import LiveCell._

  private val t0 = new DateTime(2026, 11, 1, 12, 0)
  private val rules = FreezeRules(t0.plusHours(4), t0.plusHours(5), t0.plusHours(6))
  private val problems = Seq("A", "B", "C")

  private def team(id: Int, notRated: Boolean = false): Team =
    LocalTeam(id, 1, f"School$id%02d", s"S$id", 0, "", notRated, noPrint = false, disabled = false)

  private def solved(sec: Int, attempts: Int = 1): LiveCell = Solved(attempts, sec)

  private def rec(id: Int, t: Int, p: String, sec: Int, passed: Int): SubmitRec =
    SubmitRec(id, t, p, t0.plusSeconds(sec), sec, Done(compiled = true, passed = passed, taken = 10))

  private def board(rows: (Int, Map[String, LiveCell])*): LiveBoard =
    LiveBoard.build(problems, rows.map(r => team(r._1)), rows.map(r => r._1 -> r._2).toMap)

  test("build equals ACM.calculateStatus for submit sets without pending cells") {
    val teams = Seq(team(1), team(2), team(3, notRated = true), team(4), team(5), team(6))
    val sets = Seq(
      Seq(rec(1, 1, "A", 600, 10), rec(2, 2, "A", 600, 10), rec(3, 3, "A", 100, 10), rec(4, 4, "B", 1200, 10)),
      Seq(rec(1, 1, "A", 100, 3), rec(2, 1, "A", 700, 10), rec(3, 2, "A", 600, 10), rec(4, 2, "B", 650, 10),
        rec(5, 4, "C", 3000, 10), rec(6, 5, "C", 3000, 10), rec(7, 6, "A", 50, 1)),
      Seq(),
      Seq(rec(1, 3, "A", 10, 10), rec(2, 1, "B", 10, 10), rec(3, 2, "B", 10, 10), rec(4, 5, "A", 60, 10), rec(5, 5, "A", 70, 10)))
    sets.foreach { recs =>
      val expected = ACM.calculateStatus(problems.map(Problem(1, _, "", 10)), teams, recs.map(_.toSubmit))
      val cells = recs.groupBy(_.team).map { case (tid, rs) =>
        tid -> rs.groupBy(_.problem).map { case (p, ps) => p -> LiveCells.cellOf(ps, Real, rules, t0) }
      }
      val got = LiveBoard.build(problems, teams, cells)
      assert(got.rows.map(r => (r.team.id, r.rank, r.score)) ==
        expected.rows.map(r => (r.team.id, r.rank, r.score)))
    }
  }

  test("ties share rank and next rank skips") {
    val b = board(1 -> Map("A" -> solved(600)), 2 -> Map("A" -> solved(600)), 3 -> Map("A" -> solved(1200)))
    assert(b.rows.map(r => (r.team.id, r.rank)) == Seq((1, 1), (2, 1), (3, 3)))
  }

  test("not rated team has rank 0 and sorts after rated with equal score") {
    val teams = Seq(team(1, notRated = true), team(2), team(3))
    val c = Map("A" -> solved(600))
    val b = LiveBoard.build(problems, teams, Map(1 -> c, 2 -> c, 3 -> c))
    assert(b.rows.map(r => (r.team.id, r.rank)) == Seq((2, 1), (3, 1), (1, 0)))
  }

  test("pending cells add nothing to the score") {
    assert(LiveBoard.scoreOf(Seq(Pending(2), Failed(3), Empty)) == ACM.Score(0, 0, 0))
    assert(LiveBoard.scoreOf(Seq(Pending(0), solved(125, 3), solved(60))) == ACM.Score(2, 2 + 40 + 1, 42))
  }

  private def five = board(
    1 -> Map("A" -> solved(100), "B" -> solved(100)),
    2 -> Map("A" -> solved(200), "B" -> solved(200)),
    3 -> Map("A" -> solved(300)),
    4 -> Map("A" -> solved(400)),
    5 -> Map())

  test("update moving a team up reports the affected range and new ranks") {
    val b = five
    assert(b.rows.map(_.team.id) == Seq(1, 2, 3, 4, 5))
    val (nb, ch) = b.update(team(4), Map("A" -> solved(10), "B" -> solved(10), "C" -> solved(10)))
    val c = ch.get
    assert(c.team == 4 && c.position == 0 && c.rankFrom == 0)
    assert(nb.rows.map(_.team.id) == Seq(4, 1, 2, 3, 5))
    assert(c.ranks == nb.rows.slice(0, 4).map(_.rank))
    assert(c.ranks == Seq(1, 2, 3, 4))
    assert(c.row == nb.rows.head)
    assert(nb.rows(4) == b.rows(4))
    assert(nb.rowOf(4).contains(c.row))
  }

  test("update that changes a cell but not score or order yields a one-element range") {
    val b = five
    val (nb, ch) = b.update(team(5), Map("A" -> Failed(2)))
    val c = ch.get
    assert(c.position == 4 && c.rankFrom == 4 && c.ranks == Seq(5))
    assert(nb.rowOf(5).get.cells == Map("A" -> Failed(2)))
    assert(nb.rows.map(_.team.id) == b.rows.map(_.team.id))
  }

  test("update with unchanged cells yields None") {
    val b = five
    val (nb, ch) = b.update(team(1), Map("A" -> solved(100), "B" -> solved(100)))
    assert(ch.isEmpty && nb == b)
  }

  test("solved in the change contains only counts that moved") {
    val b = five
    assert(b.solvedCounts == Map("A" -> 4, "B" -> 2, "C" -> 0))
    val (nb, ch) = b.update(team(5), Map("C" -> solved(5)))
    assert(ch.get.solved == Map("C" -> 1))
    assert(nb.solvedCounts == Map("A" -> 4, "B" -> 2, "C" -> 1))
    val (_, ch2) = nb.update(team(1), Map("A" -> solved(100), "B" -> Failed(1)))
    assert(ch2.get.solved == Map("B" -> 1))
  }

  test("Foo.ranks matches rank rule directly") {
    val s = Seq((team(1), 1), (team(2), 1), (team(3, notRated = true), 1), (team(4), 2))
    assert(Foo.ranks(s) == Seq(1, 1, 0, 3))
  }
}
