package live

import models.ProblemColumn
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, NotRated, Team}
import palevo._

/** palevo's view of the live points board: a cell at its maximum is a solve, at the time of its first full submit. */
class PalevoViewSpec extends AnyFunSuite {
  private val t0 = new DateTime(2026, 11, 1, 10, 0)
  private val contest = Contest(1, "C", t0, t0.plusHours(5), None, 1)
  private val problems = Seq(ProblemColumn("A", "a", 10), ProblemColumn("B", "b", 20))
  private val during = t0.plusHours(2)
  private val after = t0.plusHours(6)

  private def team(id: Int, notRated: Boolean = false): Team =
    Team(id, id, s"T$id", s"School$id", "City", if (notRated) Some(NotRated) else None)

  private val teams = Seq(team(1), team(2), team(3, notRated = true))

  private def full(max: Double, at: Int) = LiveCell(Some(max), 1, full = true, 0, Some(at))

  private val board = LiveBoard.build(problems, teams, Map(
    1 -> Map("A" -> full(10, 600), "B" -> LiveCell(Some(19), 2, full = false, 0, None)),
    2 -> Map("A" -> full(10, 700), "B" -> full(20, 800)),
    3 -> Map("A" -> full(10, 100))))

  test("rows: place (0 without one), the number of full cells, and their times") {
    val v = PalevoView.of(board)
    assert(v.rows.map(r => (r.team.id, r.rank, r.solved, r.solves)) == Seq(
      (2, 1, 2, Map("A" -> 700, "B" -> 800)),
      (1, 2, 1, Map("A" -> 600)),
      (3, 0, 1, Map("A" -> 100))))
  }

  test("an in-flight or partial cell is not a solve") {
    val b = LiveBoard.build(problems, teams, Map(1 -> Map("A" -> LiveCell(None, 1, full = false, 1, None), "B" -> LiveCell(Some(5), 1, full = false, 0, None))))
    assert(PalevoView.of(b).rows.forall(_.solves.isEmpty))
  }

  test("while the round runs standings events go to the jury only; once it is over, to the public") {
    val a = new StandingsAnnouncer
    val empty = LiveBoard.build(problems, teams, Map.empty)
    assert(a.observe(contest, PalevoView.of(empty), during).isEmpty) // the first observation only records
    val running = a.observe(contest, PalevoView.of(board), during)
    assert(running.nonEmpty && running.forall(_.audience == JuryAudience))
    // The public tracker starts at the end: its first observation only records, so the end itself says nothing.
    assert(a.observe(contest, PalevoView.of(board), after).isEmpty)
    val more = LiveBoard.build(problems, teams :+ team(4), Map(4 -> Map("B" -> full(20, 9000))) ++
      Map(1 -> board.rowOf(1).get.cells, 2 -> board.rowOf(2).get.cells, 3 -> board.rowOf(3).get.cells))
    assert(a.observe(contest, PalevoView.of(more), after).isEmpty, "B was already solved")
    val finished = contest.copy(isFinished = true)
    val b2 = new StandingsAnnouncer
    b2.observe(finished, PalevoView.of(empty), during)
    assert(b2.observe(finished, PalevoView.of(board), during).forall(_.audience == PublicAudience))
  }
}
