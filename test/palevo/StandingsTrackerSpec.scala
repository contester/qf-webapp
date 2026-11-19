package palevo

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, NotRated, Team}

class StandingsTrackerSpec extends AnyFunSuite {
  private val now = new DateTime(2026, 11, 1, 12, 0)
  private val contest = Contest(1, "C", now, now.plusHours(5), None, 1)

  private def team(id: Int, notRated: Boolean = false) =
    Team(id, 0, "", s"School$id", "City", (if (notRated) Some(NotRated) else None))

  // solves: problem -> seconds. Rows must be given in rank order.
  private def row(rank: Int, t: Team, solves: Map[String, Int]) = StandingsRow(t, rank, solves.size, solves)

  private def status(rows: StandingsRow*): StandingsView = StandingsView(rows)

  private def types(es: Seq[PalevoEvent]) = es.map(_.eventType)

  private val empty = status(row(1, team(1), Map()), row(1, team(2), Map()))

  test("first observation is silent, even with solves already present") {
    val tr = new StandingsTracker
    assert(tr.observe(contest, status(row(1, team(1), Map("A" -> 600))), now).isEmpty)
  }

  test("first solve of the contest and of the problem are both announced once") {
    val tr = new StandingsTracker
    tr.observe(contest, empty, now)
    val es = tr.observe(contest, status(row(1, team(1), Map("A" -> 600)), row(2, team(2), Map())), now)
    assert(types(es) == Seq("first_solve_contest", "first_solve_problem"))
    val first = es.head.asInstanceOf[FirstSolveContest]
    assert(first.team == PTeam(1, "0.  (School1, City)") && first.problem == "A" && first.elapsedSeconds == 600)
    // announcing again with identical status yields nothing
    assert(tr.observe(contest, status(row(1, team(1), Map("A" -> 600)), row(2, team(2), Map())), now).isEmpty)
  }

  test("a leader is not announced together with the very first solve") {
    val tr = new StandingsTracker
    tr.observe(contest, empty, now)
    val es = tr.observe(contest, status(row(1, team(1), Map("A" -> 600)), row(2, team(2), Map())), now)
    assert(!types(es).contains("new_leader"))
  }

  test("second team solving the same problem announces nothing") {
    val tr = new StandingsTracker
    tr.observe(contest, empty, now)
    tr.observe(contest, status(row(1, team(1), Map("A" -> 600)), row(2, team(2), Map())), now)
    val es = tr.observe(contest, status(row(1, team(1), Map("A" -> 600)), row(1, team(2), Map("A" -> 900))), now)
    assert(es.isEmpty)
  }

  test("another problem by another team announces first_solve_problem only") {
    val tr = new StandingsTracker
    tr.observe(contest, empty, now)
    tr.observe(contest, status(row(1, team(1), Map("A" -> 600)), row(2, team(2), Map())), now)
    val es = tr.observe(contest, status(row(1, team(1), Map("A" -> 600)), row(1, team(2), Map("B" -> 900))), now)
    assert(types(es) == Seq("first_solve_problem"))
    assert(es.head.asInstanceOf[FirstSolveProblem].problem == "B")
  }

  test("leader change announced with solved count") {
    val tr = new StandingsTracker
    tr.observe(contest, empty, now)
    tr.observe(contest, status(row(1, team(1), Map("A" -> 600)), row(2, team(2), Map())), now)
    val es = tr.observe(contest, status(
      row(1, team(2), Map("A" -> 900, "B" -> 1000)), row(2, team(1), Map("A" -> 600))), now)
    val leader = es.collect { case l: NewLeader => l }
    assert(leader.size == 1 && leader.head.team.id == 2 && leader.head.solved == 2)
  }

  test("same leader solving more is not re-announced") {
    val tr = new StandingsTracker
    tr.observe(contest, empty, now)
    tr.observe(contest, status(row(1, team(1), Map("A" -> 600)), row(2, team(2), Map())), now)
    val es = tr.observe(contest, status(row(1, team(1), Map("A" -> 600, "C" -> 700)), row(2, team(2), Map())), now)
    assert(!types(es).contains("new_leader"))
  }

  test("tie at rank 1 announces no leader") {
    val tr = new StandingsTracker
    tr.observe(contest, empty, now)
    tr.observe(contest, status(row(1, team(1), Map("A" -> 600)), row(2, team(2), Map())), now)
    val es = tr.observe(contest, status(row(1, team(1), Map("A" -> 600)), row(1, team(2), Map("A" -> 600))), now)
    assert(!types(es).contains("new_leader"))
  }

  test("breaking a tie by a new sole leader is announced") {
    val tr = new StandingsTracker
    tr.observe(contest, empty, now)
    tr.observe(contest, status(row(1, team(1), Map("A" -> 600)), row(2, team(2), Map())), now)
    tr.observe(contest, status(row(1, team(1), Map("A" -> 600)), row(1, team(2), Map("A" -> 700))), now)
    val es = tr.observe(contest, status(row(1, team(2), Map("A" -> 700, "B" -> 800)), row(2, team(1), Map("A" -> 600))), now)
    assert(es.collect { case l: NewLeader => l.team.id } == Seq(2))
  }

  test("unrated team solving first is ignored") {
    val tr = new StandingsTracker
    val guest = team(9, notRated = true)
    tr.observe(contest, status(row(1, team(1), Map()), row(0, guest, Map())), now)
    val es = tr.observe(contest, status(row(1, team(1), Map()), row(0, guest, Map("A" -> 100))), now)
    assert(es.isEmpty)
  }

  test("contests are tracked independently") {
    val tr = new StandingsTracker
    val other = contest.copy(id = 2)
    tr.observe(contest, empty, now)
    assert(tr.observe(other, status(row(1, team(1), Map("A" -> 600))), now).isEmpty) // first sight of contest 2
    assert(types(tr.observe(contest, status(row(1, team(1), Map("A" -> 600))), now)).contains("first_solve_contest"))
  }
}
