package palevo

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, Team}

/** One (real) board: the jury hears standings while the round runs, the public only once it is over. */
class StandingsAnnouncerSpec extends AnyFunSuite {
  private val start = new DateTime(2026, 11, 1, 10, 0)
  private val contest = Contest(1, "C", start, start.plusHours(5), None, 1)
  private val running = start.plusHours(2)
  private val over = start.plusHours(7)

  private def team(id: Int) = Team(id, id, s"T$id", s"School$id", "City", None)

  private def row(rank: Int, t: Team, solves: Map[String, Int]) = StandingsRow(t, rank, solves.size, solves)

  private val nobody = StandingsView(Seq(row(1, team(1), Map()), row(1, team(2), Map())))
  private val t1A = StandingsView(Seq(row(1, team(1), Map("A" -> 600)), row(2, team(2), Map())))
  private val t1A_t2B = StandingsView(Seq(row(1, team(1), Map("A" -> 600)), row(1, team(2), Map("B" -> 900))))

  private def types(es: Seq[PalevoEvent]) = es.map(e => (e.eventType, e.audience))

  private def primed(c: Contest = contest) = {
    val a = new StandingsAnnouncer
    a.observe(c, nobody, running) // first observation: silent
    a
  }

  test("first observation is silent") {
    assert(new StandingsAnnouncer().observe(contest, t1A, running).isEmpty)
  }

  test("while the round runs a change is announced to the jury only") {
    val es = primed().observe(contest, t1A, running)
    assert(es.nonEmpty && es.forall(_.audience == JuryAudience), types(es).toString)
    assert(es.forall(_.id.startsWith("qf-jury-")))
    assert(es.map(_.eventType).contains("first_solve_contest"))
  }

  test("the jury keeps its own state: a later change is announced without repeating older facts") {
    val a = primed()
    a.observe(contest, t1A, running)
    assert(types(a.observe(contest, t1A_t2B, running)) == Seq(("first_solve_problem", JuryAudience)))
  }

  test("at the end nothing is announced (the public tracker only records); a later change is public") {
    val a = primed()
    a.observe(contest, t1A, running)
    assert(a.observe(contest, t1A, over).isEmpty)
    val es = a.observe(contest, t1A_t2B, over)
    assert(types(es) == Seq(("first_solve_problem", PublicAudience)))
  }

  test("a round finished early is over: no jury events any more") {
    val finished = contest.copy(isFinished = true)
    val a = primed(finished)
    assert(a.observe(finished, t1A, running).forall(_.audience == PublicAudience))
  }
}
