package palevo

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, NotRated, Team}

/**
  * One (live) board: the jury hears standings while the round runs; the public hears nothing before the end, then the
  * final standings once, and again only when they change.
  */
class StandingsAnnouncerSpec extends AnyFunSuite {
  private val start = new DateTime(2026, 11, 1, 10, 0)
  private val contest = Contest(1, "C", start, start.plusHours(5), None, 1)
  private val running = start.plusHours(2)
  private val end = start.plusHours(5)
  private val over = start.plusHours(7)

  private def team(id: Int, notRated: Boolean = false) =
    Team(id, id, s"T$id", s"School$id", "City", if (notRated) Some(NotRated) else None)

  private def row(rank: Int, t: Team, solves: Map[String, Int], points: Double = -1) =
    StandingsRow(t, rank, solves.size, solves, if (points >= 0) points else solves.size * 10.0)

  private val nobody = StandingsView(Seq(row(1, team(1), Map()), row(1, team(2), Map())))
  private val t1A = StandingsView(Seq(row(1, team(1), Map("A" -> 600)), row(2, team(2), Map())))
  private val t1A_t2B = StandingsView(Seq(row(1, team(1), Map("A" -> 600)), row(1, team(2), Map("B" -> 900))))

  private def types(es: Seq[PalevoEvent]) = es.map(e => (e.eventType, e.audience))

  private def primed(c: Contest = contest) = {
    val a = new StandingsAnnouncer
    a.observe(c, nobody, running) // first observation: silent
    a
  }

  private def finals(es: Seq[PalevoEvent]) = es.collect { case f: FinalStandings => f }

  test("first observation is silent") {
    assert(new StandingsAnnouncer().observe(contest, t1A, running).isEmpty)
  }

  test("while the round runs a change is announced to the jury only; nothing public before the end") {
    val es = primed().observe(contest, t1A, running)
    assert(es.nonEmpty && es.forall(_.audience == JuryAudience), types(es).toString)
    assert(es.forall(_.id.startsWith("qf-jury-")))
    assert(es.map(_.eventType).contains("first_solve_contest"))
    val a = primed()
    val all = Seq(t1A, t1A_t2B, nobody, t1A).flatMap(b => a.observe(contest, b, end.minusMillis(1)))
    assert(all.forall(_.audience == JuryAudience) && finals(all).isEmpty)
  }

  test("the jury keeps its own state: a later change is announced without repeating older facts") {
    val a = primed()
    a.observe(contest, t1A, running)
    assert(types(a.observe(contest, t1A_t2B, running)) == Seq(("first_solve_problem", JuryAudience)))
  }

  test("at the end the public hears the final standings exactly once") {
    val a = primed()
    a.observe(contest, t1A, running)
    val es = a.observe(contest, t1A, end)
    assert(es.size == 1 && es.head.audience == PublicAudience)
    val f = finals(es).head
    assert(f.eventType == "final_standings" && !f.updated)
    assert(f.rows == Seq(FinalRow(1, EventBuilders.teamOf(team(1)), 10.0, 1)))
    assert(a.observe(contest, t1A, end).isEmpty)
    assert(a.observe(contest, t1A, over).isEmpty)
  }

  test("after the end: only a change of the final standings is announced again, as an update; no jury events") {
    val a = primed()
    a.observe(contest, t1A, running)
    a.observe(contest, t1A, end)
    val es = a.observe(contest, t1A_t2B, over) // a rejudge after the end puts team 2 next to team 1
    assert(types(es) == Seq(("final_standings", PublicAudience)))
    val f = finals(es).head
    assert(f.updated && f.rows.map(r => (r.place, r.team.id)) == Seq((1, 1), (1, 2)))
    assert(f.id == s"qf-final-1-${over.getMillis}")
    assert(a.observe(contest, t1A_t2B, over.plusMinutes(1)).isEmpty)
  }

  test("a change below the final places is not a change of the final standings") {
    val a = primed()
    def board(fourth: Double, third: Int, fourthRank: Int) = StandingsView(Seq(row(1, team(1), Map("A" -> 1), 30),
      row(2, team(2), Map(), 20), row(third, team(3), Map(), 10), row(fourthRank, team(4), Map(), fourth)).sortBy(_.rank))
    a.observe(contest, board(1, 3, 4), running)
    assert(finals(a.observe(contest, board(1, 3, 4), end)).size == 1)
    assert(a.observe(contest, board(5, 3, 4), over).isEmpty)
    assert(finals(a.observe(contest, board(15, 4, 3), over)).head.rows.map(_.team.id) == Seq(1, 2, 4))
  }

  test("unrated teams and teams without points are not in the final standings") {
    val guest = team(9, notRated = true)
    val a = primed()
    val es = a.observe(contest, StandingsView(Seq(row(0, guest, Map("A" -> 1), 50), row(1, team(1), Map(), 0))), end)
    assert(finals(es).head.rows.isEmpty)
  }

  test("restart rule: a contest first seen after its end records its final standings silently") {
    val a = new StandingsAnnouncer
    assert(a.observe(contest, t1A, over).isEmpty)
    assert(a.observe(contest, t1A, over).isEmpty)
    assert(finals(a.observe(contest, t1A_t2B, over)).size == 1)
  }

  test("a round finished early is over: no jury events any more, the final standings at once") {
    val finished = contest.copy(isFinished = true)
    val a = primed()
    val es = a.observe(finished, t1A, running)
    assert(types(es) == Seq(("final_standings", PublicAudience)))
  }

  test("contests are independent") {
    val a = primed()
    val other = contest.copy(id = 2)
    assert(a.observe(other, t1A, over).isEmpty) // first sight of contest 2, already over
    assert(finals(a.observe(contest, t1A, end)).size == 1)
  }
}
