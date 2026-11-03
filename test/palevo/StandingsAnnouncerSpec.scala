package palevo

import models.{ACM, ACMCell, AnyStatus, Foo}
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, LocalTeam}

class StandingsAnnouncerSpec extends AnyFunSuite {
  private val start = new DateTime(2026, 11, 1, 10, 0)
  // starts 10:00, freezes 14:00, ends 15:00, exposed 16:00
  private val contest = Contest(1, "C", start, start.plusHours(4), start.plusHours(5), start.plusHours(6), None, "en", schoolMode = false)
  private val beforeFreeze = start.plusHours(2)
  private val duringFreeze = start.plusHours(4).plusMinutes(30)
  private val afterExpose = start.plusHours(7)

  private def team(id: Int) = LocalTeam(id, 1, s"School$id", s"S$id", 0, "", notRated = false, noPrint = false, disabled = false)

  private def row(rank: Int, t: LocalTeam, solves: Map[String, Int]) =
    Foo.RankedRow(rank, t, ACM.Score(solves.size, solves.values.sum / 60, 0),
      solves.map { case (p, sec) => p -> ACMCell(1, sec, fullSolution = true) })

  private def status(rows: Foo.RankedRow[ACM.Score, ACMCell]*): AnyStatus = ACM.Status(Seq("A", "B"), rows)

  private val nobody = status(row(1, team(1), Map()), row(1, team(2), Map()))
  private val t1A = status(row(1, team(1), Map("A" -> 600)), row(2, team(2), Map()))
  private val t1A_t2B = status(row(1, team(1), Map("A" -> 600)), row(1, team(2), Map("B" -> 900)))

  private def types(es: Seq[PalevoEvent]) = es.map(e => (e.eventType, e.audience))

  private def primed(c: Contest = contest) = {
    val a = new StandingsAnnouncer
    a.observe(c, nobody, nobody, beforeFreeze) // first observation: silent
    a
  }

  test("first observation is silent, also inside the freeze window") {
    assert(new StandingsAnnouncer().observe(contest, t1A, t1A_t2B, duringFreeze).isEmpty)
  }

  test("before the freeze only the public events are sent") {
    val es = primed().observe(contest, t1A, t1A, beforeFreeze)
    assert(es.nonEmpty && es.forall(_.audience == PublicAudience), types(es).toString)
  }

  test("during the freeze a change only the exposed scoreboard shows is announced to the jury only") {
    val a = primed()
    a.observe(contest, nobody, nobody, duringFreeze) // still nothing
    val es = a.observe(contest, nobody, t1A, duringFreeze)
    assert(es.nonEmpty && es.forall(_.audience == JuryAudience), types(es).toString)
    assert(es.forall(_.id.startsWith("qf-jury-")))
    assert(es.map(_.eventType).contains("first_solve_contest"))
  }

  test("a pre-freeze submit judged during the freeze is announced once, publicly") {
    val a = primed()
    val es = a.observe(contest, t1A, t1A, duringFreeze) // both scoreboards change the same way
    assert(es.nonEmpty && es.forall(_.audience == PublicAudience), types(es).toString)
  }

  test("the jury keeps its own state: a later live change is announced without repeating older facts") {
    val a = primed()
    a.observe(contest, nobody, t1A, duringFreeze)
    val es = a.observe(contest, nobody, t1A_t2B, duringFreeze)
    assert(types(es) == Seq(("first_solve_problem", JuryAudience)))
    assert(es.head.asInstanceOf[FirstSolveProblem].problem == "B")
  }

  test("after the scoreboard is exposed nothing is announced twice") {
    val a = primed()
    a.observe(contest, nobody, t1A, duringFreeze)
    assert(a.observe(contest, nobody, t1A_t2B, afterExpose).isEmpty)
  }

  test("no jury events for a contest that has no freeze") {
    val noFreeze = contest.copy(freezeTime = contest.endTime)
    val a = primed(noFreeze)
    val es = a.observe(noFreeze, t1A, t1A, start.plusHours(5).plusMinutes(10)) // after the end, late judging
    assert(es.forall(_.audience == PublicAudience), types(es).toString)
  }

  test("liveWindow boundaries") {
    assert(!StandingsTracker.liveWindow(contest, start.plusHours(3)))
    assert(StandingsTracker.liveWindow(contest, start.plusHours(4)))
    assert(StandingsTracker.liveWindow(contest, start.plusHours(6).minusSeconds(1)))
    assert(!StandingsTracker.liveWindow(contest, start.plusHours(6)))
    assert(!StandingsTracker.liveWindow(contest.copy(freezeTime = contest.endTime), start.plusHours(5).plusMinutes(5)))
  }
}
