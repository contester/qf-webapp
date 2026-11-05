package live

import models.{ACM, ACMCell, AnyStatus, Foo}
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, LocalTeam, Team}
import palevo._

class PalevoViewSpec extends AnyFunSuite {
  private val t0 = new DateTime(2026, 11, 1, 10, 0)
  // starts 10:00, freezes 14:00, ends 15:00, exposed 16:00
  private val contest = Contest(1, "C", t0, t0.plusHours(4), t0.plusHours(5), t0.plusHours(6), None, "en", schoolMode = false)
  private val problems = Seq("A", "B")
  private val during = t0.plusHours(4).plusMinutes(30)

  private def team(id: Int, notRated: Boolean = false): Team =
    LocalTeam(id, 1, s"School$id", s"S$id", 0, "", notRated = notRated, noPrint = false, disabled = false)

  private val teams = Seq(team(1), team(2))

  private def board(solves: Map[Int, Map[String, Int]], pending: Map[Int, Map[String, Int]] = Map.empty,
                    teams: Seq[Team] = teams): LiveBoard = {
    val cells = (solves.keySet ++ pending.keySet).map { t =>
      t -> (solves.getOrElse(t, Map.empty).map { case (p, s) => p -> (LiveCell.Solved(1, s): LiveCell) } ++
        pending.getOrElse(t, Map.empty).map { case (p, n) => p -> (LiveCell.Pending(n): LiveCell) })
    }.toMap
    LiveBoard.build(problems, teams, cells)
  }

  private def status(solves: Map[Int, Map[String, Int]], teams: Seq[Team] = teams): AnyStatus = {
    val sorted = teams.map { t =>
      val s = solves.getOrElse(t.id, Map.empty[String, Int])
      (t, ACM.Score(s.size, s.values.map(_ / 60).sum, (0 +: s.values.map(_ / 60).toSeq).max), s)
    }.sortWith((a, b) => Foo.rowLess((a._1, a._2), (b._1, b._2)))
    val ranks = Foo.ranks(sorted.map(x => (x._1, x._2)))
    ACM.Status(problems, sorted.zip(ranks).map { case ((t, sc, s), k) =>
      Foo.RankedRow(k, t, sc, s.map { case (p, sec) => p -> ACMCell(1, sec, fullSolution = true) })
    })
  }

  private val steps = Seq(
    Map.empty[Int, Map[String, Int]],
    Map(1 -> Map("A" -> 600)),
    Map(1 -> Map("A" -> 600), 2 -> Map("A" -> 700, "B" -> 800)))

  private def run(views: Seq[StandingsView]): Seq[Seq[(String, Audience, String)]] = {
    val a = new StandingsAnnouncer
    views.map { v =>
      a.observe(contest, v, v, during).map(e => (e.eventType, e.audience, e.data.toString))
    }
  }

  test("board view and ACM status view give identical events across three observations") {
    val fromBoard = run(steps.map(s => PalevoView.of(board(s))))
    val fromAcm = run(steps.map(s => StandingsView.of(status(s))))
    assert(fromBoard == fromAcm)
    assert(fromBoard.head.isEmpty)
    assert(fromBoard(1).map(_._1).contains("first_solve_contest"))
    assert(fromBoard(2).map(_._1).contains("new_leader"))
  }

  test("tracker alone: identical events for both adapters") {
    def go(vs: Seq[StandingsView]) = {
      val t = new StandingsTracker
      vs.map(v => t.observe(contest, v, during).map(e => (e.eventType, e.data.toString)))
    }
    assert(go(steps.map(s => PalevoView.of(board(s)))) == go(steps.map(s => StandingsView.of(status(s)))))
  }

  test("pending cells are not solves") {
    val t = new StandingsTracker
    t.observe(contest, PalevoView.of(board(Map.empty)), during)
    assert(t.observe(contest, PalevoView.of(board(Map.empty, Map(1 -> Map("A" -> 2)))), during).isEmpty)
    assert(PalevoView.of(board(Map.empty, Map(1 -> Map("A" -> 2)))).rows.forall(_.solves.isEmpty))
  }

  test("view rows carry team, rank, solved and solve seconds") {
    val r = PalevoView.of(board(steps(2))).rows
    assert(r.map(x => (x.team.id, x.rank, x.solved, x.solves)) ==
      Seq((2, 1, 2, Map("A" -> 700, "B" -> 800)), (1, 2, 1, Map("A" -> 600))))
  }

  // frozen view lags the real one by a step, observed inside the live window
  private def laggingRun(steps: Seq[Map[Int, Map[String, Int]]], ts: Seq[Team], acm: Boolean) = {
    def v(s: Map[Int, Map[String, Int]]) = if (acm) StandingsView.of(status(s, ts)) else PalevoView.of(board(s, teams = ts))
    val a = new StandingsAnnouncer
    steps.indices.map { n =>
      a.observe(contest, v(steps(math.max(n - 1, 0))), v(steps(n)), during)
        .map(e => (e.eventType, e.audience, e.data.toString))
    }
  }

  private def bothAdapters(steps: Seq[Map[Int, Map[String, Int]]], ts: Seq[Team] = teams) = {
    assert(StandingsTracker.liveWindow(contest, during))
    val b = laggingRun(steps, ts, acm = false)
    val c = laggingRun(steps, ts, acm = true)
    assert(b == c)
    b.flatten
  }

  test("lagging frozen view: public/jury split is identical for both adapters, jury events occur") {
    val es = bothAdapters(steps)
    assert(es.exists(_._2 == JuryAudience), es.toString)
    assert(es.exists(_._2 == PublicAudience), es.toString)
  }

  test("tie for rank 1: no new_leader, same for both adapters") {
    val es = bothAdapters(Seq(Map.empty, Map(1 -> Map("A" -> 600)), Map(1 -> Map("A" -> 600), 2 -> Map("B" -> 600))))
    assert(!es.exists(_._1 == "new_leader"), es.toString)
  }

  test("not-rated team with the highest score is ignored") {
    val ts = Seq(team(1), team(2), team(3, notRated = true))
    val es = bothAdapters(Seq(Map.empty, Map(3 -> Map("A" -> 100, "B" -> 200)), Map(3 -> Map("A" -> 100, "B" -> 200), 1 -> Map("A" -> 900))), ts)
    assert(es.forall(e => !e._3.contains("School3")), es.toString)
    assert(es.exists(_._1 == "first_solve_contest"))
  }

  test("equal solve seconds: first solve tie broken by team id") {
    val es = bothAdapters(Seq(Map.empty, Map(2 -> Map("A" -> 600), 1 -> Map("A" -> 600))))
    assert(es.exists(e => e._1 == "first_solve_contest" && e._3.contains("School1")), es.toString)
  }
}
