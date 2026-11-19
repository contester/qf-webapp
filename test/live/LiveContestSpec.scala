package live

import models.{MonitorPoints, ProblemColumn}
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, Disqualified, NotRated, Team}

import scala.util.Random

class LiveContestSpec extends AnyFunSuite {
  private val start = new DateTime(2026, 11, 1, 10, 0)
  private val end = start.plusHours(5)
  private val contest = Contest(1, "Test", start, end, None, 1)
  private val problems = Seq(ProblemColumn("A", "a", 10), ProblemColumn("B", "b", 20), ProblemColumn("C", "c", 30),
    ProblemColumn("D", "d", 40))

  private def team(id: Int, notRated: Boolean = false): Team =
    Team(id, id, s"T$id", f"School$id%02d", "City", if (notRated) Some(NotRated) else None)

  private val teams = Seq(team(1), team(2), team(3), team(4), team(5, notRated = true))

  private def pts(p: Double) = Done(compiled = true, Some(p))
  private val ce = Done(compiled = false, None)

  private def rec(id: Int, t: Int, p: String, at: DateTime): SubmitRec =
    SubmitRec(id, t, p, at, ((at.getMillis - start.getMillis) / 1000).toInt, live.Pending)

  private def fresh(subs: Seq[SubmitRec] = Nil, ts: Seq[Team] = teams) = new LiveContest(contest, problems, ts, subs)

  private def cellOn(lc: LiveContest, t: Int, p: String): LiveCell = lc.board.rowOf(t).get.cell(p)

  private val now = start.plusHours(1)

  // Restored (deleted in Task 10), in points terms: a rejudge puts a finished submit back in flight (its points no
  // longer count for the best), and its finish brings the new points, which replace the old ones.
  test("rejudge of a finished submit sets it pending, finish sets it back with the new points") {
    val lc = fresh()
    lc.onArrived(rec(1, 1, "A", now))
    lc.onFinished(1, compiled = true, Some(10.0))
    assert(cellOn(lc, 1, "A") == LiveCell(Some(10.0), 1, full = true, 0, Some(3600)))
    assert(lc.board.rowOf(1).get.total == 10.0)

    val rejudge = lc.onRejudge(1).get
    assert(lc.records.map(_.state) == Seq(live.Pending))
    assert(cellOn(lc, 1, "A") == LiveCell(None, 1, full = false, 1, None))
    assert(lc.board.rowOf(1).get.total == 0.0)
    rejudge.board match {
      case Seq(RowOut(c)) => assert(c.team == 1 && c.row.cell("A").pending == 1 && c.row.cell("A").best.isEmpty)
      case other => fail(s"one row expected, got $other")
    }

    val finish = lc.onFinished(1, compiled = true, Some(4.0)).get
    assert(cellOn(lc, 1, "A") == LiveCell(Some(4.0), 1, full = false, 0, None))
    assert(lc.board.rowOf(1).get.total == 4.0, "the new points replace the old ones, never add")
    assert(finish.board.size == 1)
  }

  test("rejudge of a pending submit and a second rejudge are no-ops") {
    val lc = fresh()
    lc.onArrived(rec(1, 1, "A", now))
    val before = lc.board
    assert(lc.onRejudge(1).contains(Delta.empty))
    assert(lc.onRejudge(1).contains(Delta.empty))
    assert(lc.board == before)
    lc.onFinished(1, compiled = true, Some(3.0))
    assert(cellOn(lc, 1, "A") == LiveCell(Some(3.0), 1, full = false, 0, None))
  }

  test("an in-flight submit is an attempt and a marker, never the best; NULL points and compile errors are no best") {
    val lc = fresh(Seq(rec(1, 1, "B", now).copy(state = pts(7)), rec(2, 1, "B", now.plusMinutes(1)),
      rec(3, 1, "B", now.plusMinutes(2)).copy(state = ce), rec(4, 1, "B", now.plusMinutes(3)).copy(state = Done(compiled = true, None))))
    assert(cellOn(lc, 1, "B") == LiveCell(Some(7.0), 4, full = false, 2, None))
    assert(lc.board.rowOf(1).get.total == 7.0)
  }

  test("a repeated arrival keeps the finished state and emits nothing") {
    val lc = fresh()
    lc.onArrived(rec(1, 1, "A", now))
    lc.onFinished(1, compiled = true, Some(10.0))
    val before = lc.board
    assert(lc.onArrived(rec(1, 1, "A", now)) == Delta.empty)
    assert(lc.onArrived(rec(1, 2, "B", now)) == Delta.empty)
    assert(lc.board == before)
    assert(lc.records == Seq(rec(1, 1, "A", now).copy(state = pts(10))))
  }

  test("finish before arrival: replayed row then the late arrival ends finished") {
    val a = fresh()
    assert(a.onFinished(1, compiled = true, Some(2.0)).isEmpty)
    a.onArrived(rec(1, 1, "A", now).copy(state = pts(2)))
    assert(a.onArrived(rec(1, 1, "A", now)) == Delta.empty)
    assert(a.records.map(_.state) == Seq(pts(2)))
    assert(cellOn(a, 1, "A").best.contains(2.0))
  }

  test("unknown id gives None; unknown team or problem gives an empty delta") {
    val lc = fresh()
    assert(lc.onFinished(42, compiled = true, Some(1.0)).isEmpty)
    assert(lc.onRejudge(42).isEmpty)
    val before = lc.board
    assert(lc.onArrived(rec(7, 99, "A", now)) == Delta.empty)
    assert(lc.onArrived(rec(8, 1, "Z", now)) == Delta.empty)
    assert(lc.board == before)
  }

  test("differences names the team") {
    val a = fresh(Seq(rec(1, 2, "A", now).copy(state = pts(5))))
    val b = fresh()
    assert(a.differences(a.rebuilt(problems, teams, a.records)).isEmpty)
    assert(a.differences(b).exists(_.contains("2")))
  }

  test("shared places over deltas: 1, 1, 3, then a finish breaks the tie") {
    val lc = fresh(ts = Seq(team(1), team(2), team(3)))
    Seq((1, 1, "A"), (2, 2, "A"), (3, 3, "B")).foreach { case (id, t, p) => lc.onArrived(rec(id, t, p, now)) }
    lc.onFinished(1, compiled = true, Some(10.0))
    lc.onFinished(2, compiled = true, Some(10.0))
    lc.onFinished(3, compiled = true, Some(5.0))
    assert(lc.board.rows.map(r => (r.team.id, r.place)) == Seq((1, Some(1)), (2, Some(1)), (3, Some(3))))
    lc.onArrived(rec(4, 3, "C", now.plusMinutes(5)))
    val d = lc.onFinished(4, compiled = true, Some(30.0)).get
    assert(lc.board.rows.map(r => (r.team.id, r.place)) == Seq((3, Some(1)), (1, Some(2)), (2, Some(2))))
    d.board match {
      case Seq(RowOut(c)) => assert(c.team == 3 && c.position == 0 && c.rankFrom == 0 && c.ranks == Seq(Some(1), Some(2), Some(2)))
      case other => fail(s"one row expected, got $other")
    }
  }

  test("team changes: not rated moves to the tail, disqualified and removed leave, a rename stays in place") {
    val lc = fresh(Seq(rec(1, 1, "A", now).copy(state = pts(10)), rec(2, 2, "A", now).copy(state = pts(5))))
    assert(lc.board.rows.map(_.team.id).take(2) == Seq(1, 2))
    val nr = lc.onTeams(teams.map(t => if (t.id == 1) t.copy(unrated = Some(NotRated)) else t))
    assert(nr.board.size == 1)
    assert(lc.board.rows.map(_.team.id) == Seq(2, 3, 4, 1, 5) && lc.board.rowOf(1).get.place.isEmpty)
    assert(lc.board.rowOf(2).get.place.contains(1))

    val renamed = lc.onTeams(lc.teamList.map(t => if (t.id == 2) t.copy(name = "New name") else t))
    renamed.board match {
      case Seq(RowOut(c)) => assert(c.team == 2 && c.row.team.name == "New name" && c.position == 0)
      case other => fail(s"one row expected, got $other")
    }

    val dq = lc.onTeams(lc.teamList.map(t => if (t.id == 2) t.copy(unrated = Some(Disqualified)) else t))
    assert(dq.board.collect { case WholeOut(b) => b.rowOf(2) } == Seq(None))
    assert(lc.board.rowOf(2).isEmpty && lc.board.rowOf(3).get.place.contains(1))

    val gone = lc.onTeams(lc.teamList.filterNot(_.id == 3))
    assert(gone.board.collect { case WholeOut(b) => b.rowOf(3) } == Seq(None))
    assert(lc.board.rowOf(3).isEmpty)

    // Back again: the kept submits bring its work back.
    lc.onTeams(lc.teamList.map(t => if (t.id == 2) t.copy(unrated = None) else t))
    assert(lc.board.rowOf(2).get.total == 5.0)
    assert(lc.differences(new LiveContest(contest, problems, lc.teamList, lc.records)).isEmpty)
  }

  // Applies a reported change to the previous snapshot; returns the reconstructed board.
  private def applyRow(prev: LiveBoard, c: RowChange, next: LiveBoard): LiveBoard = {
    val without = prev.rows.filterNot(_.team.id == c.team)
    val placed = without.patch(c.position, Seq(c.row), 0)
    val ranked = placed.zipWithIndex.map { case (r, i) =>
      if (i >= c.rankFrom && i < c.rankFrom + c.ranks.size) r.copy(place = c.ranks(i - c.rankFrom))
      else r.copy(place = prev.rows(i).place)
    }
    next.rows.indices.filterNot(i => i >= c.rankFrom && i < c.rankFrom + c.ranks.size).foreach { i =>
      assert(next.rows(i).place == prev.rows(i).place, s"place at $i changed outside reported range")
    }
    LiveBoard(prev.problems, ranked)
  }

  private class Stats { var down = 0; var up = 0; var ties = 0; var notRated = 0; var pending = 0 }

  private def checkOut(prev: LiveBoard, outs: Seq[BoardOut], next: LiveBoard, stats: Stats): Unit = {
    assert(outs.size <= 1)
    outs.headOption match {
      case None => assert(next == prev)
      case Some(WholeOut(b)) => assert(b == next)
      case Some(RowOut(c)) =>
        assert(applyRow(prev, c, next) == next)
        val oldPos = prev.rows.indexWhere(_.team.id == c.team)
        if (c.position > oldPos) stats.down += 1
        if (c.position < oldPos) stats.up += 1
    }
    val placed = next.rows.flatMap(_.place)
    if (placed.distinct.size < placed.size) stats.ties += 1
    if (next.rowOf(5).exists(_.cells.nonEmpty)) stats.notRated += 1
    if (next.rows.exists(_.cells.values.exists(_.pending > 0))) stats.pending += 1
  }

  test("property: incremental events equal a rebuild and the static points board after every step") {
    Seq(1L, 7L, 2026L).foreach { seed =>
      val rnd = new Random(seed)
      val stats = new Stats
      val lc = fresh()
      var nextId = 1
      (1 to 260).foreach { step =>
        val prev = lc.board
        val known = lc.records
        val delta = rnd.nextInt(10) match {
          case n if n < 4 || known.isEmpty =>
            val t = 1 + rnd.nextInt(5)
            val r = rec(nextId, t, problems(rnd.nextInt(problems.size)).id, now.plusSeconds(nextId))
            nextId += 1
            lc.onArrived(r)
          case n if n < 8 =>
            val inFlight = known.filter(_.state == live.Pending)
            val pool = if (inFlight.nonEmpty && rnd.nextInt(5) > 0) inFlight else known
            val r = pool(rnd.nextInt(pool.size))
            val max = problems.find(_.id == r.problem).get.maxPoints
            // coarse points so that equal totals (shared places) happen
            val p = rnd.nextInt(3) match {
              case 0 => None
              case 1 => Some(max)
              case _ => Some((rnd.nextInt(4) * 5).toDouble.min(max))
            }
            lc.onFinished(r.id, compiled = rnd.nextInt(8) > 0, p).get
          case _ =>
            lc.onRejudge(known(rnd.nextInt(known.size)).id).get
        }
        checkOut(prev, delta.board, lc.board, stats)
        val diffs = lc.differences(lc.rebuilt(problems, teams, lc.records))
        assert(diffs.isEmpty, s"seed $seed step $step: $diffs")
        // The live board ranks exactly as the static points board of the same submits.
        val static = MonitorPoints.build(problems, teams, lc.records.map(_.toSubmit))
        assert(lc.board.rows.map(r => (r.place, r.team.id, problems.map(p => r.cell(p.id).points), r.total)) ==
          static.rows.map(r => (r.place, r.team.id, r.cells, r.total)), s"seed $seed step $step")
      }
      assert(stats.down > 0 && stats.up > 0, s"seed $seed: no moves down/up")
      assert(stats.ties > 0, s"seed $seed: no ties")
      assert(stats.notRated > 0, s"seed $seed: not rated team never scored")
      assert(stats.pending > 0, s"seed $seed: nothing was ever in flight")
    }
  }
}
