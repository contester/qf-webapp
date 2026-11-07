package live

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, LocalTeam, Team}

import scala.util.Random

class LiveContestSpec extends AnyFunSuite {
  import LiveCell._

  private val start = new DateTime(2026, 11, 1, 10, 0)
  private val freeze = start.plusHours(4)
  private val end = start.plusHours(5)
  private val expose = start.plusHours(6)
  private val contest = Contest(1, "Test", start, freeze, end, expose, None, "ru", schoolMode = false)
  private val problems = Seq("A", "B", "C", "D")

  private def team(id: Int, notRated: Boolean = false): Team =
    LocalTeam(id, 1, f"School$id%02d", s"S$id", 0, "", notRated, noPrint = false, disabled = false)

  private val teams = Seq(team(1), team(2), team(3), team(4), team(5, notRated = true))

  private val ok = Done(compiled = true, 3, 3)
  private val fail = Done(compiled = true, 1, 3)

  private def rec(id: Int, t: Int, p: String, at: DateTime): SubmitRec =
    SubmitRec(id, t, p, at, ((at.getMillis - start.getMillis) / 1000).toInt, live.Pending)

  private def fresh(now: DateTime, subs: Seq[SubmitRec] = Nil) = new LiveContest(contest, problems, teams, subs, now)

  private def cellOn(lc: LiveContest, kind: BoardKind, t: Int, p: String): LiveCell =
    lc.board(kind).rowOf(t).get.cells.getOrElse(p, Empty)

  private val emptyDelta = Delta(Nil, Nil, Nil)

  test("arrive is pending on both boards, finish success solves") {
    val now = start.plusHours(1)
    val lc = fresh(now)
    val d = lc.onArrived(rec(1, 1, "A", now), now)
    assert(d.freezing.size == 1 && d.real.size == 1 && d.ownCells.isEmpty)
    assert(cellOn(lc, Real, 1, "A") == Pending(0))
    assert(cellOn(lc, Freezing, 1, "A") == Pending(0))
    val f = lc.onFinished(1, ok.compiled, ok.passed, ok.taken, now.plusMinutes(1)).get
    assert(f.freezing.size == 1 && f.real.size == 1)
    assert(cellOn(lc, Real, 1, "A") == Solved(1, 3600))
    assert(cellOn(lc, Freezing, 1, "A") == Solved(1, 3600))
    assert(lc.board(Real).rows.head.team.id == 1)
  }

  test("post-freeze finish changes the real board only and yields own cells; expose tick equalizes") {
    val at = freeze.plusMinutes(10)
    val lc = fresh(at)
    val d = lc.onArrived(rec(1, 2, "B", at), at)
    assert(d.freezing.size == 1 && d.real.size == 1)
    val f = lc.onFinished(1, ok.compiled, ok.passed, ok.taken, at.plusMinutes(1)).get
    assert(f.freezing.isEmpty)
    assert(f.real.size == 1)
    assert(f.ownCells == Seq(2 -> Map("B" -> Solved(1, 4 * 3600 + 600))))
    assert(cellOn(lc, Freezing, 2, "B") == Pending(0))
    assert(cellOn(lc, Real, 2, "B") == Solved(1, 4 * 3600 + 600))
    assert(lc.board(Freezing) != lc.board(Real))

    assert(lc.onTick(expose.minusMinutes(1)).isEmpty)
    val t = lc.onTick(expose.plusSeconds(1)).get
    assert(lc.board(Freezing) == lc.board(Real))
    assert(t.freezing == Seq(WholeOut(lc.board(Freezing))))
    assert(t.real == Seq(WholeOut(lc.board(Real))))
    assert(t.ownCells.isEmpty)
    assert(lc.onTick(expose.plusSeconds(2)).isEmpty)
  }

  test("tick crossing start and freeze emits whole boards") {
    val lc = fresh(start.minusMinutes(5))
    assert(lc.onTick(start.minusMinutes(1)).isEmpty)
    val s = lc.onTick(start.plusSeconds(1)).get
    assert(s.freezing == Seq(WholeOut(lc.board(Freezing))) && s.real == Seq(WholeOut(lc.board(Real))))
    assert(lc.onTick(freeze.minusSeconds(1)).isEmpty)
    assert(lc.onTick(freeze.plusSeconds(1)).nonEmpty)
  }

  test("rejudge of a finished submit sets it pending, finish sets it back") {
    val now = start.plusHours(1)
    val lc = fresh(now, Seq(rec(1, 1, "A", now).copy(state = ok)))
    assert(cellOn(lc, Real, 1, "A") == Solved(1, 3600))
    val r = lc.onRejudge(1, now).get
    assert(r.real.size == 1 && r.freezing.size == 1)
    assert(cellOn(lc, Real, 1, "A") == Pending(0))
    assert(lc.records.map(_.state) == Seq(live.Pending))
    lc.onFinished(1, ok.compiled, ok.passed, ok.taken, now)
    assert(cellOn(lc, Real, 1, "A") == Solved(1, 3600))
  }

  test("rejudge of a pending submit and a second rejudge are no-ops") {
    val now = start.plusHours(1)
    val lc = fresh(now)
    lc.onArrived(rec(1, 1, "A", now), now)
    val before = (lc.board(Freezing), lc.board(Real))
    assert(lc.onRejudge(1, now).contains(emptyDelta))
    assert(lc.onRejudge(1, now).contains(emptyDelta))
    assert((lc.board(Freezing), lc.board(Real)) == before)
    assert(lc.records.map(_.state) == Seq(live.Pending))
    lc.onFinished(1, fail.compiled, fail.passed, fail.taken, now)
    assert(lc.records.map(_.state) == Seq(fail))
    assert(cellOn(lc, Real, 1, "A") == Failed(1))
  }

  test("a repeated arrival keeps the finished state and emits nothing") {
    val now = start.plusHours(1)
    val lc = fresh(now)
    lc.onArrived(rec(1, 1, "A", now), now)
    lc.onFinished(1, ok.compiled, ok.passed, ok.taken, now)
    val before = (lc.board(Freezing), lc.board(Real))
    assert(lc.onArrived(rec(1, 1, "A", now), now) == emptyDelta)
    assert(lc.onArrived(rec(1, 2, "B", now), now) == emptyDelta)
    assert((lc.board(Freezing), lc.board(Real)) == before)
    assert(lc.records == Seq(rec(1, 1, "A", now).copy(state = ok)))
    assert(cellOn(lc, Real, 1, "A") == Solved(1, 3600))
  }

  test("finish before arrival: replayed row then the late arrival ends finished") {
    val now = start.plusHours(1)
    // replay as a loaded Done row
    val a = fresh(now)
    assert(a.onFinished(1, fail.compiled, fail.passed, fail.taken, now).isEmpty)
    a.onArrived(rec(1, 1, "A", now).copy(state = fail), now)
    assert(a.onArrived(rec(1, 1, "A", now), now) == emptyDelta)
    assert(a.records.map(_.state) == Seq(fail))
    assert(cellOn(a, Real, 1, "A") == Failed(1))
    // replay as onArrived + onFinished
    val b = fresh(now)
    assert(b.onFinished(1, ok.compiled, ok.passed, ok.taken, now).isEmpty)
    b.onArrived(rec(1, 1, "A", now), now)
    b.onFinished(1, ok.compiled, ok.passed, ok.taken, now)
    assert(b.onArrived(rec(1, 1, "A", now), now) == emptyDelta)
    assert(b.records.map(_.state) == Seq(ok))
    assert(cellOn(b, Real, 1, "A") == Solved(1, 3600))
  }

  test("unknown id gives None; unknown team gives an empty delta") {
    val now = start.plusHours(1)
    val lc = fresh(now)
    assert(lc.onFinished(42, compiled = true, 1, 1, now).isEmpty)
    assert(lc.onRejudge(42, now).isEmpty)
    val before = (lc.board(Freezing), lc.board(Real))
    assert(lc.onArrived(rec(7, 99, "A", now), now) == emptyDelta)
    assert((lc.board(Freezing), lc.board(Real)) == before)
  }

  test("onContest with a later freeze reveals a hidden submit from memory") {
    val at = freeze.plusMinutes(30)
    val now = freeze.plusMinutes(40)
    val lc = fresh(now, Seq(rec(1, 3, "C", at).copy(state = ok)))
    assert(cellOn(lc, Freezing, 3, "C") == Pending(0))
    val d = lc.onContest(contest.copy(freezeTime = freeze.plusMinutes(45)), now)
    assert(lc.contest.freezeTime == freeze.plusMinutes(45))
    assert(lc.rules == FreezeRules(freeze.plusMinutes(45), end, expose))
    assert(cellOn(lc, Freezing, 3, "C") == Solved(1, 4 * 3600 + 1800))
    assert(d.freezing == Seq(WholeOut(lc.board(Freezing))))
    assert(d.real == Seq(WholeOut(lc.board(Real))))
  }

  test("differences names board and team") {
    val now = start.plusHours(1)
    val a = fresh(now, Seq(rec(1, 2, "A", now).copy(state = ok)))
    val b = fresh(now)
    assert(a.differences(a.rebuilt(problems, teams, a.records, now)).isEmpty)
    val diffs = a.differences(b)
    assert(diffs.nonEmpty)
    assert(diffs.exists(m => m.contains("Freezing") && m.contains("2")))
    assert(diffs.exists(m => m.contains("Real") && m.contains("2")))
  }

  // Applies a reported change to the previous snapshot; returns the reconstructed board.
  private def applyRow(prev: LiveBoard, c: RowChange, next: LiveBoard): LiveBoard = {
    val without = prev.rows.filterNot(_.team.id == c.team)
    val placed = without.patch(c.position, Seq(c.row), 0)
    val ranked = placed.zipWithIndex.map { case (r, i) =>
      if (i >= c.rankFrom && i < c.rankFrom + c.ranks.size) r.copy(rank = c.ranks(i - c.rankFrom))
      else r.copy(rank = prev.rows(i).rank)
    }
    // positions outside the reported range keep their previous ranks
    next.rows.indices.filterNot(i => i >= c.rankFrom && i < c.rankFrom + c.ranks.size).foreach { i =>
      assert(next.rows(i).rank == prev.rows(i).rank, s"rank at $i changed outside reported range")
    }
    LiveBoard(prev.problems, ranked, prev.solvedCounts ++ c.solved)
  }

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
    assert(LiveBoard.build(problems, teams, next.rows.map(r => r.team.id -> r.cells).toMap) == next)
    val rated = next.rows.filter(_.rank > 0).map(_.rank)
    if (rated.distinct.size < rated.size) stats.ties += 1
    if (next.rowOf(5).exists(_.cells.nonEmpty)) stats.notRated += 1
  }

  private class Stats { var down = 0; var up = 0; var ties = 0; var notRated = 0; var firstOf4 = 0; var hidden = 0; var own = 0 }

  test("property: incremental events equal a rebuild after every step") {
    Seq(1L, 7L, 2026L).foreach { seed =>
      val rnd = new Random(seed)
      val stats = new Stats
      var clock = freeze.minusMinutes(50)
      val lc = fresh(clock)
      var nextId = 1
      (1 to 260).foreach { step =>
        clock = clock.plusSeconds(rnd.nextInt(60))
        if (step == 230) clock = expose.plusSeconds(1)
        val prev = (lc.board(Freezing), lc.board(Real))
        val known = lc.records
        val teamPool = if (step < 120) Seq(1, 2, 3, 5) else Seq(1, 2, 3, 4, 5)
        val delta = rnd.nextInt(10) match {
          case n if n < 4 || known.isEmpty =>
            val t = teamPool(rnd.nextInt(teamPool.size))
            // coarse arrival instants straddling the freeze so equal penalties (ties) happen
            val at0 = freeze.plusMinutes(10 * (rnd.nextInt(7) - 3))
            val at = if (at0.isAfter(clock)) clock else at0
            if (t == 4 && !lc.records.exists(_.team == 4)) stats.firstOf4 += 1
            val r = rec(nextId, t, problems(rnd.nextInt(problems.size)), at)
            nextId += 1
            lc.onArrived(r, clock)
          case n if n < 9 =>
            // mostly finish submits still being judged, so cells do not stay blocked by pending ones
            val pending = known.filter(_.state == live.Pending)
            val pool = if (pending.nonEmpty && rnd.nextInt(5) > 0) pending else known
            val r = pool(rnd.nextInt(pool.size))
            val s = if (rnd.nextInt(3) == 0) fail else ok
            lc.onFinished(r.id, s.compiled, s.passed, s.taken, clock).get
          case _ =>
            lc.onRejudge(known(rnd.nextInt(known.size)).id, clock).get
        }
        checkOut(prev._1, delta.freezing, lc.board(Freezing), stats)
        checkOut(prev._2, delta.real, lc.board(Real), stats)
        val diffs = lc.differences(lc.rebuilt(problems, teams, lc.records, clock))
        assert(diffs.isEmpty, s"seed $seed step $step: $diffs")
        if (step < 230 && lc.board(Freezing) != lc.board(Real)) stats.hidden += 1
        if (delta.ownCells.nonEmpty) stats.own += 1
      }
      assert(stats.down > 0 && stats.up > 0, s"seed $seed: no moves down/up")
      assert(stats.ties > 0, s"seed $seed: no ties")
      assert(stats.notRated > 0, s"seed $seed: not rated team never scored")
      assert(stats.firstOf4 > 0, s"seed $seed: team 4 never got a first submit")
      assert(stats.hidden > 0 && stats.own > 0, s"seed $seed: the freeze never hid anything")
      assert(lc.board(Freezing) == lc.board(Real))
    }
  }

  test("the public board has no jury teams, keeps coach teams, and its header counts and updates follow") {
    val jury = LocalTeam(8, 1, models.InitialImportTools.JurySchoolFullName, "J", 1, "", notRated = true, noPrint = false, disabled = false)
    val coach = LocalTeam(9, 1, models.InitialImportTools.CoachSchoolFullName, "C", 1, "", notRated = true, noPrint = false, disabled = false)
    val now = start.plusHours(1)
    val lc = new LiveContest(contest, problems, teams ++ Seq(jury, coach), Nil, now)
    assert(!lc.publicBoard.rows.exists(_.team.id == 8) && lc.publicBoard.rows.exists(_.team.id == 9))
    // A jury team that is rated stays on the public board.
    val rated = jury.copy(id = 7, notRated = false)
    val withRated = new LiveContest(contest, problems, teams ++ Seq(jury, coach, rated), Nil, now)
    assert(withRated.publicBoard.rows.exists(_.team.id == 7) && !withRated.publicBoard.rows.exists(_.team.id == 8))
    // Rating it later puts it on, un-rating takes it off.
    val lc2 = new LiveContest(contest, problems, teams ++ Seq(jury), Nil, now)
    assert(lc2.onTeamChanged(jury.copy(notRated = false), now).public.nonEmpty && lc2.publicBoard.rows.exists(_.team.id == 8))
    lc2.onTeamChanged(jury, now)
    assert(!lc2.publicBoard.rows.exists(_.team.id == 8))
    assert(lc.board(Freezing).rows.exists(_.team.id == 8))

    val dj = lc.onArrived(rec(1, 8, "A", now), now)
    assert(dj.freezing.size == 1 && dj.public.isEmpty)
    lc.onFinished(1, ok.compiled, ok.passed, ok.taken, now.plusMinutes(1))
    val dc = lc.onArrived(rec(2, 9, "A", now), now)
    lc.onFinished(2, ok.compiled, ok.passed, ok.taken, now.plusMinutes(1))
    assert(lc.board(Freezing).solvedCounts("A") == 2 && lc.publicBoard.solvedCounts("A") == 1)
    val d = lc.onArrived(rec(3, 1, "A", now), now)
    assert(d.public.size == 1)
    assert(lc.differences(lc.rebuilt(problems, teams ++ Seq(jury, coach), lc.records, now.plusMinutes(2))).isEmpty)
    val t = lc.onTeamChanged(teams.head, now)
    assert(t.public == Seq(WholeOut(lc.publicBoard)) && !lc.publicBoard.rows.exists(_.team.id == 8))
  }
}
