package palevo

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Contest

class ContestPhaseTrackerSpec extends AnyFunSuite {
  private def t(h: Int, m: Int = 0) = new DateTime(2026, 11, 1, h, m)

  // starts 10:00, freezes 14:00, ends 15:00
  private def contest(id: Int = 1, freeze: DateTime = t(14), end: DateTime = t(15)) =
    Contest(id, s"C$id", t(10), freeze, end, t(16), None, "en", schoolMode = false)

  private def types(es: Seq[PalevoEvent]) = es.map(_.eventType)

  test("first observation is silent, even mid-contest and after the end") {
    for (now <- Seq(t(9), t(12), t(14, 30), t(18))) {
      assert(new ContestPhaseTracker().observe(contest(), now).isEmpty, s"at $now")
    }
  }

  test("announces start when time crosses") {
    val tr = new ContestPhaseTracker
    tr.observe(contest(), t(9, 59))
    assert(types(tr.observe(contest(), t(10))) == Seq("contest_starting"))
  }

  test("start, freeze, finish in order across observations") {
    val tr = new ContestPhaseTracker
    tr.observe(contest(), t(9))
    assert(types(tr.observe(contest(), t(10, 1))) == Seq("contest_starting"))
    assert(tr.observe(contest(), t(12)).isEmpty)
    assert(types(tr.observe(contest(), t(14, 1))) == Seq("contest_freezing"))
    assert(types(tr.observe(contest(), t(15, 1))) == Seq("contest_ending"))
    assert(tr.observe(contest(), t(17)).isEmpty)
  }

  test("skipped phases are all announced in order") {
    val tr = new ContestPhaseTracker
    tr.observe(contest(), t(9))
    assert(types(tr.observe(contest(), t(15, 30))) == Seq("contest_starting", "contest_freezing", "contest_ending"))
  }

  test("no frozen event when freezeTime >= endTime") {
    val tr = new ContestPhaseTracker
    val c = contest(freeze = t(15), end = t(15))
    tr.observe(c, t(9))
    assert(types(tr.observe(c, t(16))) == Seq("contest_starting", "contest_ending"))
  }

  test("repeat observation announces nothing") {
    val tr = new ContestPhaseTracker
    tr.observe(contest(), t(9))
    tr.observe(contest(), t(11))
    assert(tr.observe(contest(), t(11)).isEmpty)
  }

  test("contests are tracked independently") {
    val tr = new ContestPhaseTracker
    tr.observe(contest(1), t(9))
    assert(tr.observe(contest(2), t(11)).isEmpty) // first sight of contest 2
    val es = tr.observe(contest(1), t(11))
    assert(types(es) == Seq("contest_starting") && es.head.contest == PContest(1, "C1"))
  }

  test("events carry the observation time") {
    val tr = new ContestPhaseTracker
    tr.observe(contest(), t(9))
    assert(tr.observe(contest(), t(10, 5)).head.ts == t(10, 5))
  }
}
