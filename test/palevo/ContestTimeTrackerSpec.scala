package palevo

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Contest

class ContestTimeTrackerSpec extends AnyFunSuite {
  private def t(h: Int, m: Int = 0) = new DateTime(2026, 11, 1, h, m)

  // starts 10:00, freezes 14:00, ends 15:00
  private def contest(id: Int = 1, freeze: DateTime = t(14), end: DateTime = t(15)) =
    Contest(id, s"C$id", t(10), freeze, end, t(16), None, "en", schoolMode = false)

  private def types(es: Seq[PalevoEvent]) = es.map(_.eventType)

  test("first observation is silent, even mid-contest and after the end") {
    for (now <- Seq(t(9), t(12), t(14, 30), t(18))) {
      assert(new ContestTimeTracker().observe(contest(), now).isEmpty, s"at $now")
    }
  }

  test("announces start when time crosses") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(9, 59))
    assert(types(tr.observe(contest(), t(10))) == Seq("contest_starting"))
  }

  test("start, freeze, finish in order across observations") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(9))
    assert(types(tr.observe(contest(), t(10, 1))) == Seq("contest_starting"))
    assert(tr.observe(contest(), t(12)).isEmpty)
    assert(types(tr.observe(contest(), t(14, 1))) == Seq("contest_freezing"))
    assert(types(tr.observe(contest(), t(15, 1))) == Seq("contest_ending"))
    assert(tr.observe(contest(), t(17)).isEmpty)
  }

  test("skipped phases are all announced in order") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(9))
    assert(types(tr.observe(contest(), t(15, 30))) == Seq("contest_starting", "contest_freezing", "contest_ending"))
  }

  test("no frozen event when freezeTime >= endTime") {
    val tr = new ContestTimeTracker
    val c = contest(freeze = t(15), end = t(15))
    tr.observe(c, t(9))
    assert(types(tr.observe(c, t(16))) == Seq("contest_starting", "contest_ending"))
  }

  test("repeat observation announces nothing") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(9))
    tr.observe(contest(), t(11))
    assert(tr.observe(contest(), t(11)).isEmpty)
  }

  test("contests are tracked independently") {
    val tr = new ContestTimeTracker
    tr.observe(contest(1), t(9))
    assert(tr.observe(contest(2), t(11)).isEmpty) // first sight of contest 2
    val es = tr.observe(contest(1), t(11))
    assert(types(es) == Seq("contest_starting") && es.head.contest == PContest(1, "C1"))
  }

  test("events carry the observation time") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(9))
    assert(tr.observe(contest(), t(10, 5)).head.ts == t(10, 5))
  }

  // ---- countdown ----

  private def ts(h: Int, m: Int, sec: Int) = new DateTime(2026, 11, 1, h, m, sec)

  private def ahead(es: Seq[PalevoEvent]): Seq[(String, Int)] = es.collect {
    case e: ContestStarting if e.inMinutes > 0 => (e.eventType, e.inMinutes)
    case e: ContestFreezing if e.inMinutes > 0 => (e.eventType, e.inMinutes)
    case e: ContestEnding if e.inMinutes > 0 => (e.eventType, e.inMinutes)
  }

  /** Observes `c` at `from`, then every minute up to and including `to`; everything produced, in order. */
  private def run(tr: ContestTimeTracker, c: org.stingray.contester.dbmodel.Contest, from: DateTime, to: DateTime): Seq[PalevoEvent] = {
    val out = Seq.newBuilder[PalevoEvent]
    out ++= tr.observe(c, from)
    var now = from.plusMinutes(1)
    while (!now.isAfter(to)) {
      out ++= tr.observe(c, now)
      now = now.plusMinutes(1)
    }
    out.result()
  }

  test("every start threshold fires once, then the start itself") {
    val es = run(new ContestTimeTracker, contest(), t(8, 50), t(10))
    assert(ahead(es) == Seq(60, 30, 20, 10, 5, 2, 1).map(("contest_starting", _)))
    val last = es.last.asInstanceOf[ContestStarting]
    assert(last.inMinutes == 0 && es.count(_.eventType == "contest_starting") == 8)
  }

  test("a restart mid-countdown is silent and only the later thresholds fire") {
    val tr = new ContestTimeTracker
    assert(tr.observe(contest(), t(9, 53)).isEmpty)
    assert(ahead(tr.observe(contest(), t(9, 54))).isEmpty)
    assert(ahead(tr.observe(contest(), t(9, 55))) == Seq(("contest_starting", 5)))
    assert(ahead(tr.observe(contest(), t(9, 58))) == Seq(("contest_starting", 2)))
    assert(ahead(tr.observe(contest(), t(9, 59))) == Seq(("contest_starting", 1)))
  }

  test("a threshold fires exactly when the remaining time reaches it") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(9, 50))
    assert(tr.observe(contest(), ts(9, 54, 59)).isEmpty)
    assert(ahead(tr.observe(contest(), t(9, 55))) == Seq(("contest_starting", 5)))
  }

  test("a late tick that crosses several thresholds fires only the smallest") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(9))
    assert(ahead(tr.observe(contest(), t(9, 56))) == Seq(("contest_starting", 5)))
    assert(ahead(tr.observe(contest(), t(9, 59))) == Seq(("contest_starting", 1)))
  }

  test("the moment itself is announced as the phase, not as a countdown") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(9, 59))
    val es = tr.observe(contest(), t(10))
    assert(es.size == 1 && es.head.asInstanceOf[ContestStarting].inMinutes == 0)
  }

  test("no start countdown once the contest has started") {
    val es = run(new ContestTimeTracker, contest(), t(10, 30), t(10, 59))
    assert(es.isEmpty)
  }

  test("the end countdown runs between start and end only") {
    val tr = new ContestTimeTracker
    val before = run(tr, contest(), t(8, 50), t(9, 59))
    assert(ahead(before).forall(_._1 == "contest_starting"))
    val es = run(new ContestTimeTracker, contest(), t(14, 30), t(15))
    assert(ahead(es) == Seq(20, 10, 5, 1).map(("contest_ending", _)))
    assert(es.last.asInstanceOf[ContestEnding].inMinutes == 0)
  }

  test("a contest whose end is not after its start gets no end countdown and no exception") {
    val c = contest(freeze = t(10), end = t(10))
    val es = run(new ContestTimeTracker, c, t(9), t(10, 30))
    assert(ahead(es).forall(_._1 == "contest_starting"))
    assert(types(es).count(_ == "contest_ending") == 1)
    val inverted = contest(freeze = t(9), end = t(9, 30))
    assert(run(new ContestTimeTracker, inverted, t(8), t(11)).nonEmpty)
  }

  test("contests above the bound get phase events but no countdown") {
    val above = run(new ContestTimeTracker, contest(11), t(8, 50), t(10))
    assert(types(above) == Seq("contest_starting") && ahead(above).isEmpty)
    val at = run(new ContestTimeTracker, contest(10), t(8, 50), t(10))
    assert(ahead(at).nonEmpty)
    assert(ContestTimeTracker.MaxAnnouncedContest == 10)
  }

  test("thresholds come from the configuration; empty lists disable the countdown") {
    assert(ahead(run(new ContestTimeTracker(CountdownConfig(Seq(3), Nil)), contest(), t(9, 50), t(10))) ==
      Seq(("contest_starting", 3)))
    val off = new ContestTimeTracker(CountdownConfig(Nil, Nil))
    assert(ahead(run(off, contest(), t(8, 50), t(10))).isEmpty && ahead(run(off, contest(2), t(14, 30), t(15))).isEmpty)
  }

  test("a countdown event names the target time and has an id with it") {
    val es = run(new ContestTimeTracker, contest(), t(8, 50), t(9, 50))
    val e = es.collect { case e: ContestStarting if e.inMinutes == 10 => e }.head
    assert(e.at.contains(t(10)) && e.id == s"qf-contest_starting-1-in10-${t(10).getMillis}")
    assert(e.ts == t(9, 50))
  }
}

