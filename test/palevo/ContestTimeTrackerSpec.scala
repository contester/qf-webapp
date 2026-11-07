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

  // ---- reschedule ----

  private def moved(c: org.stingray.contester.dbmodel.Contest, start: DateTime = null, freeze: DateTime = null,
                    end: DateTime = null, expose: DateTime = null, name: String = null) =
    c.copy(name = Option(name).getOrElse(c.name), startTime = Option(start).getOrElse(c.startTime),
      freezeTime = Option(freeze).getOrElse(c.freezeTime), endTime = Option(end).getOrElse(c.endTime),
      exposeTime = Option(expose).getOrElse(c.exposeTime))

  private def resched(es: Seq[PalevoEvent]) = es.collect { case e: ContestRescheduled => e }

  test("a moved start is announced once, before anything else of the same observation") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(8, 30))
    val es = tr.observe(moved(contest(), start = t(11)), t(8, 40))
    assert(es.size == 1)
    val e = es.head.asInstanceOf[ContestRescheduled]
    assert(e.changes == Seq(TimeChange("start", t(10), t(11))) && e.ts == t(8, 40))
    assert(e.id == s"qf-contest_rescheduled-1-${t(8, 40).getMillis}")
    assert(tr.observe(moved(contest(), start = t(11)), t(8, 41)).isEmpty)
  }

  test("shifting all four times is one event listing start, freeze and end") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(8))
    val shifted = moved(contest(), start = t(11, 30), freeze = t(15, 30), end = t(16, 30), expose = t(17, 30))
    val es = tr.observe(shifted, t(8, 5))
    assert(es.size == 1)
    assert(es.head.asInstanceOf[ContestRescheduled].changes ==
      Seq(TimeChange("start", t(10), t(11, 30)), TimeChange("freeze", t(14), t(15, 30)), TimeChange("end", t(15), t(16, 30))))
  }

  test("an extended end of a running contest is announced with only the end") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(12))
    val es = tr.observe(moved(contest(), end = t(16)), t(12, 5))
    assert(resched(es).map(_.changes) == Seq(Seq(TimeChange("end", t(15), t(16)))) && es.size == 1)
  }

  test("the same instants in another zone are not a reschedule") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(8))
    val zone = org.joda.time.DateTimeZone.forOffsetHours(3)
    val rezoned = contest().copy(startTime = t(10).withZone(zone), freezeTime = t(14).withZone(zone),
      endTime = t(15).withZone(zone), exposeTime = t(16).withZone(zone))
    assert(tr.observe(rezoned, t(8, 1)).isEmpty)
  }

  test("expose-only and name-only changes are silent") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(8))
    assert(tr.observe(moved(contest(), expose = t(18)), t(8, 1)).isEmpty)
    assert(tr.observe(moved(contest(), expose = t(18), name = "Renamed"), t(8, 2)).isEmpty)
  }

  test("freeze is listed only when freeze is enabled before or after the change") {
    val disabled = contest(freeze = t(15), end = t(15))
    val tr = new ContestTimeTracker
    tr.observe(disabled, t(8))
    assert(tr.observe(moved(disabled, freeze = t(15, 30)), t(8, 1)).isEmpty) // still disabled
    val tr2 = new ContestTimeTracker
    tr2.observe(contest(), t(8))
    val off = tr2.observe(moved(contest(), freeze = t(15)), t(8, 1)) // enabled -> disabled
    assert(resched(off).map(_.changes) == Seq(Seq(TimeChange("freeze", t(14), t(15)))))
  }

  test("after a reschedule the countdown runs again against the new start, with new ids") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(8, 30))
    val first = tr.observe(moved(contest(), start = t(11)), t(8, 40))
    assert(ahead(first).isEmpty)
    val es = (1 to 140).flatMap(i => tr.observe(moved(contest(), start = t(11)), t(8, 40).plusMinutes(i)))
    assert(ahead(es.take(es.size)) == Seq(60, 30, 20, 10, 5, 2, 1).map(("contest_starting", _)))
    assert(es.collect { case e: ContestStarting if e.inMinutes > 0 => e.at.get }.forall(_ == t(11)))
  }

  test("a start moved earlier past a threshold marks it silently; later ones still fire") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(9, 30))
    val es = tr.observe(moved(contest(), start = t(9, 56)), t(9, 50))
    assert(es.size == 1 && es.head.isInstanceOf[ContestRescheduled]) // 6 minutes left: 10 and larger are marked
    assert(ahead(tr.observe(moved(contest(), start = t(9, 56)), ts(9, 50, 30))).isEmpty)
    assert(ahead(tr.observe(moved(contest(), start = t(9, 56)), t(9, 51))) == Seq(("contest_starting", 5)))
    assert(ahead(tr.observe(moved(contest(), start = t(9, 56)), t(9, 54))) == Seq(("contest_starting", 2)))
  }

  test("a start moved after the contest began is announced but not started again") {
    val tr = new ContestTimeTracker
    tr.observe(contest(), t(10, 30))
    val es = tr.observe(moved(contest(), start = t(11)), t(10, 40))
    assert(types(es) == Seq("contest_rescheduled"))
    val later = (41 to 90).flatMap(m => tr.observe(moved(contest(), start = t(11)), t(10, 0).plusMinutes(m)))
    assert(later.isEmpty)
  }

  test("contests above the bound are not announced when rescheduled; contests are independent") {
    val tr = new ContestTimeTracker
    tr.observe(contest(11), t(8))
    tr.observe(contest(1), t(8))
    assert(tr.observe(moved(contest(11), start = t(11)), t(8, 1)).isEmpty)
    assert(resched(tr.observe(moved(contest(1), start = t(11)), t(8, 1))).size == 1)
    assert(tr.observe(contest(2), t(8, 2)).isEmpty) // first sight
  }
}

