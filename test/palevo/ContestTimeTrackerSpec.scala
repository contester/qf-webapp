package palevo

import actors.RoundClock
import actors.RoundClock.{CountdownToEnd, CountdownToStart, Ended, Started}
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Contest

/**
  * Palevo's phase, countdown and reschedule events. The mapping is tested on its own; the timing (each threshold once,
  * restarts, late wakes) is tested by running the round clock's pure state with the tracker, as StatusActor does.
  * There is no freeze: a round has a start and an end only.
  */
class ContestTimeTrackerSpec extends AnyFunSuite {
  private def t(h: Int, m: Int = 0) = new DateTime(2026, 11, 1, h, m)

  // starts 10:00, ends 15:00
  private def contest(id: Int = 1, start: DateTime = t(10), end: DateTime = t(15)) =
    Contest(id, s"C$id", start, end, None, 1)

  private def types(es: Seq[PalevoEvent]) = es.map(_.eventType)

  private def ahead(es: Seq[PalevoEvent]): Seq[(String, Int)] = es.collect {
    case e: ContestStarting if e.inMinutes > 0 => (e.eventType, e.inMinutes)
    case e: ContestEnding if e.inMinutes > 0 => (e.eventType, e.inMinutes)
  }

  /** The round clock and the tracker together, as StatusActor runs them; `to` moves the clock and wakes it. */
  private class Sim(rounds: Seq[Contest], from: DateTime, cfg: CountdownConfig = CountdownConfig.Default) {
    private var state = RoundClock.State.load(rounds, from, ContestTimeTracker.countdownOf(cfg))
    private var now = from

    /** Wakes at `at` only (a late timer). */
    def wake(at: DateTime): Seq[PalevoEvent] = {
      now = at
      val (edges, after) = state.due(at)
      state = after
      edges.flatMap(e => ContestTimeTracker.onEdge(e, state.rounds(e.roundId), at))
    }

    /** Wakes at every deadline up to and including `to`, as the exact timer does. */
    def run(to: DateTime): Seq[PalevoEvent] = {
      val out = Seq.newBuilder[PalevoEvent]
      var more = true
      while (more) state.next(now) match {
        case Some((d, _)) if !d.isAfter(to) => out ++= wake(if (d.isAfter(now)) d else now)
        case _ => more = false
      }
      now = to
      out.result()
    }

    def change(c: Contest, at: DateTime): Seq[PalevoEvent] = {
      now = at
      val old = state.rounds.get(c.id)
      state = state.changed(c, at)
      old.flatMap(ContestTimeTracker.rescheduled(_, c, at)).toSeq ++ wake(at)
    }
  }

  // ---- the mapping ----

  test("start and end edges are the two phase events; there is no freeze event") {
    val c = contest()
    assert(ContestTimeTracker.onEdge(Started(1), c, t(10)).contains(ContestStarting(PContest(1, "C1"), t(10))))
    assert(ContestTimeTracker.onEdge(Ended(1), c, t(15)).contains(ContestEnding(PContest(1, "C1"), t(15))))
  }

  test("countdown edges name the target time and carry an id with it") {
    val c = contest()
    val e = ContestTimeTracker.onEdge(CountdownToStart(1, 10), c, t(9, 50)).get.asInstanceOf[ContestStarting]
    assert(e.inMinutes == 10 && e.at.contains(t(10)) && e.ts == t(9, 50))
    assert(e.id == s"qf-contest_starting-1-in10-${t(10).getMillis}")
    val end = ContestTimeTracker.onEdge(CountdownToEnd(1, 5), c, t(14, 55)).get.asInstanceOf[ContestEnding]
    assert(end.inMinutes == 5 && end.at.contains(t(15)))
  }

  test("a reschedule lists only start and end; same instants, names and is_finished are silent") {
    val c = contest()
    val es = ContestTimeTracker.rescheduled(c, c.copy(startTime = t(11, 30), endTime = t(16, 30)), t(8, 5))
    assert(es.map(_.asInstanceOf[ContestRescheduled].changes).contains(
      Seq(TimeChange("start", t(10), t(11, 30)), TimeChange("end", t(15), t(16, 30)))))
    val zone = org.joda.time.DateTimeZone.forOffsetHours(3)
    assert(ContestTimeTracker.rescheduled(c, c.copy(startTime = t(10).withZone(zone), endTime = t(15).withZone(zone)), t(8)).isEmpty)
    assert(ContestTimeTracker.rescheduled(c, c.copy(name = "Renamed", isFinished = true), t(8)).isEmpty)
  }

  // ---- with the round clock ----

  test("phases are announced in order and once: starting, then ending") {
    val sim = new Sim(Seq(contest()), t(9, 59), CountdownConfig(Nil, Nil))
    assert(types(sim.run(t(10, 1))) == Seq("contest_starting"))
    assert(sim.run(t(12)).isEmpty)
    assert(types(sim.run(t(15, 1))) == Seq("contest_ending"))
    assert(sim.run(t(17)).isEmpty)
  }

  test("first sight is silent, even mid-contest and after the end") {
    for (now <- Seq(t(10), t(12), t(14, 59), t(18))) {
      assert(new Sim(Seq(contest()), now).wake(now).isEmpty, s"at $now")
    }
  }

  // Restored (deleted in Task 10).
  test("contests are tracked independently") {
    val sim = new Sim(Seq(contest(1), contest(2, start = t(9))), t(9, 30), CountdownConfig(Nil, Nil)) // 2 is running
    val es = sim.run(t(11))
    assert(types(es) == Seq("contest_starting") && es.head.contest == PContest(1, "C1"))
  }

  // Restored (deleted in Task 10).
  test("every start threshold fires once, then the start itself") {
    val es = new Sim(Seq(contest()), t(8, 50)).run(t(10))
    assert(ahead(es) == Seq(60, 30, 20, 10, 5, 2, 1).map(("contest_starting", _)))
    val last = es.last.asInstanceOf[ContestStarting]
    assert(last.inMinutes == 0 && es.count(_.eventType == "contest_starting") == 8)
  }

  test("the end countdown runs between start and end, then the end itself") {
    val es = new Sim(Seq(contest()), t(14, 30)).run(t(15))
    assert(ahead(es) == Seq(20, 10, 5, 1).map(("contest_ending", _)))
    assert(es.last.asInstanceOf[ContestEnding].inMinutes == 0)
  }

  // Restored (deleted in Task 10).
  test("contests above the bound get phase events but no countdown") {
    val above = new Sim(Seq(contest(11)), t(8, 50)).run(t(10))
    assert(types(above) == Seq("contest_starting") && ahead(above).isEmpty)
    val at = new Sim(Seq(contest(10)), t(8, 50)).run(t(10))
    assert(ahead(at).nonEmpty)
    assert(ContestTimeTracker.MaxAnnouncedContest == 10)
    // and the mapping itself refuses a countdown of a test copy
    assert(ContestTimeTracker.onEdge(CountdownToStart(11, 5), contest(11), t(9, 55)).isEmpty)
  }

  test("a restart mid-countdown is silent and only the later thresholds fire") {
    val sim = new Sim(Seq(contest()), t(9, 53))
    assert(sim.wake(t(9, 53)).isEmpty)
    assert(ahead(sim.run(t(9, 54))).isEmpty)
    assert(ahead(sim.run(t(9, 55))) == Seq(("contest_starting", 5)))
    assert(ahead(sim.run(t(9, 59))) == Seq(("contest_starting", 2), ("contest_starting", 1)))
  }

  test("a late wake that crosses several thresholds announces only the latest") {
    val sim = new Sim(Seq(contest()), t(9))
    assert(ahead(sim.wake(t(9, 56))) == Seq(("contest_starting", 5)))
    assert(ahead(sim.wake(t(9, 59))) == Seq(("contest_starting", 1)))
  }

  test("a contest whose end is not after its start gets no end countdown and no exception") {
    val c = contest(end = t(10))
    val es = new Sim(Seq(c), t(9)).run(t(10, 30))
    assert(ahead(es).forall(_._1 == "contest_starting"))
    assert(types(es).count(_ == "contest_ending") == 1)
    val inverted = contest(start = t(10), end = t(9, 30))
    assert(new Sim(Seq(inverted), t(8)).run(t(11)).nonEmpty)
  }

  test("thresholds come from the configuration; empty lists disable the countdown") {
    assert(ahead(new Sim(Seq(contest()), t(9, 50), CountdownConfig(Seq(3), Nil)).run(t(10))) == Seq(("contest_starting", 3)))
    val off = CountdownConfig(Nil, Nil)
    assert(ahead(new Sim(Seq(contest()), t(8, 50), off).run(t(10))).isEmpty)
    assert(ahead(new Sim(Seq(contest(2)), t(14, 30), off).run(t(15))).isEmpty)
  }

  // Restored (deleted in Task 10).
  test("a moved start is announced once, before anything else of the same change") {
    val sim = new Sim(Seq(contest()), t(8, 30))
    val es = sim.change(contest(start = t(11)), t(8, 40))
    assert(es.size == 1)
    val e = es.head.asInstanceOf[ContestRescheduled]
    assert(e.changes == Seq(TimeChange("start", t(10), t(11))) && e.ts == t(8, 40))
    assert(e.id == s"qf-contest_rescheduled-1-${t(8, 40).getMillis}")
    assert(sim.change(contest(start = t(11)), t(8, 41)).isEmpty)
  }

  test("an extended end of a running contest is announced with only the end") {
    val sim = new Sim(Seq(contest()), t(12))
    val es = sim.change(contest(end = t(16)), t(12, 5))
    assert(es.collect { case e: ContestRescheduled => e.changes } == Seq(Seq(TimeChange("end", t(15), t(16)))) && es.size == 1)
  }

  test("after a reschedule the countdown runs again against the new start, with new ids") {
    val sim = new Sim(Seq(contest()), t(8, 30))
    sim.run(t(9, 30)) // 60 and 30 fired against 10:00
    val first = sim.change(contest(start = t(11)), t(9, 40))
    assert(ahead(first).isEmpty)
    val es = sim.run(t(11))
    assert(ahead(es) == Seq(60, 30, 20, 10, 5, 2, 1).map(("contest_starting", _)))
    assert(es.collect { case e: ContestStarting if e.inMinutes > 0 => e.at.get }.forall(_ == t(11)))
  }

  test("a start moved earlier past a threshold marks it silently; later ones still fire") {
    val sim = new Sim(Seq(contest()), t(9, 30))
    val es = sim.change(contest(start = t(9, 56)), t(9, 50)) // 6 minutes left: 10 and larger are marked
    assert(es.size == 1 && es.head.isInstanceOf[ContestRescheduled])
    assert(ahead(sim.run(t(9, 50).plusSeconds(30))).isEmpty)
    assert(ahead(sim.run(t(9, 51))) == Seq(("contest_starting", 5)))
    assert(ahead(sim.run(t(9, 54))) == Seq(("contest_starting", 2)))
  }

  test("contests above the bound are not announced when rescheduled; the others are") {
    val sim = new Sim(Seq(contest(11), contest(1)), t(8))
    assert(sim.change(contest(11, start = t(11)), t(8, 1)).isEmpty)
    assert(sim.change(contest(1, start = t(11)), t(8, 1)).collect { case e: ContestRescheduled => e }.size == 1)
    assert(sim.change(contest(2), t(8, 2)).isEmpty) // first sight
  }
}
