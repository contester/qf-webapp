package actors

import actors.RoundClock._
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Contest
import palevo.CountdownConfig

class RoundClockSpec extends AnyFunSuite {
  private val now = new DateTime(2026, 11, 18, 10, 0)

  private def round(id: Int, start: DateTime, end: DateTime, finished: Boolean = false): Contest =
    Contest(id, s"R$id", start, end, None, 1, isFinished = finished)

  private def finishedOf(rs: Contest*): Int => Boolean = id => rs.find(_.id == id).exists(_.isFinished)

  test("no rounds: no deadline") {
    assert(nextDeadline(Nil, _ => false, now, Set.empty).isEmpty)
  }

  test("the earliest edge across several rounds comes first") {
    val a = round(1, now.plusHours(2), now.plusHours(4))
    val b = round(2, now.plusHours(1), now.plusHours(5))
    val c = round(3, now.minusHours(1), now.plusMinutes(30)) // running: its start already fired
    val fired: Set[RoundEdge] = Set(Started(3))
    assert(nextDeadline(Seq(a, b, c), finishedOf(a, b, c), now, fired) == Some(now.plusMinutes(30) -> Ended(3)))
    assert(nextDeadline(Seq(a, b), finishedOf(a, b), now, fired) == Some(now.plusHours(1) -> Started(2)))
  }

  test("ties are broken by round id, then Started before Ended") {
    val t = now.plusHours(1)
    val a = round(7, t, t.plusHours(1))
    val b = round(3, t, t.plusHours(1))
    val c = round(5, t.minusHours(1), t) // ends exactly when the others start
    val fired: Set[RoundEdge] = Set(Started(5))
    assert(nextDeadline(Seq(a, b, c), _ => false, now, fired) == Some(t -> Started(3)))
    assert(nextDeadline(Seq(a, c), _ => false, now, fired) == Some(t -> Ended(5)))
    assert(nextDeadline(Seq(c, a), _ => false, now, fired) == Some(t -> Ended(5)))
  }

  test("already fired edges are skipped; a round whose end fired yields nothing") {
    val a = round(1, now.minusHours(1), now.plusHours(1))
    assert(nextDeadline(Seq(a), _ => false, now, Set(Started(1))) == Some(now.plusHours(1) -> Ended(1)))
    assert(nextDeadline(Seq(a), _ => false, now, Set(Started(1), Ended(1))).isEmpty)
  }

  test("a past edge that never fired is still due (deadline in the past)") {
    val a = round(1, now.minusMinutes(1), now.plusHours(1))
    assert(nextDeadline(Seq(a), _ => false, now, Set.empty) == Some(now.minusMinutes(1) -> Started(1)))
  }

  test("is_finished before the end yields Ended immediately, and only once") {
    val a = round(1, now.minusHours(1), now.plusHours(1), finished = true)
    assert(nextDeadline(Seq(a), finishedOf(a), now, Set(Started(1))) == Some(now -> Ended(1)))
    // finished before it even started: Ended now, and Started never follows
    val b = round(2, now.plusHours(1), now.plusHours(2), finished = true)
    assert(nextDeadline(Seq(b), finishedOf(b), now, Set.empty) == Some(now -> Ended(2)))
    val s = State.load(Seq(a.copy(isFinished = false)), now.minusMinutes(1))
    assert(s.fired == Set(Started(1)))
    val (edges, after) = s.changed(a, now).due(now)
    assert(edges == Seq(Ended(1)))
    assert(after.due(now.plusHours(3))._1.isEmpty)
    assert(after.changed(a, now.plusMinutes(1)).due(now.plusHours(3))._1.isEmpty, "an unchanged row must not re-arm it")
  }

  test("restart rule: a running round does not re-announce Started; an over round announces nothing") {
    val running = round(1, now.minusHours(1), now.plusHours(1))
    val over = round(2, now.minusHours(3), now.minusHours(1))
    val finished = round(3, now.minusHours(1), now.plusHours(1), finished = true)
    val future = round(4, now.plusHours(1), now.plusHours(2))
    val s = State.load(Seq(running, over, finished, future), now)
    assert(s.due(now)._1.isEmpty)
    assert(s.next(now) == Some(now.plusHours(1) -> Ended(1)))
    val (edges, _) = s.due(now.plusHours(3))
    assert(edges == Seq(Ended(1), Started(4), Ended(4)))
  }

  test("due edges fire in order and exactly once; a ContestChanged with the same times does not repeat them") {
    val a = round(1, now.plusMinutes(1), now.plusMinutes(2))
    val s0 = State.load(Seq(a), now)
    val (e1, s1) = s0.due(now.plusMinutes(1))
    assert(e1 == Seq(Started(1)))
    val s2 = s1.changed(a, now.plusMinutes(1).plusSeconds(1))
    assert(s2.due(now.plusMinutes(1).plusSeconds(1))._1.isEmpty)
    val (e3, s3) = s2.due(now.plusMinutes(3))
    assert(e3 == Seq(Ended(1)))
    assert(s3.changed(a, now.plusMinutes(4)).due(now.plusMinutes(5))._1.isEmpty)
  }

  test("a rescheduled round re-derives its edges: unfired ones move, a fired one in the new future fires again") {
    val a = round(1, now.minusHours(1), now.plusHours(1))
    val s = State.load(Seq(a, round(2, now.plusHours(5), now.plusHours(6))), now)
    // the end moves earlier, into the past: Ended is due at once
    val earlier = s.changed(a.copy(endTime = now.minusMinutes(1)), now)
    assert(earlier.due(now)._1 == Seq(Ended(1)))
    // the end moves later: Ended waits for the new end
    val later = s.changed(a.copy(endTime = now.plusHours(3)), now)
    assert(later.next(now) == Some(now.plusHours(3) -> Ended(1)))
    // an ended round extended into the future: Ended fires again at the new end; Started is not repeated
    val (_, ended) = s.due(now.plusHours(2))
    val extended = ended.changed(a.copy(endTime = now.plusHours(4)), now.plusHours(2))
    assert(extended.due(now.plusHours(2))._1.isEmpty)
    assert(extended.next(now.plusHours(2)) == Some(now.plusHours(4) -> Ended(1)))
    // the other round is untouched by all of this
    assert(extended.fired.filter(_.roundId == 2).isEmpty)
    assert(earlier.rounds(2) == s.rounds(2))
  }

  test("a round first seen through a change starts from its current state too") {
    val s = State.load(Nil, now)
    val running = round(9, now.minusHours(1), now.plusHours(1))
    val s1 = s.changed(running, now)
    assert(s1.due(now)._1.isEmpty)
    assert(s1.next(now) == Some(now.plusHours(1) -> Ended(9)))
  }

  test("ContestRemoved drops the round and its edges") {
    val a = round(1, now.plusHours(1), now.plusHours(2))
    val b = round(2, now.plusHours(3), now.plusHours(4))
    val s = State.load(Seq(a, b), now).removed(1)
    assert(s.rounds.keySet == Set(2))
    assert(s.next(now) == Some(now.plusHours(3) -> Started(2)))
    assert(s.removed(2).next(now).isEmpty)
    assert(s.removed(2).fired.isEmpty)
  }

  // ---- countdown edges (Task 18) ----

  private val cd = CountdownConfig(Seq(30, 10, 1), Seq(10, 1))
  private val withCd: Contest => CountdownConfig = _ => cd

  /** Every edge the state emits from `from` up to `to`, waking exactly at each deadline (as the actor does). */
  private def walk(s0: State, from: DateTime, to: DateTime): (Seq[(DateTime, RoundEdge)], State) = {
    var s = s0
    val out = Vector.newBuilder[(DateTime, RoundEdge)]
    var t = from
    var more = true
    while (more) {
      val (es, after) = s.due(t)
      out ++= es.map(t -> _)
      s = after
      s.next(t) match {
        case Some((d, _)) if !d.isAfter(to) => t = if (d.isAfter(t)) d else t
        case _ => more = false
      }
    }
    (out.result(), s)
  }

  private val start = now.plusHours(1)
  private val end = now.plusHours(3)
  private val r1 = round(1, start, end)

  test("countdown edges come at their thresholds, before their target, each exactly once") {
    val (es, s) = walk(State.load(Seq(r1), now, withCd), now, end.plusHours(1))
    assert(es == Seq(
      start.minusMinutes(30) -> CountdownToStart(1, 30), start.minusMinutes(10) -> CountdownToStart(1, 10),
      start.minusMinutes(1) -> CountdownToStart(1, 1), start -> Started(1),
      end.minusMinutes(10) -> CountdownToEnd(1, 10), end.minusMinutes(1) -> CountdownToEnd(1, 1), end -> Ended(1)))
    assert(s.next(end.plusHours(1)).isEmpty)
    assert(walk(s.changed(r1, end.plusHours(2)), end.plusHours(2), end.plusHours(9))._1.isEmpty)
  }

  test("nextDeadline sees the next unfired threshold edge") {
    assert(nextDeadline(Seq(r1), _ => false, now, Set.empty, withCd) == Some(start.minusMinutes(30) -> CountdownToStart(1, 30)))
    assert(nextDeadline(Seq(r1), _ => false, now, Set(CountdownToStart(1, 30)), withCd) ==
      Some(start.minusMinutes(10) -> CountdownToStart(1, 10)))
    // without a countdown only the round's own edges exist
    assert(nextDeadline(Seq(r1), _ => false, now, Set.empty) == Some(start -> Started(1)))
    val running: Set[RoundEdge] = Set(CountdownToStart(1, 30), CountdownToStart(1, 10), CountdownToStart(1, 1), Started(1))
    assert(nextDeadline(Seq(r1), _ => false, start, running, withCd) == Some(end.minusMinutes(10) -> CountdownToEnd(1, 10)))
  }

  test("restart rule: thresholds already passed at load are fired and never announced") {
    val s = State.load(Seq(r1), start.minusMinutes(15), withCd)
    assert(s.fired == Set(CountdownToStart(1, 30)))
    assert(s.due(start.minusMinutes(15))._1.isEmpty)
    assert(s.next(start.minusMinutes(15)) == Some(start.minusMinutes(10) -> CountdownToStart(1, 10)))
    val mid = State.load(Seq(r1), end.minusMinutes(5), withCd)
    assert(mid.due(end.minusMinutes(5))._1.isEmpty)
    assert(mid.next(end.minusMinutes(5)) == Some(end.minusMinutes(1) -> CountdownToEnd(1, 1)))
    val over = State.load(Seq(r1), end.plusMinutes(1), withCd)
    assert(over.next(end.plusMinutes(1)).isEmpty)
  }

  test("a late wake that crossed several thresholds announces only the latest; past the target, only the target") {
    val s = State.load(Seq(r1), now, withCd)
    val (late, s1) = s.due(start.minusMinutes(5))
    assert(late == Seq(CountdownToStart(1, 10)))
    assert(s1.fired.contains(CountdownToStart(1, 30)))
    assert(s1.due(start.minusMinutes(1))._1 == Seq(CountdownToStart(1, 1)))
    // a wake after the start: the start itself, no stale "starts in N minutes"
    val (pastStart, s2) = s.due(start.plusMinutes(1))
    assert(pastStart == Seq(Started(1)))
    // after the start, a wake 30 s before the end crossed both end thresholds: only the 1-minute one
    assert(s2.due(end.minusSeconds(30))._1 == Seq(CountdownToEnd(1, 1)))
    // a wake after the end: only Ended
    assert(s2.due(end.plusMinutes(1))._1 == Seq(Ended(1)))
  }

  test("an end threshold at or before the start is never an edge") {
    val short = round(2, start, start.plusMinutes(5))
    val (es, _) = walk(State.load(Seq(short), now, withCd), now, start.plusHours(1))
    assert(es.map(_._2) == Seq(CountdownToStart(2, 30), CountdownToStart(2, 10), CountdownToStart(2, 1), Started(2),
      CountdownToEnd(2, 1), Ended(2)))
  }

  test("the countdown is per round: a round given none has only its start and end") {
    val r2 = round(2, start, end)
    val only1: Contest => CountdownConfig = c => if (c.id == 1) cd else CountdownConfig(Nil, Nil)
    val (es, _) = walk(State.load(Seq(r1, r2), now, only1), now, end.plusHours(1))
    assert(es.map(_._2).filter(_.roundId == 2) == Seq(Started(2), Ended(2)))
    assert(es.map(_._2).count(_.roundId == 1) == 7)
  }

  test("is_finished: no more countdown, Ended at once") {
    val s = State.load(Seq(r1), start.minusMinutes(20), withCd)
    val (es, s1) = s.changed(r1.copy(isFinished = true), start.minusMinutes(20)).due(start.minusMinutes(20))
    assert(es == Seq(Ended(1)))
    assert(s1.next(start.minusMinutes(20)).isEmpty)
  }

  test("an unchanged ContestChanged does not re-announce a fired threshold") {
    val (es, s) = State.load(Seq(r1), now, withCd).due(start.minusMinutes(30))
    assert(es == Seq(CountdownToStart(1, 30)))
    val again = s.changed(r1, start.minusMinutes(29))
    assert(again.due(start.minusMinutes(29))._1.isEmpty)
    assert(again.next(start.minusMinutes(29)) == Some(start.minusMinutes(10) -> CountdownToStart(1, 10)))
  }

  test("a rescheduled round resets its own threshold edges only") {
    val r2 = round(2, start, end)
    val (_, s) = State.load(Seq(r1, r2), now, withCd).due(start.minusMinutes(30))
    assert(s.fired == Set(CountdownToStart(1, 30), CountdownToStart(2, 30)))
    // round 1 moves an hour later: its 30-minute threshold is in the future again and fires against the new start
    val later = s.changed(r1.copy(startTime = start.plusHours(1)), start.minusMinutes(29))
    assert(later.fired == Set(CountdownToStart(2, 30)))
    val (es, _) = walk(later, start.minusMinutes(29), start.plusHours(1))
    assert(es.filter(_._2.roundId == 1).take(2) ==
      Seq(start.plusMinutes(30) -> CountdownToStart(1, 30), start.plusMinutes(50) -> CountdownToStart(1, 10)))
    assert(!es.exists(_._2 == CountdownToStart(2, 30)))
  }

  test("a start moved earlier past thresholds marks them silently; the later ones still fire") {
    val s = State.load(Seq(r1), start.minusMinutes(40), withCd)
    val moved = s.changed(r1.copy(startTime = start.minusMinutes(34)), start.minusMinutes(40)) // 6 minutes left
    assert(moved.due(start.minusMinutes(40))._1.isEmpty)
    assert(moved.fired == Set(CountdownToStart(1, 30), CountdownToStart(1, 10)))
    assert(moved.next(start.minusMinutes(40)) == Some(start.minusMinutes(35) -> CountdownToStart(1, 1)))
  }

  test("an end moved: the end thresholds follow it; the start's are untouched") {
    val (_, running) = walk(State.load(Seq(r1), now, withCd), now, start)
    val (es1, s1) = running.due(end.minusMinutes(10))
    assert(es1 == Seq(CountdownToEnd(1, 10)))
    val extended = s1.changed(r1.copy(endTime = end.plusHours(1)), end.minusMinutes(9))
    assert(!extended.fired.contains(CountdownToEnd(1, 10)) && extended.fired.contains(CountdownToStart(1, 30)))
    assert(extended.next(end.minusMinutes(9)) == Some(end.plusMinutes(50) -> CountdownToEnd(1, 10)))
  }
}
