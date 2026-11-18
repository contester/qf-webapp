package actors

import actors.RoundClock._
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Contest

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
}
