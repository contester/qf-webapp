package actors

import actors.RoundClock.{CountdownToEnd, CountdownToStart, Ended, Started}
import actors.RoundClockActor.{Fired, Wake}
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import org.joda.time.DateTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.Contest
import palevo.CountdownConfig

import scala.concurrent.duration._

/**
  * Most tests use an injected clock with deadlines hours away, so the real timer never fires during the test; the
  * test moves the clock and delivers the timer's Wake itself. One real-time smoke test checks the actual timer.
  */
class RoundClockActorSpec extends TestKit(ActorSystem("RoundClockActorSpec")) with AnyFunSuiteLike with BeforeAndAfterAll {
  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private def round(id: Int, start: DateTime, end: DateTime, finished: Boolean = false): Contest =
    Contest(id, s"R$id", start, end, None, 1, isFinished = finished)

  private val t0 = new DateTime(2026, 11, 18, 10, 0)

  class Rig(rounds: Seq[Contest], countdown: RoundClock.Countdown = RoundClock.NoCountdown) {
    @volatile var now: DateTime = t0
    val probe = TestProbe()
    val clock: ActorRef = system.actorOf(RoundClockActor.props(rounds, Seq(probe.ref), () => now, countdown))
    // The actor reads the clock when it is constructed: wait until it runs before the test moves the clock.
    locally {
      val p = TestProbe()
      p.send(clock, akka.actor.Identify(0))
      p.expectMsgType[akka.actor.ActorIdentity](5.seconds)
    }
    def at(t: DateTime): Unit = { now = t; clock ! Wake }
    def edge(): Fired = probe.expectMsgType[Fired](3.seconds)
    def none(): Unit = probe.expectNoMessage(200.millis)
    def stop(): Unit = system.stop(clock)
  }

  test("real timer smoke test: a round starting in 300 ms and ending 600 ms later fires Started then Ended, once each") {
    val p = TestProbe()
    val start = DateTime.now.plusMillis(300)
    val r = round(1, start, start.plusMillis(600))
    val clock = system.actorOf(RoundClockActor.props(Seq(r), Seq(p.ref)))
    val s = p.expectMsgType[Fired](10.seconds)
    assert(s.edge == Started(1) && s.round == r)
    assert(!DateTime.now.isBefore(start))
    assert(p.expectMsgType[Fired](10.seconds).edge == Ended(1))
    assert(!DateTime.now.isBefore(r.endTime))
    p.expectNoMessage(500.millis)
    system.stop(clock)
  }

  test("fake clock: Started at the start, Ended at the end, each exactly once; an early wake fires nothing") {
    val r = round(1, t0.plusHours(1), t0.plusHours(3))
    val g = new Rig(Seq(r))
    g.at(t0.plusMinutes(59))
    g.none()
    g.at(t0.plusHours(1))
    assert(g.edge().edge == Started(1))
    g.at(t0.plusHours(1).plusSeconds(1))
    g.none()
    g.at(t0.plusHours(4)) // a late timer still fires the edge
    val e = g.edge()
    assert(e.edge == Ended(1) && e.round == r)
    g.at(t0.plusHours(5))
    g.none()
    g.stop()
  }

  test("edges are also published on the event stream") {
    val p = TestProbe()
    system.eventStream.subscribe(p.ref, classOf[Fired])
    try {
      val g = new Rig(Seq(round(2, t0.plusHours(1), t0.plusHours(2))))
      g.at(t0.plusHours(1))
      assert(g.edge().edge == Started(2))
      assert(p.expectMsgType[Fired](3.seconds).edge == Started(2))
      g.stop()
    } finally system.eventStream.unsubscribe(p.ref)
  }

  test("a ContestChanged that moves the end earlier re-arms for the new end; repeating it adds nothing") {
    val r = round(3, t0.minusHours(1), t0.plusHours(5))
    val g = new Rig(Seq(r))
    g.at(t0.plusMinutes(1))
    g.none() // running at start-up: no Started
    val moved = r.copy(endTime = t0.plusMinutes(10))
    g.clock ! StatusActor.ContestChanged(moved)
    g.none()
    g.at(t0.plusMinutes(10))
    val e = g.edge()
    assert(e.edge == Ended(3) && e.round == moved)
    g.clock ! StatusActor.ContestChanged(moved)
    g.at(t0.plusMinutes(11))
    g.none()
    g.stop()
  }

  test("is_finished flipping on emits Ended at once, exactly once") {
    val r = round(4, t0.minusHours(1), t0.plusHours(1))
    val g = new Rig(Seq(r))
    g.clock ! StatusActor.ContestChanged(r.copy(isFinished = true))
    assert(g.edge().edge == Ended(4))
    g.clock ! StatusActor.ContestChanged(r.copy(isFinished = true))
    g.clock ! StatusActor.ContestChanged(r.copy(isFinished = true, name = "renamed"))
    g.at(t0.plusHours(2))
    g.none()
    g.stop()
  }

  test("ContestRemoved cancels the round's pending edges") {
    val g = new Rig(Seq(round(5, t0.plusHours(1), t0.plusHours(2))))
    g.clock ! StatusActor.ContestRemoved(5)
    g.at(t0.plusHours(3))
    g.none()
    g.stop()
  }

  test("a round added by a ContestChanged is watched too") {
    val g = new Rig(Nil)
    g.clock ! StatusActor.ContestChanged(round(6, t0.plusHours(1), t0.plusHours(2)))
    g.none()
    g.at(t0.plusHours(1))
    assert(g.edge().edge == Started(6))
    g.none()
    g.stop()
  }

  test("the timer delay is the exact time left, never negative, capped at 7 days for the Akka scheduler") {
    val now = new DateTime(2026, 11, 18, 10, 0)
    assert(RoundClockActor.delay(now.plusMillis(1234), now) == 1234.millis)
    assert(RoundClockActor.delay(now.minusMinutes(5), now) == Duration.Zero)
    assert(RoundClockActor.delay(now.plusYears(70), now) == 7.days)
  }

  private val cd: RoundClock.Countdown = _ => CountdownConfig(Seq(30, 10, 1), Seq(5))

  test("countdown edges: each threshold once, a late wake gives only the latest, then the start") {
    val r = round(7, t0.plusHours(1), t0.plusHours(3))
    val g = new Rig(Seq(r), cd)
    g.at(t0.plusMinutes(29))
    g.none()
    g.at(t0.plusMinutes(30))
    val e = g.edge()
    assert(e.edge == CountdownToStart(7, 30) && e.round == r)
    g.at(t0.plusMinutes(30).plusSeconds(1))
    g.none()
    g.at(t0.plusMinutes(59).plusSeconds(30)) // a late wake: 10 and 1 crossed, only 1 is announced
    assert(g.edge().edge == CountdownToStart(7, 1))
    g.none()
    g.at(t0.plusHours(1))
    assert(g.edge().edge == Started(7))
    g.at(t0.plusHours(3).minusMinutes(5))
    assert(g.edge().edge == CountdownToEnd(7, 5))
    g.clock ! StatusActor.ContestChanged(r) // unchanged row: nothing again
    g.none()
    g.at(t0.plusHours(3))
    assert(g.edge().edge == Ended(7))
    g.none()
    g.stop()
  }

  test("countdown edges: a clock started mid-countdown is silent about passed thresholds; a reschedule re-arms them") {
    val r = round(8, t0.plusMinutes(20), t0.plusHours(3)) // 30 has passed at start-up
    val g = new Rig(Seq(r), cd)
    g.none()
    g.at(t0.plusMinutes(10))
    assert(g.edge().edge == CountdownToStart(8, 10))
    // moved an hour later: 30 and 10 are ahead again
    val moved = r.copy(startTime = t0.plusMinutes(80))
    g.clock ! StatusActor.ContestChanged(moved)
    g.none()
    g.at(t0.plusMinutes(50))
    val e = g.edge()
    assert(e.edge == CountdownToStart(8, 30) && e.round == moved)
    g.stop()
  }
}
