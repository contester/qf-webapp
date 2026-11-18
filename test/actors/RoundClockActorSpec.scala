package actors

import actors.RoundClock.{Ended, Started}
import actors.RoundClockActor.Fired
import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import org.joda.time.DateTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.Contest

import scala.concurrent.duration._

class RoundClockActorSpec extends TestKit(ActorSystem("RoundClockActorSpec")) with AnyFunSuiteLike with BeforeAndAfterAll {
  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private def round(id: Int, start: DateTime, end: DateTime, finished: Boolean = false): Contest =
    Contest(id, s"R$id", start, end, None, 1, isFinished = finished)

  private def edge(p: TestProbe, max: FiniteDuration = 3.seconds) = p.expectMsgType[Fired](max)

  test("a round starting in 300 ms and ending 600 ms later: Started, then Ended, each exactly once") {
    val p = TestProbe()
    val start = DateTime.now.plusMillis(300)
    val r = round(1, start, start.plusMillis(600))
    val clock = system.actorOf(RoundClockActor.props(Seq(r), Seq(p.ref)))
    p.expectNoMessage(150.millis)
    val s = edge(p)
    assert(s.edge == Started(1) && s.round == r)
    assert(!DateTime.now.isBefore(start))
    val e = edge(p)
    assert(e.edge == Ended(1))
    assert(!DateTime.now.isBefore(r.endTime))
    p.expectNoMessage(700.millis)
    system.stop(clock)
  }

  test("edges are also published on the event stream") {
    val p = TestProbe()
    system.eventStream.subscribe(p.ref, classOf[Fired])
    try {
      val start = DateTime.now.plusMillis(200)
      val clock = system.actorOf(RoundClockActor.props(Seq(round(2, start, start.plusHours(1))), Nil))
      assert(edge(p).edge == Started(2))
      system.stop(clock)
    } finally system.eventStream.unsubscribe(p.ref)
  }

  test("a ContestChanged that moves the end earlier re-arms the timer for the new end") {
    val p = TestProbe()
    val r = round(3, DateTime.now.minusHours(1), DateTime.now.plusHours(1))
    val clock = system.actorOf(RoundClockActor.props(Seq(r), Seq(p.ref)))
    p.expectNoMessage(300.millis) // running at start-up: no Started
    val moved = r.copy(endTime = DateTime.now.plusMillis(300))
    clock ! StatusActor.ContestChanged(moved)
    val e = edge(p)
    assert(e.edge == Ended(3) && e.round == moved)
    clock ! StatusActor.ContestChanged(moved) // same times again: nothing new
    p.expectNoMessage(600.millis)
    system.stop(clock)
  }

  test("is_finished flipping on emits Ended at once, exactly once") {
    val p = TestProbe()
    val r = round(4, DateTime.now.minusHours(1), DateTime.now.plusHours(1))
    val clock = system.actorOf(RoundClockActor.props(Seq(r), Seq(p.ref)))
    p.expectNoMessage(200.millis)
    clock ! StatusActor.ContestChanged(r.copy(isFinished = true))
    assert(edge(p, 1.second).edge == Ended(4))
    clock ! StatusActor.ContestChanged(r.copy(isFinished = true))
    clock ! StatusActor.ContestChanged(r.copy(isFinished = true, name = "renamed"))
    p.expectNoMessage(500.millis)
    system.stop(clock)
  }

  test("ContestRemoved cancels the round's pending edges") {
    val p = TestProbe()
    val start = DateTime.now.plusMillis(400)
    val clock = system.actorOf(RoundClockActor.props(Seq(round(5, start, start.plusMillis(200))), Seq(p.ref)))
    clock ! StatusActor.ContestRemoved(5)
    p.expectNoMessage(1200.millis)
    system.stop(clock)
  }

  test("a round added by a ContestChanged is watched too") {
    val p = TestProbe()
    val clock = system.actorOf(RoundClockActor.props(Nil, Seq(p.ref)))
    val start = DateTime.now.plusMillis(300)
    clock ! StatusActor.ContestChanged(round(6, start, start.plusHours(1)))
    assert(edge(p).edge == Started(6))
    p.expectNoMessage(300.millis)
    system.stop(clock)
  }

  test("the timer delay is the exact time left, never negative, capped at 7 days for the Akka scheduler") {
    val now = new DateTime(2026, 11, 18, 10, 0)
    assert(RoundClockActor.delay(now.plusMillis(1234), now) == 1234.millis)
    assert(RoundClockActor.delay(now.minusMinutes(5), now) == Duration.Zero)
    assert(RoundClockActor.delay(now.plusYears(70), now) == 7.days)
  }
}
