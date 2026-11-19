package actors

import actors.RoundClock.{CountdownToEnd, CountdownToStart, Ended, Started}
import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.testkit.{TestKit, TestProbe}
import models.ContestFixture
import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.Contest
import play.api.libs.EventSource.Event
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Try

/**
  * The round events StatusActor pushes on its SSE streams. The event itself is checked without a database; the push to
  * a joined participant and an admin runs a StatusActor on the test database (skipped without QF_TEST_URL).
  */
class StatusActorRoundEventsSpec extends TestKit(ActorSystem("StatusActorRoundEventsSpec")) with AnyFunSuiteLike with BeforeAndAfterAll {
  private implicit val ec: ExecutionContext = system.dispatcher
  private implicit val mat: Materializer = Materializer(system)
  private val fx = new ContestFixture("Round events spec")

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private val start = new DateTime(2030, 1, 1, 12, 0, DateTimeZone.UTC)
  private val c = Contest(42, "R", start, start.plusHours(2), None, 1)

  test("round events: name and data carry the round id and its times") {
    for ((edge, name) <- Seq(Started(42) -> "round_started", Ended(42) -> "round_ended")) {
      val e = StatusActor.roundEvent(edge, c).get
      assert(e.name.contains(name))
      val js = Json.parse(e.data)
      assert((js \ "round").as[Int] == 42)
      assert((js \ "startTime").as[Long] == start.getMillis)
      assert((js \ "endTime").as[Long] == start.plusHours(2).getMillis)
      assert(!(js \ "isFinished").as[Boolean])
    }
    // countdown edges are for palevo only: no browser event
    assert(StatusActor.roundEvent(CountdownToStart(42, 5), c).isEmpty)
    assert(StatusActor.roundEvent(CountdownToEnd(42, 1), c).isEmpty)
    val r = StatusActor.rescheduledEvent(c)
    assert(r.name.contains("round_rescheduled") && (Json.parse(r.data) \ "round").as[Int] == 42)
  }

  test("a rescheduled round is one whose start or end changed") {
    assert(StatusActor.rescheduled(c, c.copy(endTime = c.endTime.plusMinutes(1))))
    assert(StatusActor.rescheduled(c, c.copy(startTime = c.startTime.minusMinutes(1))))
    assert(!StatusActor.rescheduled(c, c.copy(isFinished = true, name = "x")))
  }

  /** Round events go to the probe; the first event of any kind shows the stream is running. */
  test("a failed start-up load is retried (retry-on-failure, no polling) and the actor then serves its contests") {
    val calls = new java.util.concurrent.atomic.AtomicInteger
    val status = system.actorOf(StatusActor.propsWith(null, () =>
      if (calls.incrementAndGet() == 1) scala.concurrent.Future.failed(new RuntimeException("db down"))
      else scala.concurrent.Future.successful(StatusActor.StatusActorInitialState(Nil, Map.empty,
        StatusActor.ClarificationsInitialState(Nil, Nil), Seq(c)))))
    try {
      val probe = TestProbe()
      probe.send(status, StatusActor.GetAllContests)
      val all = probe.expectMsgType[scala.util.Success[StatusActor.AllContests]](10.seconds).value
      assert(all.contests == Seq(c))
      assert(calls.get == 2)
    } finally system.stop(status)
  }

  private def roundEvents(src: Source[Event, NotUsed]): TestProbe = {
    val p = TestProbe()
    val running = TestProbe()
    src.runForeach { e =>
      running.ref ! e
      if (e.name.exists(_.startsWith("round_"))) p.ref ! e
    }
    running.expectMsgType[Event](10.seconds)
    // The broadcast hubs attach their consumers asynchronously after the first element; give them a moment.
    Thread.sleep(300)
    p
  }

  private def data(p: TestProbe, name: String): JsValue = {
    val e = p.expectMsgType[Event](5.seconds)
    assert(e.name.contains(name), e)
    Json.parse(e.data)
  }

  test("a Started edge reaches the round's joined participants and admins; a reschedule pushes the new times") {
    assume(fx.url.isDefined)
    fx.withFixture {
      val status = system.actorOf(StatusActor.props(fx.db, palevo.NoopPalevoSink, (_, _) => scala.concurrent.Future.successful(None),
        live.NoopLiveHooks, palevo.CountdownConfig.Default))
      try {
        val probe = TestProbe()
        probe.send(status, StatusActor.JoinUser(fx.round, fx.kid))
        val user = roundEvents(probe.expectMsgType[StatusActor.UserJoined](10.seconds).enumerator)
        probe.send(status, StatusActor.JoinAdmin(fx.round))
        val admin = roundEvents(probe.expectMsgType[Try[Source[Event, NotUsed]]](10.seconds).get)
        probe.send(status, StatusActor.JoinUser(fx.round + 100000, fx.kid))
        val other = roundEvents(probe.expectMsgType[StatusActor.UserJoined](10.seconds).enumerator)
        probe.send(status, StatusActor.GetSingleContest(fx.round))
        val round = probe.expectMsgType[scala.util.Success[StatusActor.GetSingleContestResult]](5.seconds).value.c.get

        status ! RoundClockActor.Fired(Started(fx.round), round)
        for (p <- Seq(user, admin)) assert((data(p, "round_started") \ "round").as[Int] == fx.round)

        val moved = round.copy(endTime = round.endTime.plusHours(1))
        status ! StatusActor.ContestChanged(moved)
        for (p <- Seq(user, admin)) {
          val js = data(p, "round_rescheduled")
          assert((js \ "round").as[Int] == fx.round && (js \ "endTime").as[Long] == moved.endTime.getMillis)
        }
        status ! StatusActor.ContestChanged(moved.copy(name = "renamed")) // not a reschedule
        // a countdown edge reaches no browser: the next round event the streams see is round_ended
        status ! RoundClockActor.Fired(CountdownToEnd(fx.round, 5), moved)
        status ! RoundClockActor.Fired(Ended(fx.round), moved)
        for (p <- Seq(user, admin)) data(p, "round_ended")
        other.expectNoMessage(300.millis)
        user.expectNoMessage(100.millis)

        status ! StatusActor.ContestRemoved(fx.round)
        probe.send(status, StatusActor.GetSingleContest(fx.round))
        assert(probe.expectMsgType[scala.util.Success[StatusActor.GetSingleContestResult]](5.seconds).value.c.isEmpty)
      } finally system.stop(status)
    }
  }
}
