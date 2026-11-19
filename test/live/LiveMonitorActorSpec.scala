package live

import java.util.concurrent.atomic.AtomicInteger

import actors.LiveMonitorActor
import actors.LiveMonitorActor._
import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem}
import akka.stream.scaladsl.Source
import akka.testkit.{EventFilter, TestEvent, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.joda.time.DateTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.Contest
import palevo.{ContestRescheduled, ContestStarting, FirstSolveContest, JuryAudience, PalevoEvent, PalevoSink}
import play.api.libs.EventSource.Event
import play.api.libs.json.JsValue

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}

class LiveMonitorActorSpec extends TestKit(ActorSystem("LiveMonitorActorSpec", ConfigFactory.parseString(
  """
    |akka.loggers = ["akka.testkit.TestEventListener"]
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |akka.use-slf4j = off
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll with LiveActorFixtures {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  // Streams of a stopped actor still report their end to it.
  system.eventStream.publish(TestEvent.Mute(EventFilter.warning(pattern = "received dead letter.*Unregister")))

  private class CollectingSink extends PalevoSink {
    val events = new java.util.concurrent.ConcurrentLinkedQueue[PalevoEvent]()
    override def send(e: PalevoEvent): Unit = events.add(e)
  }

  private class ThrowOnceSink extends PalevoSink {
    val calls = new AtomicInteger
    override def send(e: PalevoEvent): Unit =
      if (calls.incrementAndGet() == 1) throw new IllegalStateException("injected palevo failure")
  }

  test("startup loads the board before handling messages sent during the load") {
    val gate = Promise[Unit]()
    val f = new Fixture(running, gate = gate.future)
    val probe = TestProbe()
    probe.send(f.actor, JoinAdmin(1))
    val r = rec(10, 2, "A", running)
    f.actor ! Arrived(1, r)
    probe.expectNoMessage(200.millis)
    gate.success(())
    val src = probe.expectMsgType[Source[Event, NotUsed]](5.seconds)
    val p = TestProbe()
    src.runForeach(e => p.ref ! e)
    val expected = new LiveContest(contest(), problems, teams, Nil)
    expected.onArrived(r)
    assert(next(p, "board") == LiveJson.board(expected.board))
    f.stop()
  }

  test("the admin stream starts with the real points board, before the start, while running and after the end") {
    val subs = Seq(rec(1, 1, "A", t0.plusMinutes(30), ok), rec(2, 2, "B", t0.plusHours(4).plusMinutes(5), pts(4)))
    for (now <- Seq(t0.minusMinutes(5), running, late, t0.plusHours(6))) {
      val f = new Fixture(now, subs = Map(1 -> subs))
      val b = next(f.subscribe(JoinAdmin(1)), "board")
      assert(b == LiveJson.board(boardOf(contest(), subs)), s"at $now")
      assert((b \ "problems").as[Seq[JsValue]].map(p => (p \ "id").as[String]) == problemIds)
      assert(best(cellJs(b, 1, "A")).contains("10") && best(cellJs(b, 2, "B")).contains("4"))
      f.stop()
    }
  }

  test("an unknown contest is refused for the board and for the snapshot") {
    val f = new Fixture(running)
    val probe = TestProbe()
    probe.send(f.actor, JoinAdmin(42))
    probe.expectMsgType[akka.actor.Status.Failure](5.seconds)
    probe.send(f.actor, Snapshot(JoinAdmin(42)))
    probe.expectMsgType[akka.actor.Status.Failure](5.seconds)
    probe.send(f.actor, Snapshot(JoinAdmin(1)))
    assert(probe.expectMsgType[LiveBoard](5.seconds) == boardOf(contest(), Nil))
    f.stop()
  }

  // Restored (deleted in Task 10): an arrival and then its finish each give the admin stream a row event.
  test("Arrived then Finished produce row events on the admin stream: in flight, then the points and the new place") {
    val f = new Fixture(running, subs = Map(1 -> Seq(rec(1, 1, "A", t0.plusMinutes(10), pts(5)))))
    val adm = f.subscribe(JoinAdmin(1))
    next(adm, "board")

    f.actor ! Arrived(1, rec(21, 2, "B", running))
    val arrived = next(adm, "row")
    assert((arrived \ "team").as[Int] == 2)
    assert(pending(rowCellJs(arrived, "B")) == 1 && best(rowCellJs(arrived, "B")).isEmpty)
    assert((arrived \ "row" \ "total").as[String] == "0")

    f.actor ! Finished(1, 21, compiled = true, Some(7.0))
    val done = next(adm, "row")
    assert((done \ "team").as[Int] == 2 && (done \ "pos").as[Int] == 0)
    assert(best(rowCellJs(done, "B")).contains("7") && pending(rowCellJs(done, "B")) == 0)
    assert((done \ "row" \ "place").as[Int] == 1 && (done \ "row" \ "total").as[String] == "7")
    // team 2 passed team 1: places 1 and 2 from position 0
    assert((done \ "rankFrom").as[Int] == 0 && (done \ "ranks").as[Seq[Int]].take(2) == Seq(1, 2))
    adm.expectNoMessage(200.millis)
    f.stop()
  }

  test("an exception while applying an event does not kill the actor and rebuilds the contest") {
    val sink = new ThrowOnceSink
    val f = new Fixture(running, palevoSink = sink)
    f.barrier()
    assert(f.loads.get == 1)
    EventFilter[IllegalStateException](occurrences = 1).intercept {
      f.actor ! Arrived(1, rec(31, 1, "A", running, ok))
    }
    awaitCond(f.loads.get == 2, 5.seconds)
    f.barrier()
    f.stop()
  }

  test("a submit with a rejudge in flight is excluded from a reload's comparison") {
    val solved = rec(51, 1, "A", t0.plusMinutes(30), ok)
    val f = new Fixture(running, subs = Map(1 -> Seq(solved)))
    val real = f.subscribe(JoinAdmin(1))
    next(real, "board")
    f.actor ! RejudgeRequested(51)
    val rejudged = next(real, "row")
    assert(pending(rowCellJs(rejudged, "A")) == 1 && best(rowCellJs(rejudged, "A")).isEmpty)
    // A finish for an id the contest does not know makes it reload from the database.
    f.actor ! Finished(1, 999, compiled = true, Some(1.0))
    awaitCond(f.loads.get == 2, 5.seconds)
    real.expectNoMessage(500.millis)
    assert(pending(cellJs(next(f.subscribe(JoinAdmin(1)), "board"), 1, "A")) == 1)
    f.actor ! Finished(1, 51, compiled = true, Some(6.0))
    val after = next(real, "row")
    assert(best(rowCellJs(after, "A")).contains("6") && pending(rowCellJs(after, "A")) == 0)
    f.stop()
  }

  test("palevo hears the first solve") {
    val sink = new CollectingSink
    val f = new Fixture(running, palevoSink = sink)
    f.barrier()
    assert(sink.events.isEmpty)
    f.actor ! Arrived(1, rec(71, 2, "B", running))
    f.actor ! Finished(1, 71, compiled = true, Some(10.0))
    awaitCond(sink.events.asScala.exists(_.isInstanceOf[FirstSolveContest]), 5.seconds)
    // While the round runs only the jury hears standings (the public learns nothing of the board before the end).
    assert(sink.events.asScala.collect { case e: FirstSolveContest => e.audience }.forall(_ == JuryAudience))
    f.stop()
  }

  test("a partial score is not a solve for palevo") {
    val sink = new CollectingSink
    val f = new Fixture(running, palevoSink = sink)
    f.barrier()
    f.actor ! Arrived(1, rec(72, 2, "B", running))
    f.actor ! Finished(1, 72, compiled = true, Some(9.0))
    f.barrier()
    assert(!sink.events.asScala.exists(_.isInstanceOf[FirstSolveContest]))
    f.stop()
  }

  private def aheadOf(sink: CollectingSink): Seq[Int] =
    sink.events.asScala.toSeq.collect { case e: ContestStarting if e.inMinutes > 0 => e.inMinutes }

  test("palevo hears a start countdown when a tick passes a threshold") {
    val sink = new CollectingSink
    val f = new Fixture(t0.minusMinutes(61), palevoSink = sink)
    f.barrier()
    assert(sink.events.isEmpty) // the first observation only records
    f.clock.at = t0.minusMinutes(59)
    f.actor ! Tick
    f.barrier()
    assert(aheadOf(sink) == Seq(60))
    f.stop()
  }

  test("contests above 10 get no countdown and no reschedule announcement") {
    val sink = new CollectingSink
    val f = new Fixture(t0.minusMinutes(61), rows = Seq(contest(11)), palevoSink = sink)
    def barrier11(): Unit = f.join(JoinAdmin(11))
    barrier11()
    f.clock.at = t0.minusMinutes(59)
    f.actor ! Tick
    f.actor ! ContestChanged(contest(11).copy(startTime = t0.plusHours(1)))
    barrier11()
    assert(sink.events.isEmpty)
    f.stop()
  }

  test("a palevo failure while announcing time events does not stop the actor") {
    val sink = new ThrowOnceSink
    val f = new Fixture(t0.minusMinutes(61), palevoSink = sink)
    f.barrier()
    f.clock.at = t0.minusMinutes(59)
    f.actor ! Tick
    f.barrier() // the tick must be handled before the clock moves on
    f.clock.at = t0.minusMinutes(29)
    f.actor ! Tick
    f.barrier()
    assert(sink.calls.get == 2)
    f.stop()
  }

  test("a monitor started in the middle of a countdown stays silent about the thresholds already passed") {
    val sink = new CollectingSink
    val f = new Fixture(t0.minusMinutes(21), palevoSink = sink)
    f.barrier()
    f.actor ! Tick
    f.barrier()
    assert(sink.events.isEmpty)
    f.clock.at = t0.minusMinutes(19) // 60 and 30 were used up at startup; only 20 is still to come
    f.actor ! Tick
    f.barrier()
    assert(aheadOf(sink) == Seq(20))
    f.stop()
  }

  // ---- deadline timers (the scheduled actor, on the system clock) ----

  private val scheduled = LiveTimings(scheduled = true, reloadGap = Duration.Zero, ping = 1.hour)

  private def startingIn(d: FiniteDuration, id: Int = 1): Contest = {
    val start = DateTime.now.plusMillis(d.toMillis.toInt)
    Contest(id, s"C$id", start, start.plusHours(5), None, 1)
  }

  private def startedNow(sink: CollectingSink) =
    sink.events.asScala.exists { case e: ContestStarting => e.inMinutes == 0; case _ => false }

  test("the monitor wakes at the start by itself, on its one deadline timer") {
    val sink = new CollectingSink
    val f = new Fixture(DateTime.now, rows = Seq(startingIn(2500.millis)), palevoSink = sink, timings = scheduled, realClock = true)
    awaitCond(startedNow(sink), 8.seconds)
    f.stop()
  }

  test("re-arming from other messages does not drop a wake that is already due") {
    val sink = new CollectingSink
    val f = new Fixture(DateTime.now, rows = Seq(startingIn(2500.millis)), palevoSink = sink, timings = scheduled, realClock = true)
    f.barrier()
    // A message every 20 ms for the whole window in which the wake is due: each one re-arms the timer.
    val deadline = System.currentTimeMillis() + 4500
    while (System.currentTimeMillis() < deadline && !startedNow(sink)) {
      f.actor ! ContestChanged(f.contestRows.head)
      Thread.sleep(20)
    }
    assert(startedNow(sink), "the start was not announced although the timer was due")
    f.stop()
  }

  test("a contest change re-arms the timer for the new deadline") {
    val sink = new CollectingSink
    val f = new Fixture(DateTime.now, rows = Seq(startingIn(1.hour)), palevoSink = sink, timings = scheduled, realClock = true)
    f.barrier()
    assert(!startedNow(sink))
    f.actor ! ContestChanged(startingIn(2500.millis))
    awaitCond(startedNow(sink), 8.seconds)
    assert(sink.events.asScala.exists(_.isInstanceOf[ContestRescheduled]))
    f.stop()
  }

  test("a contest added later is watched by the timer too") {
    val sink = new CollectingSink
    val f = new Fixture(DateTime.now, rows = Seq(startingIn(1.hour)), palevoSink = sink, timings = scheduled, realClock = true)
    f.barrier()
    val added = startingIn(2500.millis, id = 2)
    f.contestRows = Seq(startingIn(1.hour), added)
    f.data = f.data + (2 -> ContestData(problems, teams, Nil))
    f.actor ! ContestChanged(added)
    awaitCond(startedNow(sink), 8.seconds)
    f.stop()
  }

  test("an unknown submit id reloads the contest once and is then applied; still unknown is ignored") {
    val f = new Fixture(running)
    val p = f.subscribe(JoinAdmin(1))
    next(p, "board")
    f.data = Map(1 -> ContestData(problems, teams, Seq(rec(91, 2, "C", running))))
    f.actor ! Finished(1, 91, compiled = true, Some(3.0))
    assert(pending(cellJs(next(p, "board"), 2, "C")) == 1)
    assert(best(rowCellJs(next(p, "row"), "C")).contains("3"))
    assert(f.loads.get == 2)

    EventFilter.warning(pattern = "unknown submit 99", occurrences = 1).intercept {
      f.actor ! Finished(1, 99, compiled = true, Some(3.0))
      awaitCond(f.loads.get == 3, 5.seconds)
    }
    f.actor ! Finished(1, 99, compiled = true, Some(3.0))
    f.barrier()
    Thread.sleep(200)
    assert(f.loads.get == 3)
    f.stop()
  }

  // Constructor throws: a team that is null.
  private val badData = ContestData(problems, Seq(null), Nil)

  test("a contest whose data cannot be built is skipped and retried; the others keep working") {
    var f: Fixture = null
    EventFilter[NullPointerException](occurrences = 1).intercept {
      f = new Fixture(running, rows = Seq(contest(), contest(2)), overrideData = Map(2 -> badData))
      f.barrier()
    }
    next(f.subscribe(JoinAdmin(1)), "board")
    val probe = TestProbe()
    probe.send(f.actor, JoinAdmin(2))
    probe.expectMsgType[akka.actor.Status.Failure](5.seconds)

    f.data = f.data + (2 -> ContestData(problems, teams, Nil))
    // No contest poll any more: the next change of that round retries it.
    f.actor ! ContestChanged(contest(2))
    awaitCond({
      probe.send(f.actor, JoinAdmin(2))
      probe.expectMsgType[AnyRef](5.seconds).isInstanceOf[Source[_, _]]
    }, 5.seconds)
    f.stop()
  }

  test("a failed reload is retried and its unknown id is then applied") {
    val f = new Fixture(running)
    val p = f.subscribe(JoinAdmin(1))
    next(p, "board")
    f.data = Map(1 -> ContestData(problems, teams, Seq(rec(95, 2, "C", running))))
    f.failLoads.set(1)
    EventFilter.warning(pattern = "load failed", occurrences = 1).intercept {
      f.actor ! Finished(1, 95, compiled = true, Some(3.0))
    }
    next(p, "board")
    assert(best(rowCellJs(next(p, "row"), "C")).contains("3"))
    assert(f.loads.get == 3)
    // Not ignored: a rejudge of it is applied.
    f.actor ! RejudgeRequested(95)
    assert(pending(rowCellJs(next(p, "row"), "C")) == 1)
    f.stop()
  }

  test("a failed rebuild is retried and does not ignore the waiting id") {
    val f = new Fixture(running, timings = defaultTimings.copy(reloadGap = 1.second))
    val p = f.subscribe(JoinAdmin(1))
    next(p, "board")
    f.data = Map(1 -> badData)
    EventFilter[NullPointerException](occurrences = 1).intercept {
      f.actor ! Finished(1, 96, compiled = true, Some(3.0))
    }
    f.data = Map(1 -> ContestData(problems, teams, Seq(rec(96, 3, "B", running))))
    next(p, "board")
    assert(best(rowCellJs(next(p, "row"), "B")).contains("3"))
    f.stop()
  }

  test("a rejudge of a submit no contest knows reloads only the contest the database names") {
    val f = new Fixture(running, rows = Seq(contest(), contest(2)))
    val p = f.subscribe(JoinAdmin(1))
    next(p, "board")
    val solved = rec(61, 1, "A", t0.plusMinutes(30), ok)
    f.data = f.data + (1 -> ContestData(problems, teams, Seq(solved)))
    f.owners = Map(61 -> 1)
    val loadsBefore = f.loads.get
    f.actor ! RejudgeRequested(61)
    next(p, "board")
    assert(pending(rowCellJs(next(p, "row"), "A")) == 1)
    assert(f.loads.get == loadsBefore + 1)
    assert(f.lookups.get == 1)
    f.stop()
  }

  test("a rejudge of a submit that is not in the database is ignored without reloading") {
    val f = new Fixture(running)
    f.barrier()
    val loadsBefore = f.loads.get
    EventFilter.warning(pattern = "not in the database", occurrences = 1).intercept {
      f.actor ! RejudgeRequested(62)
      awaitCond(f.lookups.get == 1, 5.seconds)
    }
    f.actor ! RejudgeRequested(62)
    f.barrier()
    assert(f.loads.get == loadsBefore)
    assert(f.lookups.get == 1)
    f.stop()
  }

  test("when the lookup fails a rejudge falls back to reloading every contest") {
    val f = new Fixture(running, rows = Seq(contest(), contest(2)))
    f.barrier()
    f.lookupFails = true
    val loadsBefore = f.loads.get
    EventFilter.warning(pattern = "cannot find the contest of submit 63", occurrences = 1).intercept {
      f.actor ! RejudgeRequested(63)
      awaitCond(f.loads.get == loadsBefore + 2, 5.seconds)
    }
    f.stop()
  }

  test("a rejudge whose finish is lost heals at the first reload after the age limit") {
    val solved = rec(97, 1, "A", t0.plusMinutes(30), ok)
    val f = new Fixture(running, subs = Map(1 -> Seq(solved)), timings = defaultTimings.copy(rejudgeMaxAge = 10.seconds))
    val real = f.subscribe(JoinAdmin(1))
    next(real, "board")
    f.actor ! RejudgeRequested(97)
    assert(pending(rowCellJs(next(real, "row"), "A")) == 1)

    // Reloads are caused by finishes of ids the contest does not know.
    f.clock.ms = 5000
    f.actor ! Finished(1, 998, compiled = true, Some(1.0))
    awaitCond(f.loads.get == 2, 5.seconds)
    real.expectNoMessage(500.millis)
    assert(pending(cellJs(next(f.subscribe(JoinAdmin(1)), "board"), 1, "A")) == 1)

    f.clock.ms = 10000
    f.actor ! Finished(1, 999, compiled = true, Some(1.0))
    assert(best(cellJs(next(real, "board"), 1, "A")).contains("10"))
    f.stop()
  }

  test("a round_changed with an unchanged row (its problems changed) reloads the contest; the new problem appears") {
    val f = new Fixture(running)
    val p = f.subscribe(JoinAdmin(1))
    def ids(b: JsValue) = (b \ "problems").as[Seq[JsValue]].map(p => (p \ "id").as[String])
    assert(ids(next(p, "board")) == problemIds)
    f.data = Map(1 -> ContestData(problems :+ models.ProblemColumn("D", "Problem D", 25), teams, Nil))
    f.actor ! ContestChanged(contest())
    assert(ids(next(p, "board")) == problemIds :+ "D")
    assert(f.loads.get == 2)
    f.stop()
  }

  test("a burst of unchanged-row round_changed during a load coalesces into one more load") {
    val f = new Fixture(running)
    f.barrier()
    val gate = Promise[Unit]()
    f.loadGate = gate.future
    (1 to 10).foreach(_ => f.actor ! ContestChanged(contest()))
    f.barrier()
    assert(f.loads.get == 2)
    gate.success(())
    awaitCond(f.loads.get == 3, 5.seconds)
    f.barrier()
    Thread.sleep(300) // a window in which an extra load would show up
    assert(f.loads.get == 3, "the burst may cause at most two loads")
    f.stop()
  }

  test("a removed round is dropped: its streams end, joins fail, and later events for it are ignored") {
    val f = new Fixture(running, rows = Seq(contest(), contest(2)))
    val p = TestProbe()
    val done = f.join(JoinAdmin(2)).runForeach(e => p.ref ! e)
    next(p, "board")
    f.actor ! ContestRemoved(2)
    scala.concurrent.Await.result(done, 5.seconds)
    val probe = TestProbe()
    probe.send(f.actor, JoinAdmin(2))
    probe.expectMsgType[akka.actor.Status.Failure](5.seconds)
    f.actor ! Arrived(2, rec(81, 1, "A", running))
    next(f.subscribe(JoinAdmin(1)), "board") // contest 1 keeps working
    f.stop()
  }

  test("hooks send fire-and-forget messages") {
    val probe = TestProbe()
    val hooks = new ActorLiveHooks(probe.ref)
    val r = rec(1, 1, "A", running)
    hooks.submitArrived(1, r)
    hooks.finished(1, 1, compiled = true, Some(2.5))
    hooks.rejudged(1)
    hooks.teamsChanged(5)
    hooks.contestRemoved(4)
    probe.expectMsg(Arrived(1, r))
    probe.expectMsg(Finished(1, 1, compiled = true, Some(2.5)))
    probe.expectMsg(RejudgeRequested(1))
    probe.expectMsg(TeamsChanged(5))
    probe.expectMsg(ContestRemoved(4))
    NoopLiveHooks.rejudged(1)
  }
}
