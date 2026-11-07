package live

import java.io.File
import java.nio.file.Files
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
import org.stingray.contester.dbmodel.{Contest, LocalTeam, Team}
import palevo.{ContestRescheduled, ContestStarting, FirstSolveContest, PalevoEvent, PalevoSink, TimeChange}
import play.api.libs.EventSource.Event
import play.api.libs.json.{JsObject, JsValue, Json}

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

  test("startup loads both boards before handling messages sent during the load") {
    val gate = Promise[Unit]()
    val f = new Fixture(running, gate = gate.future)
    val probe = TestProbe()
    probe.send(f.actor, JoinPublic(1))
    val r = rec(10, 2, "A", running)
    f.actor ! Arrived(1, r)
    probe.expectNoMessage(200.millis)
    gate.success(())
    val src = probe.expectMsgType[Source[Event, NotUsed]](5.seconds)
    val p = TestProbe()
    src.runForeach(e => p.ref ! e)
    val expected = new LiveContest(contest(), problems, teams, Nil, running)
    expected.onArrived(r, running)
    assert(next(p, "board") == LiveJson.board(expected.board(Freezing)))
    f.stop()
  }

  test("JoinPublic starts with the freezing board") {
    val subs = Seq(rec(1, 1, "A", t0.plusMinutes(30), ok), rec(2, 2, "B", t0.plusHours(4).plusMinutes(5), ok))
    val f = new Fixture(frozen, subs = Map(1 -> subs))
    val p = f.subscribe(JoinPublic(1))
    assert(next(p, "board") == LiveJson.board(boardOf(contest(), subs, frozen, Freezing)))
    f.stop()
  }

  test("JoinPublic of a contest that is not public fails") {
    val f = new Fixture(running, rows = Seq(contest(), contest(42)))
    val probe = TestProbe()
    probe.send(f.actor, JoinPublic(42))
    probe.expectMsgType[akka.actor.Status.Failure](5.seconds)
    f.stop()
  }

  test("before start the public, team and freezing admin boards list the teams without problems; the start tick sends the whole board") {
    val f = new Fixture(t0.minusMinutes(5), subs = Map(1 -> Seq(rec(1, 1, "A", t0.minusMinutes(10), ok))))
    val pub = f.subscribe(JoinPublic(1))
    val tm = f.subscribe(JoinTeam(1, 2))
    val adm = f.subscribe(JoinAdmin(1, real = false))
    val real = f.subscribe(JoinAdmin(1, real = true))
    def bare(js: JsValue) = {
      assert((js \ "problems").as[Seq[String]].isEmpty)
      val rows = (js \ "rows").as[Seq[JsValue]]
      assert(rows.map(r => (r \ "team").as[Int]).toSet == Set(1, 2, 3))
      assert(rows.forall(r => (r \ "cells").as[JsObject].fields.isEmpty))
    }
    Seq(pub, tm, adm).foreach(p => bare(next(p, "board")))
    assert((next(real, "board") \ "problems").as[Seq[String]] == problems)
    f.clock.at = t0.plusSeconds(1)
    f.actor ! Tick
    for (p <- Seq(pub, tm, adm)) {
      val b = next(p, "board")
      assert((b \ "rows").as[Seq[JsValue]].size == 3 && (b \ "problems").as[Seq[String]] == problems)
    }
    f.stop()
  }

  test("Arrived then Finished produce row events") {
    val f = new Fixture(running)
    val p = f.subscribe(JoinPublic(1))
    next(p, "board")
    f.actor ! Arrived(1, rec(11, 1, "A", running))
    val r1 = next(p, "row")
    assert((r1 \ "team").as[Int] == 1)
    assert((r1 \ "row" \ "cells" \ "A" \ "t").as[String] == "pend")
    f.actor ! Finished(1, 11, compiled = true, 3, 3)
    val r2 = next(p, "row")
    assert((r2 \ "row" \ "cells" \ "A" \ "t").as[String] == "ok")
    f.stop()
  }

  test("a team gets cells events for its own row only; the freezing admin stream never sees the real board") {
    val f = new Fixture(frozen)
    val t2 = f.subscribe(JoinTeam(1, 2))
    val t3 = f.subscribe(JoinTeam(1, 3))
    val adm = f.subscribe(JoinAdmin(1, real = false))
    val real = f.subscribe(JoinAdmin(1, real = true))
    Seq(t2, t3, adm, real).foreach(next(_, "board"))

    f.actor ! Arrived(1, rec(21, 2, "B", frozen))
    assert((next(t2, "row") \ "team").as[Int] == 2)
    assert((next(t2, "cells") \ "team").as[Int] == 2)
    assert((next(t3, "row") \ "team").as[Int] == 2)
    next(adm, "row")
    next(real, "row")

    f.actor ! Finished(1, 21, compiled = true, 3, 3)
    val own = next(t2, "cells")
    assert((own \ "cells" \ "B" \ "t").as[String] == "ok")
    assert((next(real, "row") \ "row" \ "cells" \ "B" \ "t").as[String] == "ok")
    t3.expectNoMessage(300.millis)
    adm.expectNoMessage(100.millis)

    // A later row about team 2 carries its real cells on its own stream only.
    f.actor ! Arrived(1, rec(22, 2, "C", frozen.plusMinutes(1)))
    assert((next(t2, "row") \ "row" \ "cells" \ "B" \ "t").as[String] == "ok")
    assert((next(t3, "row") \ "row" \ "cells" \ "B" \ "t").as[String] == "pend")
    assert((next(adm, "row") \ "row" \ "cells" \ "B" \ "t").as[String] == "pend")

    // A new team connection gets the overlay in its first board; the public board does not.
    assert(cellT(next(f.subscribe(JoinTeam(1, 2)), "board"), 2, "B") == "ok")
    assert(cellT(next(f.subscribe(JoinPublic(1)), "board"), 2, "B") == "pend")
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

  test("cross-check finds a difference, logs it, replaces the state and sends the board") {
    val f = new Fixture(running)
    val p = f.subscribe(JoinPublic(1))
    next(p, "board")
    val missed = Seq(rec(41, 3, "C", running, ok))
    f.data = Map(1 -> ContestData(problems, teams, missed))
    EventFilter.warning(pattern = "cross-check.*contest 1", occurrences = 1).intercept {
      f.actor ! CrossCheck
    }
    assert(next(p, "board") == LiveJson.board(boardOf(contest(), missed, running, Freezing)))
    f.stop()
  }

  test("with a zero cross-check interval no cross-check runs") {
    val f = new Fixture(running, cfg = defaultCfg.copy(crosscheckInterval = Duration.Zero))
    f.barrier()
    f.actor ! CrossCheck
    f.barrier()
    Thread.sleep(200)
    assert(f.loads.get == 1)
    f.stop()
  }

  test("a submit with a rejudge in flight is excluded from the cross-check") {
    val solved = rec(51, 1, "A", t0.plusMinutes(30), ok)
    val f = new Fixture(running, subs = Map(1 -> Seq(solved)))
    val real = f.subscribe(JoinAdmin(1, real = true))
    next(real, "board")
    f.actor ! RejudgeRequested(51)
    assert((next(real, "row") \ "row" \ "cells" \ "A" \ "t").as[String] == "pend")
    EventFilter.warning(pattern = "cross-check", occurrences = 0).intercept {
      f.actor ! CrossCheck
      awaitCond(f.loads.get == 2, 5.seconds)
      real.expectNoMessage(500.millis)
    }
    assert(cellT(next(f.subscribe(JoinAdmin(1, real = true)), "board"), 1, "A") == "pend")
    f.actor ! Finished(1, 51, compiled = true, 3, 3)
    assert((next(real, "row") \ "row" \ "cells" \ "A" \ "t").as[String] == "ok")
    f.stop()
  }

  test("static file is written once per interval when dirty and not when clean") {
    val dir = Files.createTempDirectory("live-static").toFile
    val f = new Fixture(running, rows = Seq(contest(), contest(11)), staticDir = Some(dir))
    f.barrier()
    f.actor ! StaticCheck
    f.barrier()
    assert(f.rendered.get == 1)
    assert(new File(dir, "1.html").exists() && !new File(dir, "11.html").exists())
    f.actor ! StaticCheck
    f.barrier()
    assert(f.rendered.get == 1)

    f.actor ! Arrived(1, rec(61, 1, "A", running))
    f.actor ! StaticCheck
    f.barrier()
    assert(f.rendered.get == 1)
    f.clock.ms = 30000
    f.actor ! StaticCheck
    f.barrier()
    assert(f.rendered.get == 2)
    f.actor ! StaticCheck
    f.barrier()
    assert(f.rendered.get == 2)
    f.stop()
    dir.listFiles().foreach(_.delete())
    dir.delete()
  }

  test("palevo hears the first solve") {
    val sink = new CollectingSink
    val f = new Fixture(running, palevoSink = sink)
    f.barrier()
    assert(sink.events.isEmpty)
    f.actor ! Arrived(1, rec(71, 2, "B", running))
    f.actor ! Finished(1, 71, compiled = true, 3, 3)
    awaitCond(sink.events.asScala.exists(_.isInstanceOf[FirstSolveContest]), 5.seconds)
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

  test("a sent contest change is announced as a reschedule at once, without a tick") {
    val sink = new CollectingSink
    val f = new Fixture(t0.minusHours(2), palevoSink = sink)
    f.barrier()
    f.actor ! ContestChanged(contest().copy(startTime = t0.plusHours(1)))
    f.barrier()
    val resched = sink.events.asScala.collect { case e: ContestRescheduled => e }.toSeq
    assert(resched.size == 1 && resched.head.changes == Seq(TimeChange("start", t0, t0.plusHours(1))))
    f.stop()
  }

  test("contests above 10 get no countdown and no reschedule announcement") {
    val sink = new CollectingSink
    val f = new Fixture(t0.minusMinutes(61), rows = Seq(contest(11)), palevoSink = sink)
    def barrier11(): Unit = f.join(JoinAdmin(11, real = true))
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

  private val scheduled = LiveTimings(scheduled = true, reloadGap = Duration.Zero, ping = 1.hour, heartbeat = 1.hour)

  private def startingIn(d: FiniteDuration, id: Int = 1): Contest = {
    val start = DateTime.now.plusMillis(d.toMillis.toInt)
    Contest(id, s"C$id", start, start.plusHours(4), start.plusHours(5), start.plusHours(6), None, "ru", schoolMode = false)
  }

  private def startedNow(sink: CollectingSink) =
    sink.events.asScala.exists { case e: ContestStarting => e.inMinutes == 0; case _ => false }

  test("the monitor wakes at the start by itself, without a tick or a heartbeat") {
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
    assert(startedNow(sink), "the start was not announced although the timer was due; it would wait for the heartbeat")
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

  test("a changed contest row reaches the live state without reading submits") {
    val late = rec(81, 1, "A", t0.plusHours(3), ok)
    val now = t0.plusHours(3).plusMinutes(30)
    val f = new Fixture(now, subs = Map(1 -> Seq(late)))
    val p = f.subscribe(JoinPublic(1))
    assert(cellT(next(p, "board"), 1, "A") == "ok")
    f.contestRows = Seq(contest(freeze = t0.plusHours(2)))
    f.actor ! PollContests
    assert(cellT(next(p, "board"), 1, "A") == "pend")
    assert(f.loads.get == 1)
    f.stop()
  }

  test("a contest change that is sent reaches the live state at once, without a poll") {
    val late = rec(81, 1, "A", t0.plusHours(3), ok)
    val f = new Fixture(t0.plusHours(3).plusMinutes(30), subs = Map(1 -> Seq(late)))
    val p = f.subscribe(JoinPublic(1))
    assert(cellT(next(p, "board"), 1, "A") == "ok")
    f.actor ! ContestChanged(contest(freeze = t0.plusHours(2)))
    assert(cellT(next(p, "board"), 1, "A") == "pend")
    assert(f.loads.get == 1)
    f.stop()
  }

  test("a poll that read the database before a contest change was sent does not undo it") {
    val late = rec(81, 1, "A", t0.plusHours(3), ok)
    val f = new Fixture(t0.plusHours(3).plusMinutes(30), subs = Map(1 -> Seq(late)))
    val p = f.subscribe(JoinPublic(1))
    assert(cellT(next(p, "board"), 1, "A") == "ok")
    val gate = scala.concurrent.Promise[Unit]()
    f.pollGate = gate.future
    f.actor ! PollContests // reads the old row, answers later
    f.actor ! ContestChanged(contest(freeze = t0.plusHours(2)))
    assert(cellT(next(p, "board"), 1, "A") == "pend")
    gate.success(())
    f.barrier()
    Thread.sleep(200)
    assert(cellT(next(f.subscribe(JoinPublic(1)), "board"), 1, "A") == "pend")
    // a poll that starts after the change is believed again (here the row went back to the old freeze)
    f.pollGate = Future.successful(())
    f.actor ! PollContests
    assert(cellT(next(p, "board"), 1, "A") == "ok")
    assert(f.loads.get == 1)
    f.stop()
  }

  test("an unknown submit id reloads the contest once and is then applied; still unknown is ignored") {
    val f = new Fixture(running)
    val p = f.subscribe(JoinAdmin(1, real = true))
    next(p, "board")
    f.data = Map(1 -> ContestData(problems, teams, Seq(rec(91, 2, "C", running))))
    f.actor ! Finished(1, 91, compiled = true, 3, 3)
    assert(cellT(next(p, "board"), 2, "C") == "pend")
    assert((next(p, "row") \ "row" \ "cells" \ "C" \ "t").as[String] == "ok")
    assert(f.loads.get == 2)

    EventFilter.warning(pattern = "unknown submit 99", occurrences = 1).intercept {
      f.actor ! Finished(1, 99, compiled = true, 3, 3)
      awaitCond(f.loads.get == 3, 5.seconds)
    }
    f.actor ! Finished(1, 99, compiled = true, 3, 3)
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
    next(f.subscribe(JoinPublic(1)), "board")
    val probe = TestProbe()
    probe.send(f.actor, JoinAdmin(2, real = true))
    probe.expectMsgType[akka.actor.Status.Failure](5.seconds)

    f.data = f.data + (2 -> ContestData(problems, teams, Nil))
    f.actor ! PollContests
    awaitCond({
      probe.send(f.actor, JoinAdmin(2, real = true))
      probe.expectMsgType[AnyRef](5.seconds).isInstanceOf[Source[_, _]]
    }, 5.seconds)
    f.stop()
  }

  test("a failed reload is retried and its unknown id is then applied (no cross-check)") {
    val f = new Fixture(running, cfg = defaultCfg.copy(crosscheckInterval = Duration.Zero))
    val p = f.subscribe(JoinAdmin(1, real = true))
    next(p, "board")
    f.data = Map(1 -> ContestData(problems, teams, Seq(rec(95, 2, "C", running))))
    f.failLoads.set(1)
    EventFilter.warning(pattern = "load failed", occurrences = 1).intercept {
      f.actor ! Finished(1, 95, compiled = true, 3, 3)
    }
    next(p, "board")
    assert((next(p, "row") \ "row" \ "cells" \ "C" \ "t").as[String] == "ok")
    assert(f.loads.get == 3)
    // Not ignored: a rejudge of it is applied.
    f.actor ! RejudgeRequested(95)
    assert((next(p, "row") \ "row" \ "cells" \ "C" \ "t").as[String] == "pend")
    f.stop()
  }

  test("a failed rebuild is retried and does not ignore the waiting id") {
    val f = new Fixture(running, timings = defaultTimings.copy(reloadGap = 1.second))
    val p = f.subscribe(JoinAdmin(1, real = true))
    next(p, "board")
    f.data = Map(1 -> badData)
    EventFilter[NullPointerException](occurrences = 1).intercept {
      f.actor ! Finished(1, 96, compiled = true, 3, 3)
    }
    f.data = Map(1 -> ContestData(problems, teams, Seq(rec(96, 3, "B", running))))
    next(p, "board")
    assert((next(p, "row") \ "row" \ "cells" \ "B" \ "t").as[String] == "ok")
    f.stop()
  }

  test("a rejudge of a submit no contest knows reloads only the contest the database names") {
    val f = new Fixture(running, rows = Seq(contest(), contest(2)))
    val p = f.subscribe(JoinAdmin(1, real = true))
    next(p, "board")
    val solved = rec(61, 1, "A", t0.plusMinutes(30), ok)
    f.data = f.data + (1 -> ContestData(problems, teams, Seq(solved)))
    f.owners = Map(61 -> 1)
    val loadsBefore = f.loads.get
    f.actor ! RejudgeRequested(61)
    next(p, "board")
    assert((next(p, "row") \ "row" \ "cells" \ "A" \ "t").as[String] == "pend")
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

  test("a rejudge whose finish is lost heals at the first cross-check after the age limit") {
    val solved = rec(97, 1, "A", t0.plusMinutes(30), ok)
    val f = new Fixture(running, subs = Map(1 -> Seq(solved)), timings = defaultTimings.copy(rejudgeMaxAge = 10.seconds))
    val real = f.subscribe(JoinAdmin(1, real = true))
    next(real, "board")
    f.actor ! RejudgeRequested(97)
    assert((next(real, "row") \ "row" \ "cells" \ "A" \ "t").as[String] == "pend")

    f.clock.ms = 5000
    EventFilter.warning(pattern = "cross-check", occurrences = 0).intercept {
      f.actor ! CrossCheck
      awaitCond(f.loads.get == 2, 5.seconds)
      real.expectNoMessage(500.millis)
    }
    assert(cellT(next(f.subscribe(JoinAdmin(1, real = true)), "board"), 1, "A") == "pend")

    f.clock.ms = 10000
    EventFilter.warning(pattern = "cross-check.*contest 1", occurrences = 1).intercept {
      f.actor ! CrossCheck
    }
    assert(cellT(next(real, "board"), 1, "A") == "ok")
    f.stop()
  }

  test("hooks send fire-and-forget messages") {
    val probe = TestProbe()
    val hooks = new ActorLiveHooks(probe.ref)
    val r = rec(1, 1, "A", running)
    hooks.submitArrived(1, r)
    hooks.finished(1, 1, compiled = true, 3, 3)
    hooks.rejudged(1)
    probe.expectMsg(Arrived(1, r))
    probe.expectMsg(Finished(1, 1, compiled = true, 3, 3))
    probe.expectMsg(RejudgeRequested(1))
    NoopLiveHooks.rejudged(1)
  }
}
