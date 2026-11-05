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
import palevo.{FirstSolveContest, PalevoEvent, PalevoSink}
import play.api.libs.EventSource.Event
import play.api.libs.json.{JsValue, Json}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}

class LiveMonitorActorSpec extends TestKit(ActorSystem("LiveMonitorActorSpec", ConfigFactory.parseString(
  """
    |akka.loggers = ["akka.testkit.TestEventListener"]
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |akka.use-slf4j = off
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  // Streams of a stopped actor still report their end to it.
  system.eventStream.publish(TestEvent.Mute(EventFilter.warning(pattern = "received dead letter.*Unregister")))

  private val t0 = new DateTime(2026, 11, 1, 10, 0)
  private val running = t0.plusHours(1)
  private val frozen = t0.plusHours(4).plusMinutes(10)

  private def contest(id: Int = 1, freeze: DateTime = t0.plusHours(4)): Contest =
    Contest(id, s"C$id", t0, freeze, t0.plusHours(5), t0.plusHours(6), None, "ru", schoolMode = false)

  private def team(id: Int): Team =
    LocalTeam(id, 1, f"School$id%02d", s"S$id", 0, "", notRated = false, noPrint = false, disabled = false)

  private val problems = Seq("A", "B", "C")
  private val teams = Seq(team(1), team(2), team(3))
  private val ok = Done(compiled = true, 3, 3)

  private def rec(id: Int, t: Int, p: String, at: DateTime, state: SubmitState = live.Pending): SubmitRec =
    SubmitRec(id, t, p, at, ((at.getMillis - t0.getMillis) / 1000).toInt, state)

  private val defaultCfg = LiveMonitorConfig(enabled = true, staticInterval = 30.seconds,
    crosscheckInterval = 60.seconds, publicContests = (1 to 10).toSet, publicMaxConnections = 1000)

  private val timings = LiveTimings(scheduled = false, reloadGap = Duration.Zero, ping = 1.hour)

  private class FakeClock(@volatile var at: DateTime) extends LiveClock {
    @volatile var ms: Long = 0L
    override def now(): DateTime = at
    override def millis(): Long = ms
  }

  private class CollectingSink extends PalevoSink {
    val events = new java.util.concurrent.ConcurrentLinkedQueue[PalevoEvent]()
    override def send(e: PalevoEvent): Unit = events.add(e)
  }

  private class ThrowOnceSink extends PalevoSink {
    val calls = new AtomicInteger
    override def send(e: PalevoEvent): Unit =
      if (calls.incrementAndGet() == 1) throw new IllegalStateException("injected palevo failure")
  }

  private class Fixture(now: DateTime,
                        rows: Seq[Contest] = Seq(contest()),
                        subs: Map[Int, Seq[SubmitRec]] = Map.empty,
                        cfg: LiveMonitorConfig = defaultCfg,
                        staticDir: Option[File] = None,
                        palevoSink: PalevoSink = palevo.NoopPalevoSink,
                        gate: Future[Unit] = Future.successful(())) {
    val clock = new FakeClock(now)
    @volatile var contestRows: Seq[Contest] = rows
    @volatile var data: Map[Int, ContestData] =
      rows.map(c => c.id -> ContestData(problems, teams, subs.getOrElse(c.id, Nil))).toMap
    val loads = new AtomicInteger
    val rendered = new AtomicInteger

    private val loader = new ContestLoader {
      override def load(c: Contest)(implicit ec: ExecutionContext): Future[ContestData] = {
        loads.incrementAndGet()
        gate.map(_ => data(c.id))
      }
    }

    val actor: ActorRef = system.actorOf(LiveMonitorActor.propsWith(cfg, staticDir, () => Future.successful(contestRows),
      loader, (c: Contest, b: LiveBoard) => { rendered.incrementAndGet(); s"${c.id}:${b.rows.size}" },
      palevoSink, clock, timings))

    def join(m: Any): Source[Event, NotUsed] = {
      val p = TestProbe()
      p.send(actor, m)
      p.expectMsgType[Source[Event, NotUsed]](5.seconds)
    }

    def subscribe(m: Any): TestProbe = {
      val p = TestProbe()
      join(m).runForeach(e => p.ref ! e)
      p
    }

    // A reply to a join proves every earlier message has been handled.
    def barrier(): Unit = join(JoinAdmin(1, real = true))

    def stop(): Unit = system.stop(actor)
  }

  private def next(p: TestProbe, name: String): JsValue = {
    val e = p.expectMsgType[Event](5.seconds)
    assert(e.name.contains(name), e)
    Json.parse(e.data)
  }

  private def boardOf(c: Contest, subs: Seq[SubmitRec], now: DateTime, kind: BoardKind): LiveBoard =
    new LiveContest(c, problems, teams, subs, now).board(kind)

  private def cellT(js: JsValue, team: Int, p: String): String =
    ((js \ "rows").as[Seq[JsValue]].find(r => (r \ "team").as[Int] == team).get \ "cells" \ p \ "t").as[String]

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

  test("before start the public and team boards are empty; the start tick sends the whole board") {
    val f = new Fixture(t0.minusMinutes(5))
    val pub = f.subscribe(JoinPublic(1))
    val tm = f.subscribe(JoinTeam(1, 2))
    val adm = f.subscribe(JoinAdmin(1, real = false))
    val empty = LiveJson.board(LiveBoard(Nil, Vector.empty, Map.empty))
    assert(next(pub, "board") == empty)
    assert(next(tm, "board") == empty)
    assert((next(adm, "board") \ "rows").as[Seq[JsValue]].size == 3)
    f.clock.at = t0.plusSeconds(1)
    f.actor ! Tick
    val b = next(pub, "board")
    assert((b \ "rows").as[Seq[JsValue]].size == 3 && (b \ "problems").as[Seq[String]] == problems)
    assert((next(tm, "board") \ "rows").as[Seq[JsValue]].size == 3)
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
