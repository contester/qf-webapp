package live

import java.io.File
import java.util.concurrent.atomic.AtomicInteger

import actors.LiveMonitorActor
import actors.LiveMonitorActor._
import akka.NotUsed
import akka.actor.ActorRef
import akka.stream.scaladsl.Source
import akka.testkit.{TestKit, TestProbe}
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{Contest, LocalTeam, Team}
import palevo.PalevoSink
import play.api.libs.EventSource.Event
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

/** Shared fixtures for the specs that drive a LiveMonitorActor with a fake loader and clock. */
trait LiveActorFixtures { this: TestKit =>
  val t0 = new DateTime(2026, 11, 1, 10, 0)
  val running = t0.plusHours(1)
  val frozen = t0.plusHours(4).plusMinutes(10)

  def contest(id: Int = 1, freeze: DateTime = t0.plusHours(4)): Contest =
    Contest(id, s"C$id", t0, freeze, t0.plusHours(5), t0.plusHours(6), None, "ru", schoolMode = false)

  def team(id: Int): Team =
    LocalTeam(id, 1, f"School$id%02d", s"S$id", 0, "", notRated = false, noPrint = false, disabled = false)

  val problems = Seq("A", "B", "C")
  val teams = Seq(team(1), team(2), team(3))
  val ok = Done(compiled = true, 3, 3)

  def rec(id: Int, t: Int, p: String, at: DateTime, state: SubmitState = live.Pending): SubmitRec =
    SubmitRec(id, t, p, at, ((at.getMillis - t0.getMillis) / 1000).toInt, state)

  val defaultCfg = LiveMonitorConfig(enabled = true, staticInterval = 30.seconds,
    crosscheckInterval = 60.seconds, publicContests = (1 to 10).toSet, publicMaxConnections = 1000)

  val defaultTimings = LiveTimings(scheduled = false, reloadGap = Duration.Zero, ping = 1.hour)

  class FakeClock(@volatile var at: DateTime) extends LiveClock {
    @volatile var ms: Long = 0L
    override def now(): DateTime = at
    override def millis(): Long = ms
  }

  class Fixture(now: DateTime,
                        rows: Seq[Contest] = Seq(contest()),
                        subs: Map[Int, Seq[SubmitRec]] = Map.empty,
                        cfg: LiveMonitorConfig = defaultCfg,
                        staticDir: Option[File] = None,
                        palevoSink: PalevoSink = palevo.NoopPalevoSink,
                        gate: Future[Unit] = Future.successful(()),
                        timings: LiveTimings = defaultTimings,
                        overrideData: Map[Int, ContestData] = Map.empty,
                        render: (Contest, LiveBoard) => String = (c, b) => s"${c.id}:${b.rows.size}") {
    val clock = new FakeClock(now)
    @volatile var contestRows: Seq[Contest] = rows
    @volatile var data: Map[Int, ContestData] =
      rows.map(c => c.id -> ContestData(problems, teams, subs.getOrElse(c.id, Nil))).toMap ++ overrideData
    val loads = new AtomicInteger
    // Number of upcoming loads that fail.
    val failLoads = new AtomicInteger
    val rendered = new AtomicInteger

    private val loader = new ContestLoader {
      override def load(c: Contest)(implicit ec: ExecutionContext): Future[ContestData] = {
        loads.incrementAndGet()
        if (failLoads.getAndUpdate(n => (n - 1).max(0)) > 0) Future.failed(new RuntimeException("injected load failure"))
        else gate.map(_ => data(c.id))
      }
    }

    val actor: ActorRef = system.actorOf(LiveMonitorActor.propsWith(cfg, staticDir, () => Future.successful(contestRows),
      loader, (c: Contest, b: LiveBoard) => { rendered.incrementAndGet(); render(c, b) },
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

  def next(p: TestProbe, name: String): JsValue = {
    val e = p.expectMsgType[Event](5.seconds)
    assert(e.name.contains(name), e)
    Json.parse(e.data)
  }

  def boardOf(c: Contest, subs: Seq[SubmitRec], now: DateTime, kind: BoardKind): LiveBoard =
    new LiveContest(c, problems, teams, subs, now).board(kind)

  def cellT(js: JsValue, team: Int, p: String): String =
    ((js \ "rows").as[Seq[JsValue]].find(r => (r \ "team").as[Int] == team).get \ "cells" \ p \ "t").as[String]

}
