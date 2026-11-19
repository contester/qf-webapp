package actors

import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem}
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.testkit.{TestKit, TestProbe}
import org.joda.time.DateTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.Contest
import palevo._
import play.api.libs.EventSource.Event

import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Try

/**
  * The wiring from the round clock to palevo: StatusActor's RoundClockActor fires an edge, StatusActor maps it with
  * ContestTimeTracker and sends it to the PalevoSink; countdown edges reach no browser. Runs on an injected clock with
  * the clock's Wake delivered by hand; no database.
  */
class StatusActorPalevoSpec extends TestKit(ActorSystem("StatusActorPalevoSpec")) with AnyFunSuiteLike with BeforeAndAfterAll {
  private implicit val mat: Materializer = Materializer(system)

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private val t0 = new DateTime(2026, 11, 18, 10, 0)

  private class ProbeSink extends PalevoSink {
    val events = new java.util.concurrent.ConcurrentLinkedQueue[PalevoEvent]()
    override def send(e: PalevoEvent): Unit = events.add(e)
    def all: Seq[PalevoEvent] = events.asScala.toSeq
  }

  private class Rig(rounds: Seq[Contest], cfg: CountdownConfig = CountdownConfig(Seq(30, 10), Seq(5))) {
    @volatile var now: DateTime = t0
    val sink = new ProbeSink
    val status: ActorRef = system.actorOf(StatusActor.propsWith(null, () => Future.successful(
      StatusActor.StatusActorInitialState(Nil, Map.empty, StatusActor.ClarificationsInitialState(Nil, Nil), rounds)),
      sink, () => now, cfg))
    // Answered once the contests are loaded, which is when the round clock exists.
    locally {
      val p = TestProbe()
      p.send(status, StatusActor.GetAllContests)
      p.expectMsgType[scala.util.Success[StatusActor.AllContests]](10.seconds)
    }
    private val clock = system.actorSelection(status.path / "round-clock")

    def at(t: DateTime): Unit = { now = t; clock ! RoundClockActor.Wake }
    def await(n: Int): Seq[PalevoEvent] = { awaitCond(sink.events.size >= n, 5.seconds); sink.all }
    def quiet(): Unit = { Thread.sleep(300) }
    def stop(): Unit = system.stop(status)
  }

  private def round(id: Int, start: DateTime, end: DateTime) = Contest(id, s"R$id", start, end, None, 1)

  test("a round clock edge reaches the PalevoSink: countdowns, the start, the end, each once") {
    val r = round(1, t0.plusHours(1), t0.plusHours(3))
    val g = new Rig(Seq(r))
    g.quiet()
    assert(g.sink.all.isEmpty) // start-up announces nothing
    g.at(t0.plusMinutes(30))
    assert(g.await(1) == Seq(ContestStarting(PContest(1, "R1"), t0.plusMinutes(30), 30, Some(r.startTime))))
    g.at(t0.plusMinutes(55)) // late: 10 was crossed, still announced as the latest crossed threshold
    g.await(2)
    g.at(t0.plusHours(1))
    g.await(3)
    g.at(t0.plusHours(3).minusMinutes(5))
    g.await(4)
    g.at(t0.plusHours(3))
    val es = g.await(5)
    assert(es.map(e => (e.eventType, e match {
      case s: ContestStarting => s.inMinutes
      case e: ContestEnding => e.inMinutes
      case _ => -1
    })) == Seq(("contest_starting", 30), ("contest_starting", 10), ("contest_starting", 0), ("contest_ending", 5),
      ("contest_ending", 0)))
    g.at(t0.plusHours(4))
    g.quiet()
    assert(g.sink.events.size == 5)
    assert(!es.exists(_.eventType == "contest_freezing"))
    g.stop()
  }

  test("a reschedule goes to palevo once and re-arms the countdown against the new start") {
    val r = round(2, t0.plusHours(1), t0.plusHours(3))
    val g = new Rig(Seq(r))
    val moved = r.copy(startTime = t0.plusHours(2))
    g.status ! StatusActor.ContestChanged(moved)
    val es = g.await(1)
    assert(es == Seq(ContestRescheduled(PContest(2, "R2"), t0, Seq(TimeChange("start", r.startTime, moved.startTime)))))
    g.status ! StatusActor.ContestChanged(moved)
    g.at(t0.plusMinutes(30)) // the old 30-minute threshold: nothing any more
    g.quiet()
    assert(g.sink.events.size == 1)
    g.at(t0.plusMinutes(90))
    assert(g.await(2).last == ContestStarting(PContest(2, "R2"), t0.plusMinutes(90), 30, Some(moved.startTime)))
    g.stop()
  }

  test("test copies above the bound: phase events only, no countdown, no reschedule") {
    val r = round(11, t0.plusHours(1), t0.plusHours(3))
    val g = new Rig(Seq(r))
    g.status ! StatusActor.ContestChanged(r.copy(endTime = t0.plusHours(4)))
    g.at(t0.plusMinutes(50))
    g.at(t0.plusHours(1))
    assert(g.await(1) == Seq(ContestStarting(PContest(11, "R11"), t0.plusHours(1))))
    g.quiet()
    assert(g.sink.events.size == 1)
    g.stop()
  }

  test("countdown edges are not pushed to browsers; the start is") {
    val r = round(3, t0.plusHours(1), t0.plusHours(3))
    val g = new Rig(Seq(r))
    val p = TestProbe()
    p.send(g.status, StatusActor.JoinAdmin(3))
    val src = p.expectMsgType[Try[Source[Event, NotUsed]]](10.seconds).get
    val events = TestProbe()
    src.runForeach(e => if (e.name.exists(_.startsWith("round_"))) events.ref ! e)
    Thread.sleep(300) // the broadcast hubs attach their consumers asynchronously
    g.at(t0.plusMinutes(30))
    g.await(1) // palevo heard the countdown
    events.expectNoMessage(300.millis)
    g.at(t0.plusHours(1))
    assert(events.expectMsgType[Event](5.seconds).name.contains("round_started"))
    g.stop()
  }
}
