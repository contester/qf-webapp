package controllers

import actors.LiveMonitorActor.JoinPublic
import akka.actor.{ActorRef, ActorSystem, Status}
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import akka.testkit.{TestActor, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import live.{LiveContestLookup, LiveMonitorConfig, LiveMonitorRef}
import org.joda.time.DateTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.Eventually
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.Contest
import play.api.http.{DefaultFileMimeTypes, FileMimeTypesConfiguration}
import play.api.i18n.{DefaultLangs, DefaultMessagesApi}
import play.api.libs.EventSource.Event
import play.api.libs.typedmap.TypedMap
import play.api.mvc._
import play.api.mvc.request.{RemoteConnection, RequestFactory, RequestTarget}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class LiveMonitorControllerSpec extends TestKit(ActorSystem("LiveMonitorControllerSpec", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |akka.use-slf4j = off
    |""".stripMargin))) with AnyFunSuiteLike
  with BeforeAndAfterAll with Eventually {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private implicit val mat: Materializer = Materializer(system)
  private implicit val ec: ExecutionContext = system.dispatcher

  private val cc: ControllerComponents = {
    val parsers = PlayBodyParsers()
    DefaultControllerComponents(DefaultActionBuilder(parsers.defaultBodyParser), parsers, new DefaultMessagesApi(),
      new DefaultLangs(), new DefaultFileMimeTypes(FileMimeTypesConfiguration()), ec)
  }

  private def cfg(enabled: Boolean = true, max: Int = 1000) =
    LiveMonitorConfig(enabled, 30.seconds, 60.seconds, (1 to 10).toSet, max)

  private val t0 = DateTime.now.minusHours(1)
  private def contest(id: Int) = Contest(id, s"C$id", t0, t0.plusHours(4), t0.plusHours(5), t0.plusHours(6), None, "en", schoolMode = false)
  private val lookup = LiveContestLookup(id => Future.successful(Some(contest(id))))

  private val boardEvent = Event("{}", None, Some("board"))

  // A probe that answers every JoinPublic with a one-event stream (finite unless `endless`).
  private def liveProbe(endless: Boolean = false): TestProbe = {
    val p = TestProbe()
    p.setAutoPilot((sender: ActorRef, msg: Any) => {
      msg match {
        case JoinPublic(_) =>
          sender ! (if (endless) Source.single(boardEvent).concat(Source.maybe[Event]) else Source.single(boardEvent))
        case _ =>
      }
      TestActor.KeepRunning
    })
    p
  }

  private def controller(probe: Option[TestProbe], config: LiveMonitorConfig = cfg()) =
    new PublicMonitor(cc, config, LiveMonitorRef(probe.map(_.ref)), lookup)

  private def get(path: String): Request[AnyContent] =
    RequestFactory.plain.createRequest(RemoteConnection("127.0.0.1", secure = false, None), "GET",
      RequestTarget(path, path, Map.empty), "HTTP/1.1", Headers(), TypedMap.empty, AnyContentAsEmpty)

  private def await[T](f: Future[T]): T = Await.result(f, 5.seconds)

  private def run(c: PublicMonitor, action: Action[AnyContent], path: String): Result = await(action(get(path)))

  test("public page and stream: 404 for a contest that is not public, without asking the live monitor") {
    val probe = liveProbe()
    val c = controller(Some(probe))
    assert(run(c, c.page(11), "/public/monitor/11").header.status == 404)
    assert(run(c, c.events(11), "/public/monitor/11/events").header.status == 404)
    probe.expectNoMessage(100.millis)
  }

  test("public page and stream: 404 when the live monitor is disabled") {
    val c = controller(None, cfg(enabled = false))
    assert(run(c, c.page(1), "/public/monitor/1").header.status == 404)
    assert(run(c, c.events(1), "/public/monitor/1/events").header.status == 404)
  }

  test("public stream: 404 when the live monitor does not know the contest") {
    val probe = TestProbe()
    probe.setAutoPilot((sender: ActorRef, _: Any) => {
      sender ! Status.Failure(new NoSuchElementException("no contest"))
      TestActor.KeepRunning
    })
    val c = controller(Some(probe))
    assert(run(c, c.events(2), "/public/monitor/2/events").header.status == 404)
    assert(c.publicConnections == 0)
  }

  test("public stream: 200, event stream, no buffering headers, no session or cookie, board first") {
    val probe = liveProbe()
    val c = controller(Some(probe))
    val r = run(c, c.events(1), "/public/monitor/1/events")
    probe.expectMsg(JoinPublic(1))
    assert(r.header.status == 200)
    assert(r.body.contentType.exists(_.startsWith("text/event-stream")))
    assert(r.header.headers.get("Cache-Control").contains("no-cache"))
    assert(r.header.headers.get("X-Accel-Buffering").contains("no"))
    assert(r.newSession.isEmpty && r.newCookies.isEmpty && !r.header.headers.contains("Set-Cookie"))
    val body = await(r.body.dataStream.runWith(Sink.fold("")(_ + _.utf8String)))
    assert(body.startsWith("event: board\n"))
  }

  test("public page: 200 shell without a session, pointing at the public stream") {
    val c = controller(Some(liveProbe()))
    val r = run(c, c.page(1), "/public/monitor/1")
    assert(r.header.status == 200)
    assert(r.newSession.isEmpty && r.newCookies.isEmpty && !r.header.headers.contains("Set-Cookie"))
    val body = await(r.body.consumeData).utf8String
    assert(body.contains("/public/monitor/1/events"))
    assert(body.contains("liveMonitor("))
    assert(!body.contains("location.reload"))
    // The live monitor did not answer the snapshot in time: the shell is served empty and the stream fills it.
    assert(!body.contains("<table"))
    assert(r.header.headers.get("Cache-Control").contains("no-store"))
  }

  test("public page: renders the snapshot board into the page when the live monitor answers") {
    val probe = TestProbe()
    val board = live.LiveBoard.build(Seq("A"), Seq(org.stingray.contester.dbmodel.LocalTeam(7, 1, "School07", "S7", 0, "", notRated = false, noPrint = false, disabled = false)), Map.empty)
    probe.setAutoPilot((sender: ActorRef, msg: Any) => {
      msg match {
        case actors.LiveMonitorActor.Snapshot(JoinPublic(1)) => sender ! board
        case _ =>
      }
      TestActor.KeepRunning
    })
    val c = controller(Some(probe))
    val r = run(c, c.page(1), "/public/monitor/1")
    assert(r.header.status == 200)
    val body = await(r.body.consumeData).utf8String
    assert(body.contains("""<tr data-team="7""""))
    assert(body.contains("liveMonitor("))
    assert(r.header.headers.get("Cache-Control").contains("no-store"))
  }

  test("public stream: the (max + 1)-th concurrent stream gets 503; a finished stream frees its slot") {
    val c = controller(Some(liveProbe(endless = true)), cfg(max = 2))
    val first = run(c, c.events(1), "/public/monitor/1/events")
    val second = run(c, c.events(1), "/public/monitor/1/events")
    assert(first.header.status == 200 && second.header.status == 200)
    assert(run(c, c.events(1), "/public/monitor/1/events").header.status == 503)
    assert(c.publicConnections == 2)

    // The client goes away after the first event: the stream terminates and releases its slot.
    await(first.body.dataStream.take(1).runWith(Sink.ignore))
    eventually(timeout(3.seconds)) { assert(c.publicConnections == 1) }
    assert(run(c, c.events(1), "/public/monitor/1/events").header.status == 200)
  }
}
