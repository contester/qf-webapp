package live

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Keep, Sink, Source}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.Eventually
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Millis, Seconds, Span}

import play.api.libs.json.Json

import scala.concurrent.duration._
import scala.concurrent.{Await, Promise}

class ConnectionStatsSpec extends AnyFunSuite with BeforeAndAfterAll with Eventually {
  implicit val system: ActorSystem = ActorSystem("connection-stats-spec")
  implicit override val patienceConfig: PatienceConfig = PatienceConfig(Span(3, Seconds), Span(20, Millis))

  override protected def afterAll(): Unit = {
    system.terminate()
    ()
  }

  test("counts are kept per contest and per kind, and a closed connection is taken off") {
    val s = new ConnectionStats
    s.opened(FeedKind.PublicMonitor, 12)
    s.opened(FeedKind.PublicMonitor, 12)
    s.opened(FeedKind.TeamFeed, 12)
    s.opened(FeedKind.AdminFeed, 13)
    assert(s.snapshot == Map(
      12 -> Map(FeedKind.PublicMonitor -> 2, FeedKind.TeamFeed -> 1),
      13 -> Map(FeedKind.AdminFeed -> 1)))
    s.closed(FeedKind.PublicMonitor, 12)
    s.closed(FeedKind.AdminFeed, 13)
    // A contest with no connection left has no row.
    assert(s.snapshot == Map(12 -> Map(FeedKind.PublicMonitor -> 1, FeedKind.TeamFeed -> 1)))
  }

  test("a close without an open never makes a count negative") {
    val s = new ConnectionStats
    s.closed(FeedKind.TeamMonitor, 5)
    assert(s.snapshot.isEmpty)
  }

  test("a tracked stream counts from its start to its end, however it ends") {
    val s = new ConnectionStats
    val p = Promise[Option[Int]]()
    Source.future(p.future).via(s.track(FeedKind.AdminMonitorRestricted, 3)).runWith(Sink.ignore)
    eventually(assert(s.snapshot == Map(3 -> Map(FeedKind.AdminMonitorRestricted -> 1))))
    p.success(None)
    eventually(assert(s.snapshot.isEmpty))

    val q = Promise[Option[Int]]()
    Source.future(q.future).via(s.track(FeedKind.TeamMonitor, 3)).runWith(Sink.ignore)
    eventually(assert(s.snapshot == Map(3 -> Map(FeedKind.TeamMonitor -> 1))))
    q.failure(new RuntimeException("client went away"))
    eventually(assert(s.snapshot.isEmpty))
  }

  test("listeners hear of every change, and stop hearing once removed") {
    val s = new ConnectionStats
    @volatile var heard = 0
    val l = s.listen(() => heard += 1)
    s.opened(FeedKind.PublicMonitor, 1)
    s.closed(FeedKind.PublicMonitor, 1)
    assert(heard == 2)
    l.close()
    s.opened(FeedKind.PublicMonitor, 1)
    assert(heard == 2)
  }

  test("the JSON has a row per contest and every kind in the same order for each") {
    val s = new ConnectionStats
    s.opened(FeedKind.AdminMonitorUnrestricted, 9)
    val j = s.json
    assert((j \ "kinds").as[Seq[String]] == FeedKind.all.map(_.id))
    assert((j \ "contests" \ "9").as[Seq[Int]] == FeedKind.all.map(k => if (k == FeedKind.AdminMonitorUnrestricted) 1 else 0))
  }

  test("the event stream starts with the counts, tells of each change, and lets go of the listener when it ends") {
    val s = new ConnectionStats
    s.opened(FeedKind.PublicMonitor, 1)
    val q = s.events(10.millis).toMat(Sink.queue())(Keep.right).run()
    def next() = Await.result(q.pull(), 3.seconds).get
    val first = next()
    assert(first.name.contains("counts"))
    assert(Json.parse(first.data) == s.json)
    s.opened(FeedKind.TeamFeed, 2)
    val second = next()
    assert(Json.parse(second.data) == s.json)
    assert((Json.parse(second.data) \ "contests" \ "2").isDefined)
    q.cancel()
    eventually(assert(s.listenerCount == 0))
  }
}
