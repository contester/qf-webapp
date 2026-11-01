package palevo

import java.util.concurrent.atomic.AtomicInteger

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import play.api.libs.json.JsValue
import scala.collection.mutable

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration._

class PalevoSinkSpec extends AnyFunSuite {
  private val event = ContestStarted(PContest(1, "C"), new DateTime(2026, 11, 1, 12, 0))

  // Runs everything on the calling thread, so tests are deterministic.
  private val sameThread = new ExecutionContext {
    def execute(r: Runnable): Unit = r.run()
    def reportFailure(t: Throwable): Unit = throw t
  }

  private val delays = Seq(1.second, 5.seconds)
  private val schedule: (FiniteDuration, () => Unit) => Unit = (_, f) => f()

  private def sinkWith(results: Seq[Either[Throwable, Int]]) = {
    val posted = ArrayBuffer[JsValue]()
    val n = new AtomicInteger(0)
    val post: JsValue => Future[Int] = j => {
      posted += j
      results(math.min(n.getAndIncrement(), results.size - 1)) match {
        case Right(status) => Future.successful(status)
        case Left(t) => Future.failed(t)
      }
    }
    (new HttpPalevoSink(post, delays, schedule)(sameThread), posted)
  }

  test("posts the event envelope") {
    val (sink, posted) = sinkWith(Seq(Right(202)))
    sink.send(event)
    assert(posted.toList == List(PalevoEvent.toJson(event)))
  }

  test("retries 5xx then succeeds") {
    val (sink, posted) = sinkWith(Seq(Right(503), Right(503), Right(202)))
    sink.send(event)
    assert(posted.size == 3)
  }

  test("gives up after the delays are exhausted") {
    val (sink, posted) = sinkWith(Seq(Right(503)))
    sink.send(event)
    assert(posted.size == 3)
  }

  test("treats 200 (duplicate) as success") {
    val (sink, posted) = sinkWith(Seq(Right(200)))
    sink.send(event)
    assert(posted.size == 1)
  }

  test("does not retry client errors") {
    for (status <- Seq(400, 401, 413, 422)) {
      val (sink, posted) = sinkWith(Seq(Right(status)))
      sink.send(event)
      assert(posted.size == 1, s"status $status")
    }
  }

  test("retries on failed future") {
    val (sink, posted) = sinkWith(Seq(Left(new RuntimeException("boom")), Right(202)))
    sink.send(event)
    assert(posted.size == 2)
  }

  test("send never throws when post throws synchronously") {
    val sink = new HttpPalevoSink(_ => throw new RuntimeException("sync"), delays, schedule)(sameThread)
    sink.send(event) // must not throw; retried and dropped
  }

  test("uses the configured delays for retries") {
    val seen = ArrayBuffer[FiniteDuration]()
    val sink = new HttpPalevoSink(_ => Future.successful(503), delays, (d, f) => { seen += d; f() })(sameThread)
    sink.send(event)
    assert(seen.toList == delays.toList)
  }

  test("Noop does nothing") {
    NoopPalevoSink.send(event)
  }

  private def ev(n: Int) = ContestStarted(PContest(n, s"C$n"), event.ts)

  private def idOf(j: JsValue) = (j \ "id").as[String]

  test("events are delivered in order, one at a time") {
    val promises = ArrayBuffer[Promise[Int]]()
    val posted = ArrayBuffer[String]()
    val sink = new HttpPalevoSink(j => { posted += idOf(j); val p = Promise[Int](); promises += p; p.future },
      delays, schedule)(sameThread)
    sink.send(ev(1)); sink.send(ev(2)); sink.send(ev(3))
    assert(posted.toList == List(ev(1).id)) // the others wait
    promises(0).success(202)
    assert(posted.toList == List(ev(1).id, ev(2).id))
    promises(1).success(202)
    promises(2).success(202)
    assert(posted.toList == List(ev(1).id, ev(2).id, ev(3).id))
  }

  test("a retried event is delivered before the next one") {
    val posted = ArrayBuffer[String]()
    val statuses = mutable.Queue(503, 202, 202)
    val sink = new HttpPalevoSink(j => { posted += idOf(j); Future.successful(statuses.dequeue()) },
      delays, schedule)(sameThread)
    sink.send(ev(1)); sink.send(ev(2))
    assert(posted.toList == List(ev(1).id, ev(1).id, ev(2).id))
  }

  test("a dropped event does not block the ones behind it") {
    val posted = ArrayBuffer[String]()
    val sink = new HttpPalevoSink(j => { posted += idOf(j); Future.successful(if (idOf(j) == ev(1).id) 422 else 202) },
      delays, schedule)(sameThread)
    sink.send(ev(1)); sink.send(ev(2))
    assert(posted.toList == List(ev(1).id, ev(2).id))
  }

  test("backlog is bounded and drops the oldest waiting events") {
    val promises = ArrayBuffer[Promise[Int]]()
    val posted = ArrayBuffer[String]()
    val sink = new HttpPalevoSink(j => { posted += idOf(j); val p = Promise[Int](); promises += p; p.future },
      delays, schedule, maxBacklog = 2)(sameThread)
    (1 to 5).foreach(n => sink.send(ev(n))) // 1 in flight, 2..5 waiting, only the newest two are kept
    promises(0).success(202)
    promises(1).success(202)
    promises(2).success(202)
    assert(posted.toList == List(ev(1).id, ev(4).id, ev(5).id))
  }
}
