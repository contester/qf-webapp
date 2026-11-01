package palevo

import java.util.concurrent.atomic.AtomicInteger

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import play.api.libs.json.JsValue

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}
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
}
