package palevo

import akka.actor.ActorSystem
import play.api.{Configuration, Logging}
import play.api.libs.json.JsValue
import play.api.libs.ws.WSClient

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scala.util.control.NonFatal

/** Fire-and-forget delivery of events to palevo. Implementations never throw and never block. */
trait PalevoSink {
  def send(e: PalevoEvent): Unit
}

object NoopPalevoSink extends PalevoSink {
  override def send(e: PalevoEvent): Unit = ()
}

/**
  * Posts events through `post` (which returns the HTTP status), one at a time and in the order they were sent,
  * so announcements cannot overtake each other. 2xx is success; 400/401/413/422 are dropped (retrying cannot
  * help); anything else, including failures, is retried after each of `delays`, then dropped. Events waiting
  * behind a slow one are kept up to `maxBacklog`; beyond that the oldest waiting event is dropped.
  */
final class HttpPalevoSink(post: JsValue => Future[Int],
                           delays: Seq[FiniteDuration],
                           schedule: (FiniteDuration, () => Unit) => Unit,
                           maxBacklog: Int = 1000)
                          (implicit ec: ExecutionContext) extends PalevoSink with Logging {
  private val permanentFailures = Set(400, 401, 413, 422)

  private val lock = new Object
  private val backlog = mutable.Queue[PalevoEvent]()
  private var busy = false

  override def send(e: PalevoEvent): Unit = {
    val start = lock.synchronized {
      if (backlog.size >= maxBacklog) {
        val dropped = backlog.dequeue()
        logger.error(s"palevo: backlog full, dropping ${dropped.id}")
      }
      backlog.enqueue(e)
      if (busy) false else { busy = true; true }
    }
    if (start) pump()
  }

  /** Delivers waiting events one after another; `busy` stays set until the backlog is empty. */
  private def pump(): Unit = {
    val next = lock.synchronized {
      if (backlog.isEmpty) { busy = false; None } else Some(backlog.dequeue())
    }
    next.foreach(deliver(_, () => pump()))
  }

  private def deliver(e: PalevoEvent, done: () => Unit): Unit =
    try attempt(e, PalevoEvent.toJson(e), delays, done)
    catch {
      case NonFatal(t) =>
        logger.error(s"palevo: cannot send ${e.id}", t)
        done()
    }

  private def attempt(e: PalevoEvent, json: JsValue, remaining: Seq[FiniteDuration], done: () => Unit): Unit = {
    val result = try post(json) catch { case NonFatal(t) => Future.failed(t) }
    result.onComplete {
      case Success(status) if status >= 200 && status < 300 =>
        logger.debug(s"palevo: ${e.id} delivered ($status)")
        done()
      case Success(status) if permanentFailures(status) =>
        logger.error(s"palevo: ${e.id} rejected with $status, dropping")
        done()
      case Success(status) => retry(e, json, remaining, s"status $status", done)
      case Failure(t) => retry(e, json, remaining, t.toString, done)
    }
  }

  private def retry(e: PalevoEvent, json: JsValue, remaining: Seq[FiniteDuration], why: String,
                    done: () => Unit): Unit =
    remaining match {
      case next +: rest =>
        logger.warn(s"palevo: ${e.id} failed ($why), retrying in $next")
        try schedule(next, () => attempt(e, json, rest, done))
        catch {
          case NonFatal(t) =>
            logger.error(s"palevo: cannot schedule retry of ${e.id}", t)
            done()
        }
      case _ =>
        logger.error(s"palevo: ${e.id} failed ($why), giving up")
        done()
    }
}

object PalevoSink {
  /** An HTTP sink when both palevo.url and palevo.token are configured, otherwise a no-op. */
  def fromConfig(cfg: Configuration, ws: WSClient, system: ActorSystem)(implicit ec: ExecutionContext): PalevoSink =
    (cfg.getOptional[String]("palevo.url"), cfg.getOptional[String]("palevo.token")) match {
      case (Some(url), Some(token)) if url.nonEmpty && token.nonEmpty =>
        import play.api.libs.ws.JsonBodyWritables._
        val post: JsValue => Future[Int] = json =>
          ws.url(url)
            .withRequestTimeout(5.seconds)
            .addHttpHeaders("Authorization" -> s"Bearer $token")
            .post(json)
            .map(_.status)
        new HttpPalevoSink(post, Seq(1.second, 5.seconds),
          (d, f) => { system.scheduler.scheduleOnce(d)(f()); () })
      case _ => NoopPalevoSink
    }
}
