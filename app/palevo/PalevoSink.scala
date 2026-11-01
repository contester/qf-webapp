package palevo

import akka.actor.ActorSystem
import play.api.{Configuration, Logging}
import play.api.libs.json.JsValue
import play.api.libs.ws.WSClient

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
  * Posts events through `post` (which returns the HTTP status). 2xx is success; 400/401/413/422 are dropped
  * (retrying cannot help); anything else, including failures, is retried after each of `delays`, then dropped.
  */
final class HttpPalevoSink(post: JsValue => Future[Int],
                           delays: Seq[FiniteDuration],
                           schedule: (FiniteDuration, () => Unit) => Unit)
                          (implicit ec: ExecutionContext) extends PalevoSink with Logging {
  private val permanentFailures = Set(400, 401, 413, 422)

  override def send(e: PalevoEvent): Unit =
    try attempt(e, PalevoEvent.toJson(e), delays)
    catch { case NonFatal(t) => logger.error(s"palevo: cannot send ${e.id}", t) }

  private def attempt(e: PalevoEvent, json: JsValue, remaining: Seq[FiniteDuration]): Unit = {
    val result = try post(json) catch { case NonFatal(t) => Future.failed(t) }
    result.onComplete {
      case Success(status) if status >= 200 && status < 300 =>
        logger.debug(s"palevo: ${e.id} delivered ($status)")
      case Success(status) if permanentFailures(status) =>
        logger.error(s"palevo: ${e.id} rejected with $status, dropping")
      case Success(status) => retry(e, json, remaining, s"status $status")
      case Failure(t) => retry(e, json, remaining, t.toString)
    }
  }

  private def retry(e: PalevoEvent, json: JsValue, remaining: Seq[FiniteDuration], why: String): Unit =
    remaining match {
      case next +: rest =>
        logger.warn(s"palevo: ${e.id} failed ($why), retrying in $next")
        try schedule(next, () => attempt(e, json, rest))
        catch { case NonFatal(t) => logger.error(s"palevo: cannot schedule retry of ${e.id}", t) }
      case _ =>
        logger.error(s"palevo: ${e.id} failed ($why), giving up")
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
