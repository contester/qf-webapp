package controllers

import akka.NotUsed
import akka.actor.ActorRef
import akka.pattern.{AskTimeoutException, ask}
import akka.stream.scaladsl.Source
import play.api.http.{ContentTypes, HeaderNames}
import play.api.libs.EventSource.Event
import play.api.mvc.{Result, Results}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

/** Joins a live monitor stream and turns it into an SSE response; shared by the team, admin and public routes. */
object LiveStreams extends Results {
  private val askTimeout = 3.seconds

  // So a reverse proxy neither caches nor buffers the stream.
  val streamHeaders: Seq[(String, String)] = Seq(HeaderNames.CACHE_CONTROL -> "no-cache", "X-Accel-Buffering" -> "no")

  /** Asks the live monitor for a stream: an unknown contest gives 404, a busy or dead monitor 503. */
  def join(ref: ActorRef, msg: Any)(implicit ec: ExecutionContext): Future[Either[Result, Source[Event, NotUsed]]] =
    ref.ask(msg)(askTimeout).mapTo[Source[Event, NotUsed]].map(Right(_): Either[Result, Source[Event, NotUsed]]).recover {
      case _: NoSuchElementException => Left(NotFound)
      case _: AskTimeoutException => Left(ServiceUnavailable)
    }

  /** Only an account that can see everything gets the real board; every other admin gets the freezing one. */
  def adminJoin(contestId: Int, canSeeAll: Boolean): actors.LiveMonitorActor.JoinAdmin =
    actors.LiveMonitorActor.JoinAdmin(contestId, real = canSeeAll)

  def response(source: Source[Event, _]): Result =
    Ok.chunked(source).as(ContentTypes.EVENT_STREAM).withHeaders(streamHeaders: _*)

  /** The whole route body for the team and admin streams; 404 when the live monitor is disabled. */
  def stream(ref: Option[ActorRef], msg: Any)(implicit ec: ExecutionContext): Future[Result] =
    ref match {
      case None => Future.successful(NotFound)
      case Some(r) => join(r, msg).map(_.fold(identity, response))
    }
}
