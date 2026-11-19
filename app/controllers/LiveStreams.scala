package controllers

import akka.NotUsed
import akka.actor.ActorRef
import akka.pattern.{AskTimeoutException, ask}
import akka.stream.scaladsl.Source
import live.{ConnectionStats, FeedKind}
import play.api.http.{ContentTypes, HeaderNames}
import play.api.libs.EventSource.Event
import play.api.mvc.{Result, Results}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

/** Joins a live monitor stream and turns it into an SSE response; for the admin routes only. */
object LiveStreams extends Results {
  private val askTimeout = 3.seconds
  private val snapshotTimeout = 1.second

  // So a reverse proxy neither caches nor buffers the stream.
  val streamHeaders: Seq[(String, String)] = Seq(HeaderNames.CACHE_CONTROL -> "no-cache", "X-Accel-Buffering" -> "no")

  /** Asks the live monitor for a stream: an unknown contest gives 404, a busy or dead monitor 503. */
  def join(ref: ActorRef, msg: Any)(implicit ec: ExecutionContext): Future[Either[Result, Source[Event, NotUsed]]] =
    ref.ask(msg)(askTimeout).mapTo[Source[Event, NotUsed]].map(Right(_): Either[Result, Source[Event, NotUsed]]).recover {
      case _: NoSuchElementException => Left(NotFound)
      case _: AskTimeoutException => Left(ServiceUnavailable)
    }

  /**
    * The board the viewer's stream will start with, to render into the page. None (an empty shell, filled by the
    * stream) when the live monitor is off, slow, still starting or does not know the contest.
    */
  def snapshot(ref: Option[ActorRef], join: actors.LiveMonitorActor.Join)(implicit ec: ExecutionContext): Future[Option[live.LiveBoard]] =
    ref match {
      case None => Future.successful(None)
      case Some(r) =>
        r.ask(actors.LiveMonitorActor.Snapshot(join))(snapshotTimeout).mapTo[live.LiveBoard].map(Option(_)).recover { case _ => None }
    }

  /** The page is a snapshot of a moving board; nothing may cache it. */
  val pageHeaders: Seq[(String, String)] = Seq(HeaderNames.CACHE_CONTROL -> "no-store")

  /** The admin live board of a round (admins only: nobody else gets a live board). */
  def adminJoin(contestId: Int): actors.LiveMonitorActor.JoinAdmin = actors.LiveMonitorActor.JoinAdmin(contestId)

  /** The rows of the admin status page (every submit of the round). */
  def adminStatusJoin(contestId: Int): actors.LiveMonitorActor.JoinAdminStatus =
    actors.LiveMonitorActor.JoinAdminStatus(contestId)

  def response(source: Source[Event, _]): Result =
    Ok.chunked(source).as(ContentTypes.EVENT_STREAM).withHeaders(streamHeaders: _*)

  /** The kind of feed and the contest a join is counted under on the contester page. */
  def feedOf(j: actors.LiveMonitorActor.Join): (FeedKind, Int) = j match {
    case actors.LiveMonitorActor.JoinAdmin(c) => (FeedKind.AdminMonitor, c)
    case actors.LiveMonitorActor.JoinAdminStatus(c) => (FeedKind.StatusRows, c)
  }

  /** The whole route body for the admin streams, counted in `stats`; 404 without a live monitor. */
  def stream(ref: Option[ActorRef], msg: actors.LiveMonitorActor.Join, stats: ConnectionStats)(implicit ec: ExecutionContext): Future[Result] =
    ref match {
      case None => Future.successful(NotFound)
      case Some(r) =>
        val (kind, contest) = feedOf(msg)
        join(r, msg).map(_.fold(identity, src => response(src.via(stats.track(kind, contest)))))
    }
}
