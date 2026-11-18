package controllers

import java.util.concurrent.atomic.AtomicInteger

import actors.LiveMonitorActor
import live.{ConnectionStats, FeedKind, LiveMonitorConfig, LiveMonitorRef}
import models.{Anonymous, BoardSource}
import org.joda.time.DateTime
import play.api.i18n.Messages
import play.api.mvc._
import views.html

import scala.concurrent.Future

/** The unauthenticated scoreboard: a static page of any round once it is over, and the live stream of the public
 * contests (Task 17). No session or cookie is read or created here. */
class PublicMonitor(cc: ControllerComponents, liveConfig: LiveMonitorConfig, liveMonitor: LiveMonitorRef,
                    boards: BoardSource, stats: ConnectionStats) extends AbstractController(cc) {
  private implicit val ec = defaultExecutionContext

  private val connections = new AtomicInteger(0)

  private[controllers] def publicConnections: Int = connections.get

  // The live monitor actor, if it is running and the contest is public.
  private def liveFor(contestId: Int) =
    liveMonitor.ref.filter(_ => liveConfig.enabled && liveConfig.publicContests.contains(contestId))

  /** The public scoreboard of a round: 403 until the round is over, then the points board rendered from the DB. */
  def page(contestId: Int): Action[AnyContent] = pageAt(contestId, () => DateTime.now)

  private[controllers] def pageAt(contestId: Int, now: () => DateTime): Action[AnyContent] = Action.async { implicit request =>
    implicit val messages: Messages = messagesApi.preferred(request.acceptLanguages) // no cookie is read here
    boards.round(contestId).flatMap { round =>
      BoardAccess.decide(round, Anonymous, now()) match {
        case Left(result) => Future.successful(result)
        case Right(contest) => boards.board(contest.id).map(b => Ok(html.staticmonitor(contest, b)))
      }
    }
  }

  // TODO(task17): enforce Visibility on this stream (the public live board is removed by Task 17).
  def events(contestId: Int) = Action.async {
    liveFor(contestId) match {
      case None => Future.successful(NotFound)
      case Some(ref) =>
        if (connections.incrementAndGet() > liveConfig.publicMaxConnections) {
          connections.decrementAndGet()
          Future.successful(ServiceUnavailable)
        } else {
          LiveStreams.join(ref, LiveMonitorActor.JoinPublic(contestId)).transform { result =>
            // Anything but a stream handed to the client gives the slot back now.
            if (!result.toOption.exists(_.isRight)) connections.decrementAndGet()
            result
          }.map {
            case Left(r) => r
            case Right(source) =>
              LiveStreams.response(source.via(stats.track(FeedKind.PublicMonitor, contestId)).watchTermination() { (m, done) =>
                done.onComplete(_ => connections.decrementAndGet())
                m
              })
          }
        }
    }
  }
}
