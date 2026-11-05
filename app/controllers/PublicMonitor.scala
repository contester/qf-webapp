package controllers

import java.util.concurrent.atomic.AtomicInteger

import actors.LiveMonitorActor
import live.{LiveContestLookup, LiveMonitorConfig, LiveMonitorRef}
import play.api.mvc._
import views.html

import scala.concurrent.Future

/** The unauthenticated live monitor: a page and a stream of the freezing board for the public contests.
 * No session or cookie is read or created here. */
class PublicMonitor(cc: ControllerComponents, liveConfig: LiveMonitorConfig, liveMonitor: LiveMonitorRef,
                    contests: LiveContestLookup) extends AbstractController(cc) {
  private implicit val ec = defaultExecutionContext

  private val connections = new AtomicInteger(0)

  private[controllers] def publicConnections: Int = connections.get

  // The live monitor actor, if it is running and the contest is public.
  private def liveFor(contestId: Int) =
    liveMonitor.ref.filter(_ => liveConfig.enabled && liveConfig.publicContests.contains(contestId))

  def page(contestId: Int) = Action.async { implicit request =>
    liveFor(contestId) match {
      case None => Future.successful(NotFound)
      case Some(_) =>
        val messages = messagesApi.preferred(request.acceptLanguages)
        contests.get(contestId).flatMap {
          case Some(contest) =>
            LiveStreams.snapshot(liveFor(contestId), actors.LiveMonitorActor.JoinPublic(contestId)).map { initial =>
              Ok(html.publiclive(contest, routes.PublicMonitor.events(contestId).url, initial)(messages))
                .withHeaders(LiveStreams.pageHeaders: _*)
            }
          case None => Future.successful(NotFound)
        }
    }
  }

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
              LiveStreams.response(source.watchTermination() { (m, done) =>
                done.onComplete(_ => connections.decrementAndGet())
                m
              })
          }
        }
    }
  }
}
