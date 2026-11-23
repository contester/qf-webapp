package controllers

import java.nio.charset.StandardCharsets

import actors.StatusActor
import com.mohiva.play.silhouette.api.{Authorization, Silhouette}
import com.mohiva.play.silhouette.impl.authenticators.SessionAuthenticator
import models._
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.http.ContentTypes
import play.api.i18n.I18nSupport
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc._
import play.api.{Configuration, Logger, Logging}
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import utils.FormUtil
import utils.auth.TeamsEnv
import views.html

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class SubmitData(problem: String, compiler: Int, inline: String)

class Application (cc: ControllerComponents,
                             silhouette: Silhouette[TeamsEnv],
                             dbConfig: DatabaseConfig[JdbcProfile],
                             monitorModel: Monitor,
                            statusActorModel: StatusActorModel,
                            configuration: Configuration,
                            liveHooks: live.LiveHooks,
                            liveMonitor: live.LiveMonitorRef,
                            stats: live.ConnectionStats,
                            ws: WSClient) extends AbstractController(cc) with I18nSupport with Logging {

  private val db = dbConfig.db
  private val fileserverBaseOpt = configuration.getOptional[String]("fileserver.url").map(_.stripSuffix("fs/"))
  private val shortn = configuration.getOptional[String]("fileserver.shortn").getOrElse("")
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel._

  import scala.language.postfixOps

  // The problems of the contestant's round and the languages of the round's season, straight from the database.
  private[this] def getProblemsAndCompilers(contest: Contest)(implicit ec: ExecutionContext) =
    db.run(SlickModel.problems.filter(_.contestID === contest.id).sortBy(_.id).result)
      .zip(db.run(Contests.getCompilers(contest).result))

  import scala.concurrent.ExecutionContext.Implicits.global

  // The participant's live points board (no freeze; teams only before the start). Falls back to a DB render when no snapshot is available.
  def monitorDefault = silhouette.SecuredAction.async { implicit request =>
    val team = request.identity
    val now = org.joda.time.DateTime.now
    monitorModel.source.round(team.contest.id).flatMap { round =>
      BoardAccess.decide(round, TeamViewer(team.team.id, team.contest.id), now) match {
        case Left(result) => Future.successful(result)
        case Right(contest) =>
          LiveStreams.snapshot(liveMonitor.ref, LiveStreams.teamJoin(contest.id)).flatMap {
            case Some(initial) =>
              Future.successful(Ok(html.livemonitor(contest, team, initial))
                .withHeaders(LiveStreams.pageHeaders: _*))
            case None =>
              monitorModel.source.board(contest.id).map { board =>
                val visible = if (Visibility.isStarted(contest, now)) board else MonitorPoints.teamsOnly(board)
                Ok(html.loggedinmonitor(contest, visible, team))
                  .withHeaders(LiveStreams.pageHeaders: _*)
              }
          }
      }
    }
  }

  def monitorEvents = silhouette.SecuredAction.async { implicit request =>
    LiveStreams.stream(liveMonitor.ref, LiveStreams.teamJoin(request.identity.contest.id), stats)
  }

  private[this] def getSubmits(team: LoggedInTeam) = {
    db.run(Submits.getContestTeamSubmits(team.contest.id, team.team.id))
  }

  private implicit val standardTimeout: akka.util.Timeout = {
    import scala.concurrent.duration._
    Duration(5, SECONDS)
  }

  def index = silhouette.SecuredAction.async { implicit request =>
    getSubmits(request.identity).flatMap(x =>
      Submits.groupAndAnnotate(db, x.map(Submits.upliftSub(_))
      )).map { subs =>
      Ok(html.index(request.identity, subs)).withHeaders(LiveStreams.pageHeaders: _*)
    }
  }

  def showExtraInfo(num: Int) = silhouette.SecuredAction.async { implicit request =>
    Future.successful(Ok(html.addoninfo(request.identity, request.identity.einfo.find(_.num == num))))
  }

  val submitForm = Form {
    mapping("problem" -> text, "compiler" -> number, "inline" -> text)(SubmitData.apply)(SubmitData.unapply)
  }

  private def sendSolutionForm(identity: LoggedInTeam, form: Form[SubmitData], problems: Seq[Problem], compilers: Seq[Compiler])(implicit request: RequestHeader) =
    html.sendsolution(identity, form,
      Problems.forForm(problems), Compilers.forForm(compilers))

  def submit = silhouette.SecuredAction.async { implicit request =>
    getProblemsAndCompilers(request.identity.contest).map {
      case (problems, compilers) => {
        Ok(sendSolutionForm(request.identity, submitForm, problems,
          compilers))
      }
    }
  }

  // Tells the live monitor about a new submit; the arrival time is the one the database assigned. Never throws.
  private[this] def liveArrived(team: LoggedInTeam, id: Long, problem: String, arrivedAt: org.joda.time.DateTime): Unit =
    Try {
      val seconds = org.joda.time.Seconds.secondsBetween(team.contest.startTime, arrivedAt).getSeconds
      liveHooks.submitArrived(team.contest.id, live.SubmitRec(id.toInt, team.team.id, problem, arrivedAt, seconds, live.Pending))
    }.failed.foreach(e => logger.warn(s"live monitor: submit $id arrival not sent", e))

  def submitPost = silhouette.SecuredAction(parse.multipartFormData).async { implicit request =>
    getProblemsAndCompilers(request.identity.contest).flatMap {
      case (problems, compilers) =>
        val parsed = submitForm.bindFromRequest

        parsed.fold(
          formWithErrors => Future.successful(SubmitOutcome.Invalid(formWithErrors): SubmitOutcome),
          submitData => {
            val solutionBytes = FormUtil.inlineOrFile(submitData.inline, request.body.file("file")).getOrElse(FormUtil.emptyBytes)
            if (solutionBytes.isEmpty) {
              Future.successful(SubmitOutcome.Rejected("submit.error.noSolution"): SubmitOutcome)
            } else {
              val now = org.joda.time.DateTime.now
              Submits.insert(db, request.identity, submitData.problem, submitData.compiler, solutionBytes,
                Some(request.remoteAddress), now).map { res =>
                res.foreach { id =>
                  logger.info(s"Inserted submit id: $id")
                  liveArrived(request.identity, id, submitData.problem, now)
                }
                SubmitOutcome.of(res)
              }
            }
          }
        ).map(outcome => SubmitFlow.respond(parsed, outcome,
          form => sendSolutionForm(request.identity, form, problems, compilers), Redirect(routes.Application.index)))
    }
  }

  def ackMessage = silhouette.SecuredAction.async { implicit request =>
    request.body.asFormUrlEncoded.flatMap(_.get("msgid")).flatMap(_.headOption).flatMap(x => Try(x.toLong).toOption).foreach { msgid =>
      statusActorModel.statusActor ! StatusActor.Ack(request.identity, msgid)
    }
    Future.successful(Ok("ok"))
  }

  private case class canSeeSubmit(submitId: Long) extends Authorization[LoggedInTeam, SessionAuthenticator] {
    override def isAuthorized[B](identity: LoggedInTeam, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] = {
      db.run(SlickModel.submits.filter(x => (x.id === submitId && x.contest === identity.contest.id && x.team === identity.team.id)).exists.result)
    }
  }

  def showSubmit(submitId: Int) = silhouette.SecuredAction(canSeeSubmit(submitId)).async { implicit request =>
    Submits.getSubmitById(db, submitId).flatMap {
      case Some(sub) =>
        val compileF: Future[Option[CompileOutput]] = (for {
          base <- fileserverBaseOpt
          tid <- sub.fsub.submit.testingId
          if sub.fsub.details.exists(d => d.test == 0 && !d.success)
        } yield Outputs.getCompileAssets(ws, base, shortn, submitId, tid)).getOrElse(Future.successful(None))
        val rowsF = sub.fsub.submit.testingId.map(t => Submits.loadTestRows(db, t.toLong, request2Messages(request))).getOrElse(Future.successful(Nil))
        compileF.zip(rowsF).map { case (compileOut, rows) =>
          Ok(html.showsubmit(request.identity, Some(sub), rows, compileOut)).withHeaders(LiveStreams.pageHeaders: _*)
        }
      case None =>
        Future.successful(Ok(html.showsubmit(request.identity, None, Nil, None)).withHeaders(LiveStreams.pageHeaders: _*))
    }
  }

  private case class canSeeFeed(contestId: Int, teamId: Int) extends Authorization[LoggedInTeam, SessionAuthenticator] {
    override def isAuthorized[B](identity: LoggedInTeam, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] =
      Future.successful(identity.contest.id == contestId && identity.team.id == teamId)
  }

  def feed(contestId: Int, teamId: Int) = silhouette.SecuredAction(canSeeFeed(contestId, teamId)).async { implicit request =>
    import akka.pattern.ask

    import scala.concurrent.duration._

    statusActorModel.statusActor.ask(StatusActor.JoinUser(contestId, teamId))(Duration(5, SECONDS)).map {
      case StatusActor.UserJoined(e) => {
        Ok.chunked(e.via(stats.track(live.FeedKind.TeamFeed, contestId))).as(ContentTypes.EVENT_STREAM).withHeaders(LiveStreams.streamHeaders: _*)
      }
      case _ => BadRequest("foo")
    }
  }
}