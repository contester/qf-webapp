package controllers

import java.nio.charset.StandardCharsets

import actors.StatusActor
import com.mohiva.play.silhouette.api.{Authorization, Silhouette}
import com.mohiva.play.silhouette.impl.authenticators.SessionAuthenticator
import com.spingo.op_rabbit._
import models._
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.http.ContentTypes
import play.api.i18n.I18nSupport
import play.api.libs.json._
import play.api.mvc._
import play.api.{Configuration, Logger}
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import utils.FormUtil
import utils.auth.TeamsEnv
import views.html

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class SubmitData(problem: String, compiler: Int, inline: String)

case class SubmitMessage(id: Int)

object SubmitMessage {
  implicit val formatSubmitMessage = Json.format[SubmitMessage]
}

class Application (cc: ControllerComponents,
                             silhouette: Silhouette[TeamsEnv],
                             dbConfig: DatabaseConfig[JdbcProfile],
                             monitorModel: Monitor,
                            rabbitMqModel: RabbitMqModel,
                            statusActorModel: StatusActorModel,
                            configuration: Configuration) extends AbstractController(cc) with I18nSupport{

  private val db = dbConfig.db
  import dbConfig.profile.api._

  import scala.language.postfixOps

  private val rabbitMq = rabbitMqModel.rabbitMq

  private def getProblems(contest: Int)(implicit ec: ExecutionContext) =
    monitorModel.problemClient.getProblems(contest)

  private def getProblemsAndCompilers(contestId: Int)(implicit ec: ExecutionContext) =
    getProblems(contestId).zip(db.run(Contests.getCompilers(contestId).result))

  import scala.concurrent.ExecutionContext.Implicits.global

  def monitorDefault = silhouette.SecuredAction.async { implicit request =>
    monitorModel.getMonitor(request.identity.contest.id, overrideFreeze = false).map(x => {
      Ok(html.loggedinmonitor(x.get.contest, x.get.status, request.identity))})
  }

  private def getSubmits(team: LoggedInTeam) =
    db.run(Submits.getContestTeamSubmits(team.contest.id, team.team.localId))

  private implicit val standardTimeout: akka.util.Timeout = {
    import scala.concurrent.duration._
    Duration(5, SECONDS)
  }

  import utils.Db._

  def index = silhouette.SecuredAction.async { implicit request =>
    getSubmits(request.identity).flatMap(Submits.groupAndAnnotate(db, request.identity.contest.schoolMode, _)).map { subs =>
      Ok(html.index(request.identity, subs))
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
    getProblemsAndCompilers(request.identity.contest.id).map {
      case (problems, compilers) => {
        Ok(sendSolutionForm(request.identity, submitForm, problems,
          compilers))
      }
    }
  }

  private def submitInsertQuery(contestId: Int, teamId: Int, problemId: String, srcLang: Int, source: Array[Byte], remoteAddr: String) =
    sqlu"""insert into NewSubmits (Contest, Team, Problem, SrcLang, Source, Computer, Arrived)
          values ($contestId, $teamId, $problemId, $srcLang, $source, inet_aton($remoteAddr), CURRENT_TIMESTAMP())
        """.andThen(sql"""select LAST_INSERT_ID()""".as[Long]).withPinnedSession

  import com.spingo.op_rabbit.PlayJsonSupport._

  def submitPost = silhouette.SecuredAction(parse.multipartFormData).async { implicit request =>
    getProblemsAndCompilers(request.identity.contest.id).flatMap {
      case (problems, compilers) =>
        val parsed = submitForm.bindFromRequest

        parsed.fold(
          formWithErrors => {
            Future.successful(Some(formWithErrors))
          },
          submitData => {
            val solutionBytes = FormUtil.inlineOrFile(submitData.inline, request.body.file("file")).getOrElse(FormUtil.emptyBytes)
            if (solutionBytes.isEmpty) {
              Future.successful(Some(parsed.withGlobalError("No solution")))
            } else if (!request.identity.contest.running) {
              Future.successful(Some(parsed.withGlobalError("Contest is not running")))
            } else {
              db.run(submitInsertQuery(request.identity.contest.id, request.identity.team.localId, submitData.problem,
                submitData.compiler, solutionBytes, request.remoteAddress)).map { wat =>
                Logger.info(s"Inserted submit id: $wat")
                rabbitMq ! Message.queue(SubmitMessage(wat.head.toInt), queue = "contester.submitrequests")

                None
              }
            }
          }
        ).map {
          case Some(form) => BadRequest(sendSolutionForm(request.identity, form,
            problems, compilers))
          case None => Redirect(routes.Application.index)
        }
    }
  }

  def ackMessage = silhouette.SecuredAction.async { implicit request =>
    request.body.asFormUrlEncoded.flatMap(_.get("msgid")).flatMap(_.headOption).flatMap(x => Try(x.toInt).toOption).foreach { msgid =>
      Logger.info(s"Acking: $msgid")
      statusActorModel.statusActor ! StatusActor.Ack(request.identity, msgid)
    }
    Future.successful(Ok("ok"))
  }

  private case class canSeeSubmit(submitId: Int) extends Authorization[LoggedInTeam, SessionAuthenticator] {
    override def isAuthorized[B](identity: LoggedInTeam, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] =
      db.run(sql"select Contest, Team from NewSubmits where ID = $submitId".as[ContestTeamIds]).map { ids =>
        ids.exists(identity.matching)
      }
  }

  def showSubmit(submitId: Int) = silhouette.SecuredAction(canSeeSubmit(submitId)).async { implicit request =>
    Submits.getSubmitById(db, submitId).map(x => Ok(html.showsubmit(request.identity, x)))
  }

  def feed(contestId: Int, teamId: Int) = silhouette.SecuredAction.async { implicit request =>
    import akka.pattern.ask

    import scala.concurrent.duration._

    statusActorModel.statusActor.ask(StatusActor.JoinUser(contestId, teamId))(Duration(5, SECONDS)).map {
      case StatusActor.UserJoined(e) => {
        Ok.chunked(e).as(ContentTypes.EVENT_STREAM)
      }
      case _ => BadRequest("foo")
    }
  }

  // TODO: dafuq is this
  def getCompilerOutput(testingId: Int) = silhouette.SecuredAction.async { implicit request =>
    db.run(
      sql"""select TesterOutput, TesterError
           from Results where UID = $testingId and Test = 0 order by Test""".as[(String, String)].headOption)
      .map { opt =>
        opt.map { res =>
          Ok(Json.obj("output" -> res._1, "error" -> res._2))
        }.getOrElse(BadRequest(Json.obj()))
      }
  }
}