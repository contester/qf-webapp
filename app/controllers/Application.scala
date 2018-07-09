package controllers

import java.nio.charset.StandardCharsets

import actors.StatusActor
import com.mohiva.play.silhouette.api.{Authorization, Silhouette}
import com.mohiva.play.silhouette.impl.authenticators.SessionAuthenticator
import com.spingo.op_rabbit._
import javax.inject.{Inject, Singleton}
import models.ContesterResults.{CustomTestResult, FinishedTesting}
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
import utils.auth.TeamsEnv
import views.html

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class SubmitData(problem: String, compiler: Int, inline: String)

case class SubmitMessage(id: Int)

object SubmitMessage {
  implicit val formatSubmitMessage = Json.format[SubmitMessage]
}

@Singleton
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
  import com.spingo.op_rabbit.PlayJsonSupport._

  import akka.pattern.ask

  import scala.concurrent.duration._

  implicit private val recoveryStrategy = RecoveryStrategy.none

  val finishedRef = Subscription.run(rabbitMq) {
    import Directives._
    channel(qos = 1) {
      import play.api.libs.concurrent.Execution.Implicits.defaultContext
      consume(queue("contester.finished")) {
        body(as[FinishedTesting]) { submit =>
          Logger.info(s"Received $submit")
          val acked = statusActorModel.statusActor.ask(submit)(1 minute)
          ack(acked)
        }
      }
    }
  }

  val finishedEvalRef = Subscription.run(rabbitMq) {
    import Directives._
    channel(qos = 1) {
      import play.api.libs.concurrent.Execution.Implicits.defaultContext
      consume(queue("contester.evals")) {
        body(as[CustomTestResult]) { submit =>
          Logger.info(s"Received $submit")
          val acked = statusActorModel.statusActor.ask(submit)(1 minute)
          ack(acked)
        }
      }
    }
  }

  private def getProblems(contest: Int)(implicit ec: ExecutionContext) =
    monitorModel.problemClient.getProblems(contest).map(Problems.toSelect)

  private def getProblemsAndCompilers(contestId: Int)(implicit ec: ExecutionContext) =
    getProblems(contestId).zip(db.run(Contests.getCompilers(contestId)))

  import scala.concurrent.ExecutionContext.Implicits.global

  def monitorDefault = silhouette.SecuredAction.async { implicit request =>
    monitorModel.getMonitor(request.identity.contest.id, false).map(x => {
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

  private def compilersForForm(compilers: Seq[Compiler]) =
    Seq(("", "Выберите компилятор")) ++ compilers.sortBy(_.name).map(x => x.id.toString -> x.name)

  def submit = silhouette.SecuredAction.async { implicit request =>
    getProblemsAndCompilers(request.identity.contest.id).map {
      case (problems, compilers) => {
        Ok(html.sendsolution(request.identity, submitForm, Seq[(String, String)](("", "Выберите задачу")) ++ problems,
          compilersForForm(compilers)))
      }
    }
  }

  private def submitInsertQuery(contestId: Int, teamId: Int, problemId: String, srcLang: Int, source: Array[Byte], remoteAddr: String) =
    sqlu"""insert into NewSubmits (Contest, Team, Problem, SrcLang, Source, Computer, Arrived)
          values ($contestId, $teamId, $problemId, $srcLang, $source, inet_aton($remoteAddr), CURRENT_TIMESTAMP())
        """.andThen(sql"""select LAST_INSERT_ID()""".as[Long]).withPinnedSession

  def submitPost = silhouette.SecuredAction(parse.multipartFormData).async { implicit request =>
    getProblemsAndCompilers(request.identity.contest.id).flatMap {
      case (problems, compilers) =>
        val parsed = submitForm.bindFromRequest

        val solutionOpt = request.body.file("file").map { solution =>
          FileUtils.readFileToByteArray(solution.ref.file)
        }

        //val parsed0 = if (solutionOpt.isDefined) parsed
        //else parsed.withGlobalError("can't open the file")

        parsed.fold(
          formWithErrors => {
            Future.successful(BadRequest(html.sendsolution(request.identity, formWithErrors, problems,
              compilersForForm(compilers))))
          },
          submitData => {
            if (submitData.inline.isEmpty && solutionOpt.isEmpty) {
              Future.successful(BadRequest(html.sendsolution(request.identity, parsed.withGlobalError("No solution"),
                problems, compilersForForm(compilers))))
            } else
            if (!request.identity.contest.running) {
              Future.successful(BadRequest(html.sendsolution(request.identity, parsed.withGlobalError("Contest is not running"),
                problems, compilersForForm(compilers))))
            } else {
              val df = if (!submitData.inline.isEmpty) submitData.inline.getBytes(StandardCharsets.UTF_8) else solutionOpt.getOrElse(submitData.inline.getBytes(StandardCharsets.UTF_8))
              db.run(submitInsertQuery(request.identity.contest.id, request.identity.team.localId, submitData.problem,
                submitData.compiler, df, request.remoteAddress)).map { wat =>

                Logger.info(s"$wat")
                rabbitMq ! Message.queue(SubmitMessage(wat.head.toInt), queue = "contester.submitrequests")

                Redirect(routes.Application.index)
              }
            }
          }
        )
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
      sql"""select Test, Result, Timex, Memory, Info, TesterExitCode, TesterOutput, TesterError
           from Results where UID = $testingId and Test = 0 order by Test""".as[ResultEntry])
      .map(_.headOption)
      .map { opt =>
        opt.map { res =>
          Ok(Json.obj("output" -> res.testerOutput, "error" -> res.testerError))
        }.getOrElse(BadRequest(Json.obj()))
      }
  }
}