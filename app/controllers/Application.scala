package controllers

import java.io.File
import javax.inject.{Inject, Singleton}

import actors.StatusActor
import akka.actor.{Props, ActorSystem}
import com.spingo.op_rabbit._
import jp.t2v.lab.play2.auth.AuthElement
import models.ContesterResults.{CustomTestResult, FinishedTesting}
import models._
import org.apache.commons.io.{Charsets, FileUtils}
import play.api.{Configuration, Logger}
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.EventSource
import play.api.libs.iteratee.Iteratee
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import play.twirl.api.Html
import slick.driver.JdbcProfile
import views.html

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class SubmitData(problem: String, compiler: Int)

case class SubmitMessage(id: Int)

object SubmitMessage {
  implicit val formatSubmitMessage = Json.format[SubmitMessage]
}

@Singleton
class Application @Inject() (dbConfigProvider: DatabaseConfigProvider,
                             monitorModel: Monitor,
                            rabbitMqModel: RabbitMqModel,
                            statusActorModel: StatusActorModel,
                            configuration: Configuration,
                             val auth: AuthWrapper,
                             val messagesApi: MessagesApi) extends Controller with AuthElement with AuthConfigImpl with I18nSupport{

  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import dbConfig.driver.api._
  import utils.Db._
  import scala.language.postfixOps

  private val rabbitMq = rabbitMqModel.rabbitMq
  import com.spingo.op_rabbit.PlayJsonSupport._

  val userPermissions = new UserPermissions(db)

  def monitorDefault = AsyncStack(AuthorityKey -> UserPermissions.any) { implicit request =>
    val loggedInTeam = loggedIn
    implicit val ec = StackActionExecutionContext
    monitorModel.getMonitor(loggedInTeam.contest.id, false).map(x => {
      Ok(html.loggedinmonitor(x.get.contest, x.get.status, loggedInTeam))})
  }

  private def getSubmits(team: LoggedInTeam) =
    db.run(Submits.getContestTeamSubmits(team.contest.id, team.team.localId))

  def index = AsyncStack(AuthorityKey -> UserPermissions.any) { implicit request =>
    val loggedInTeam = loggedIn
    implicit val ec = StackActionExecutionContext

    getSubmits(loggedInTeam).flatMap(Submits.groupAndAnnotate(db, loggedInTeam.contest.schoolMode, _)).map { subs =>
      Ok(html.index(loggedInTeam, subs))
    }
  }

  def additionalInfo = AsyncStack(AuthorityKey -> UserPermissions.any) { implicit request =>
    implicit val ec = StackActionExecutionContext

    val loggedInTeam = loggedIn

    db.run(sql"select Content from Extrainfo where Contest = ${loggedInTeam.contest.id}".as[String]).map { content =>
      Ok(html.addoninfo(loggedIn, content.headOption.map(Html.apply)))
    }
  }

  private def getProblems(contest: Int)(implicit ec: ExecutionContext) =
    db.run(Contests.getProblems(contest)).map(Problems.toSelect(_))

  val submitForm = Form {
    mapping("problem" -> text, "compiler" -> number)(SubmitData.apply)(SubmitData.unapply)
  }

  private def compilersForForm(compilers: Seq[Compiler]) =
    compilers.map(x => x.id.toString -> x.name)

  private def getProblemsAndCompilers(contestId: Int)(implicit ec: ExecutionContext) =
    getProblems(contestId).zip(db.run(Contests.getCompilers(contestId)))

  def submit = AsyncStack(AuthorityKey -> UserPermissions.any) { implicit request =>
    val loggedInTeam = loggedIn
    implicit val ec = StackActionExecutionContext

    getProblemsAndCompilers(loggedInTeam.contest.id).map {
      case (problems, compilers) => {
        Ok(html.sendsolution(loggedInTeam, submitForm, problems, compilersForForm(compilers)))
      }
    }
  }


  def submitInsertQuery(contestId: Int, teamId: Int, problemId: String, srcLang: Int, source: Array[Byte], remoteAddr: String) =
    sqlu"""insert into NewSubmits (Contest, Team, Problem, SrcLang, Source, Computer, Arrived)
          values ($contestId, $teamId, $problemId, $srcLang, $source, inet_aton($remoteAddr), CURRENT_TIMESTAMP())
        """.andThen(sql"""select LAST_INSERT_ID()""".as[Long]).withPinnedSession

  def submitPost = AsyncStack(parse.multipartFormData, AuthorityKey -> UserPermissions.any) { implicit request =>
    val loggedInTeam = loggedIn
    implicit val ec = StackActionExecutionContext

    getProblemsAndCompilers(loggedInTeam.contest.id).flatMap {
      case (problems, compilers) =>
        val parsed = submitForm.bindFromRequest
        val solutionOpt = request.body.file("file").map { solution =>
          FileUtils.readFileToByteArray(solution.ref.file)
        }

        val parsed0 = if (solutionOpt.isDefined) parsed
        else parsed.withGlobalError("can't open the file")

        parsed0.fold(
          formWithErrors => {
            Future.successful(BadRequest(html.sendsolution(loggedInTeam, formWithErrors, problems,
              compilersForForm(compilers))))
          },
          submitData => {
            if (loggedIn.contest.finished || !loggedIn.contest.started) {
              Future.successful(BadRequest(html.sendsolution(loggedInTeam, parsed0.withGlobalError("Contest is not running"),
                problems, compilersForForm(compilers))))
            } else {
              db.run(submitInsertQuery(loggedInTeam.contest.id, loggedInTeam.team.localId, submitData.problem,
                submitData.compiler, solutionOpt.get, request.remoteAddress)).map { wat =>

                Logger.info(s"$wat")
                rabbitMq ! Message.queue(SubmitMessage(wat.head.toInt), queue = "contester.submitrequests")

                Redirect(routes.Application.index)
              }
            }
          }
        )
    }
  }

  //import play.api.Play.current
  import play.api.mvc._

  import scala.concurrent.duration._
  import akka.pattern.ask

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

  def ackMessage = AsyncStack(AuthorityKey -> UserPermissions.any) { implicit request =>
    implicit val ec = StackActionExecutionContext

    request.body.asFormUrlEncoded.flatMap(_.get("msgid")).flatMap(_.headOption).flatMap(x => Try(x.toInt).toOption).foreach { msgid =>
      Logger.info(s"Acking: $msgid")
      statusActorModel.statusActor ! StatusActor.Ack(loggedIn, msgid)
    }
    Future.successful(Ok("ok"))
  }

  def showSubmit(submitId: Int) = AsyncStack(AuthorityKey -> userPermissions.submit(submitId)) { implicit request =>
    implicit val ec = StackActionExecutionContext
    Submits.getSubmitById(db, submitId).map(x => Ok(html.showsubmit(loggedIn, x)))
  }

  def feed(contestId: Int, teamId: Int) = AsyncStack(AuthorityKey -> UserPermissions.any) { implicit request =>
    implicit val ec = StackActionExecutionContext

    import scala.concurrent.duration._
    import akka.pattern.ask

    statusActorModel.statusActor.ask(StatusActor.JoinUser(contestId, teamId))(Duration(5, SECONDS)).map {
      case StatusActor.UserJoined(e) => {
        Ok.feed(e).as("text/event-stream")
      }
      case _ => BadRequest("foo")
    }
  }

  def getCompilerOutput(testingId: Int) = AsyncStack(AuthorityKey -> UserPermissions.any) { implicit request =>
    implicit val ec = StackActionExecutionContext

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