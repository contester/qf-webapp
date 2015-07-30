package controllers

import java.sql.Timestamp
import javax.inject.{Inject, Singleton}

import actors.StatusActor
import akka.actor.{Props, ActorSystem}
import com.spingo.op_rabbit.{UTF8StringMarshaller, RabbitControl}
import com.spingo.op_rabbit.consumer.Subscription
import jp.t2v.lab.play2.auth.AuthElement
import models._
import org.apache.commons.io.FileUtils
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.iteratee.Iteratee
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import slick.driver.JdbcProfile
import views.html

import scala.concurrent.Future

case class SubmitData(problem: String, compiler: Int)

case class SubmitObject(id: Int, team: Int, contest: Int,  problem: String,
                        schoolMode: Boolean)

case class FinishedTesting(submit: SubmitObject, testingId: Int, compiled: Boolean, passed: Int, taken: Int)

@Singleton
class Application @Inject() (dbConfigProvider: DatabaseConfigProvider,
                             monitorModel: Monitor,
                             system: ActorSystem,
                             val auth: AuthWrapper,
                             val messagesApi: MessagesApi) extends Controller with AuthElement with AuthConfigImpl with I18nSupport{

  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import dbConfig.driver.api._
  import utils.Db._

  val rabbitMq = system.actorOf(Props[RabbitControl])
  import com.spingo.op_rabbit.PlayJsonSupport._
  import play.api.libs.functional.syntax._

  implicit val submitObjectFormat = Json.format[SubmitObject]
  implicit val finishedTestingFormat = Json.format[FinishedTesting]



  def monitor(id: Int) = Action.async { implicit request =>
    monitorModel.getMonitor(id, false).map(x => Ok(html.monitor(x.contest, x.status)))
  }

  def monitorDefault = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn
    monitorModel.getMonitor(loggedInTeam.contest.id, false).map(x => {
      Ok(html.loggedinmonitor(x.contest, x.status, loggedInTeam))})
  }

  private def anyUser(account: LoggedInTeam): Future[Boolean] = Future.successful(true)

  private def getSubmits(team: LoggedInTeam) =
    db.run(Submits.getContestTeamSubmits(team.contest.id, team.team.localId))

  private def annot8(submits: Seq[Submit], schoolMode: Boolean) =
    if (schoolMode)
      Submits.annotateSchoolSubmits(db, submits)
    else
      Submits.annotateACMSubmits(db, submits)

  def index = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn

    getSubmits(loggedInTeam).flatMap(annot8(_, loggedInTeam.contest.schoolMode)).map { subs =>
      Ok(html.index(loggedInTeam, subs))
    }
  }

  private def getProblems(contest: Int) =
    db.run(Contests.getProblems(contest)).map(Problems.toSelect(_))

  val submitForm = Form {
    mapping("problem" -> text, "compiler" -> number)(SubmitData.apply)(SubmitData.unapply)
  }

  private def compilersForForm(compilers: Seq[Compiler]) =
    compilers.map(x => x.id.toString -> x.name)

  private def getProblemsAndCompilers(contestId: Int) =
    getProblems(contestId).zip(db.run(Contests.getCompilers(contestId)))

  def submit = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn

    getProblemsAndCompilers(loggedInTeam.contest.id).map {
      case (problems, compilers) => {
        Ok(html.sendsolution(loggedInTeam, submitForm, problems, compilersForForm(compilers)))
      }
    }
  }


  def submitInsertQuery(contestId: Int, teamId: Int, problemId: String, srcLang: Int, source: Array[Byte], remoteAddr: String) =
    sqlu"""insert into NewSubmits (Contest, Team, Problem, SrcLang, Source, Computer, Arrived)
          values ($contestId, $teamId, $problemId, $srcLang, $source, inet_aton($remoteAddr), CURRENT_TIMESTAMP())
        """.andThen(sql"""select LAST_INSERT_ID()""".as[Long])

  def submitPost = AsyncStack(parse.multipartFormData, AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn

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
                Redirect(routes.Application.index)
              }
            }
          }
        )
    }
  }

  import play.api.Play.current
  import play.api.mvc._

  val statusActor = system.actorOf(StatusActor.props(db), "status-actor")

  val subscription = new Subscription {
    // A qos of 3 will cause up to 3 concurrent messages to be processed at any given time.
    def config = channel(qos = 1) {
      consume(queue("contester.finished")) {
        body(as[FinishedTesting]) { submit =>
          // do work; this body is executed in a separate thread, as provided by the implicit execution context
          Logger.info(s"Received $submit")
          ack()
        }
      }
    }
  }

  rabbitMq ! subscription


  def socket = WebSocket.tryAccept[JsValue] { implicit request =>
    authorized(anyUser).flatMap {
      case Left(result) => Future.successful(Left(result))
      case Right((user, resultUpdater)) => {
        val in = Iteratee.foreach[JsValue] {
          msg => Logger.debug(msg.toString())
        }.map { msg =>
          Logger.debug(s"Disconnected: $user ($msg)")
        }
        import scala.concurrent.duration._
        import akka.pattern.ask

        statusActor.ask(StatusActor.Join(user))(5 seconds).map {
          case StatusActor.Connected(out) =>
            Right((in, out))
          case _ => Left(BadRequest("foo"))
        }
      }
    }
 }

}