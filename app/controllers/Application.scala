package controllers

import javax.inject.Inject

import jp.t2v.lab.play2.auth.{AuthenticationElement, AuthElement, LoginLogout}
import models._
import org.apache.commons.io.FileUtils
import org.joda.time.DateTime
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.inject.ApplicationLifecycle
import play.api.libs.Files.TemporaryFile
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.iteratee.{Enumerator, Concurrent}
import play.api.libs.json.JsValue
import play.api.mvc.{RequestHeader, Action, Controller}
import slick.driver.JdbcProfile
import slick.jdbc.{PositionedParameters, SetParameter, GetResult}
import spire.math.Rational
import views.html

import scala.concurrent.{Promise, Future}

case class SubmitData(problem: String, compiler: Int)

import akka.actor._

object HelloActor {
  def props = Props[HelloActor]

  case class SayHello(name: String)
}

case class Join(username: String)
case class Connected(enumerator:Enumerator[String])

class HelloActor extends Actor {
  import HelloActor._
  import scala.concurrent.duration._

  import context.dispatcher
  val tick =
    context.system.scheduler.schedule(60 seconds, 60 seconds, self, "tick")

  val (chatEnumerator, chatChannel) = Concurrent.broadcast[String]

  def receive = {
    case Join(username) => {
      sender ! Connected(chatEnumerator)
    }
    case "tick" => {
      println("tick")
      //chatChannel.push("blah")
    }
  }
}

case class Clarification(time: DateTime, problem: String, text: String)
case class ClarificationRequest(time: DateTime, problem: String, text: String, answer: String)

case class ClarificationReqData(problem: String, text: String)

class Application @Inject() (dbConfigProvider: DatabaseConfigProvider,
                             monitorModel: Monitor,
                             system: ActorSystem,
                             val auth: AuthWrapper,
                             val messagesApi: MessagesApi) extends Controller with AuthElement with AuthConfigImpl with I18nSupport{

  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import utils.Db._
  import dbConfig.driver.api._

  private val helloActor = system.actorOf(HelloActor.props, "hello-actor")

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

  implicit val getClarification = GetResult(r => Clarification(new DateTime(r.nextTimestamp()), r.nextString().toUpperCase, r.nextString()))

  implicit val getClarificationRequest = GetResult(
    r => ClarificationRequest(new DateTime(r.nextTimestamp()), r.nextString(), r.nextString(), r.nextString())
  )


  def getClarificationsQuery(contestId: Int) =
    sql"""select cl_date, cl_task, cl_text from clarifications where cl_is_hidden = '0' and cl_contest_idf = $contestId""".as[Clarification]

  def getClarifications(contestId: Int) =
    db.run(getClarificationsQuery(contestId))

  def getClarificationRequests(contestId: Int, teamId: Int) =
    db.run(
      sql"""select Arrived, Problem, Request, Answer from ClarificationRequests
           where Contest = $contestId and Team = $teamId""".as[ClarificationRequest]
    )


  val clarificationReqForm = Form {
    mapping("problem" -> text, "text" -> text)(ClarificationReqData.apply)(ClarificationReqData.unapply)
  }

  private def clrForm(loggedInTeam: LoggedInTeam, form: Form[ClarificationReqData])(implicit request: RequestHeader) =
    getProblems(loggedInTeam.contest.id).flatMap { probs =>
      getClarifications(loggedInTeam.contest.id).flatMap { clars =>
        getClarificationRequests(loggedInTeam.contest.id, loggedInTeam.team.localId).map { clReq =>
          html.clarifications(loggedInTeam, clars, clReq, probs, form)
        }
      }
    }

  def clarifications = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    clrForm(loggedIn, clarificationReqForm).map(Ok(_))
  }

  def clarificationPost = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn

    clarificationReqForm.bindFromRequest.fold(
      formWithErrors => clrForm(loggedIn, formWithErrors).map(BadRequest(_)),
      clrData => {
        db.run(
          sqlu"""insert into ClarificationRequests (Contest, Team, Problem, Request, Arrived) values
                (${loggedInTeam.contest.id}, ${loggedInTeam.team.localId}, ${clrData.problem}, ${clrData.text},
                CURRENT_TIMESTAMP())
              """
        ).map { _ =>
          Redirect(routes.Application.clarifications)
        }
      }
    )
  }


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
            Future.successful(BadRequest(html.sendsolution(loggedInTeam, formWithErrors, problems, compilersForForm(compilers))))
          },
          submitData => {
            db.run(submitInsertQuery(loggedInTeam.contest.id, loggedInTeam.team.localId, submitData.problem, submitData.compiler, solutionOpt.get, request.remoteAddress)).map { wat =>
              println(wat.headOption)
              Redirect(routes.Application.index)
            }
          }
        )
    }
  }

  import play.api.mvc._
  import play.api.libs.iteratee._

  def socket = WebSocket.tryAccept[String] { implicit request =>
    println(request)

    authorized(anyUser).flatMap {
      case Left(result) => Future.successful(Left(result))
      case Right((user, resultUpdater)) => {
        println(user)
        val in = Iteratee.foreach[String] {
          msg => println(msg)
        }
        import scala.concurrent.duration._
        import akka.pattern.ask

        (helloActor.ask(Join("foo"))(5 seconds)).map {
          case Connected(out) =>
            Right((in, out))
          case _ => Left(BadRequest("foo"))
        }
      }
    }
 }

}