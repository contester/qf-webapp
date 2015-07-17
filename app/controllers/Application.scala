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
import play.api.mvc.{Action, Controller}
import slick.driver.JdbcProfile
import slick.jdbc.{PositionedParameters, SetParameter, GetResult}
import spire.math.Rational
import views.html

import scala.concurrent.{Promise, Future}

case class SubmitData(problem: String, compiler: String)

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

class Application @Inject() (override val dbConfigProvider: DatabaseConfigProvider, monitorModel: Monitor,
                             system: ActorSystem, val messagesApi: MessagesApi) extends Controller with AuthElement with AuthConfigImpl with I18nSupport{

  val dbConfig = dbConfigProvider.get[JdbcProfile]

  import dbConfig.driver.api._
  val helloActor = system.actorOf(HelloActor.props, "hello-actor")

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
    db.db.run(Submits.getContestTeamSubmits(team.contest.id, team.team.localId))

  private def annot8(submits: Seq[Submit], schoolMode: Boolean) =
    if (schoolMode)
      Submits.annotateSchoolSubmits(db.db, submits)
    else
      Submits.annotateACMSubmits(db.db, submits)

  def index = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn
    getSubmits(loggedInTeam).flatMap(annot8(_, loggedInTeam.contest.schoolMode)).map { subs =>
      Ok(html.index(loggedInTeam, subs))
    }
  }

  private def getProblemsQuery(contest: Int) =
    sql"""select ID, Name from Problems where Contest = $contest order by ID""".as[(String, String)]

  private def getProblems(contest: Int) =
    db.db.run(getProblemsQuery(contest)).map(_.map { case (id, name) => id -> s"$id. $name"})

  private def getCompilersQuery(contest: Int) =
    sql"""select ID, Name from Languages where Contest = $contest order by ID""".as[(String, String)]

  private def getCompilers(contest: Int) =
    db.db.run(getCompilersQuery(contest))

  val submitForm = Form {
    mapping("problem" -> text, "compiler" -> text)(SubmitData.apply)(SubmitData.unapply)
  }


  def submit = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn

    getProblems(loggedInTeam.contest.id).flatMap { problems =>
      getCompilers(loggedInTeam.contest.id).map { compilers =>
        Ok(html.sendsolution(loggedInTeam, submitForm, problems, compilers))
      }
    }
  }

  implicit object SetByteArray extends SetParameter[Array[Byte]] {
    override def apply(v1: Array[Byte], v2: PositionedParameters): Unit = v2.setBytes(v1)
  }

  def submitInsertQuery(contestId: Int, teamId: Int, problemId: String, srcLang: Int, source: Array[Byte], remoteAddr: String) =
    sqlu"""insert into NewSubmits (Contest, Team, Problem, SrcLang, Source, Computer, Arrived)
          values ($contestId, $teamId, $problemId, $srcLang, $source, inet_aton($remoteAddr), CURRENT_TIMESTAMP())
        """

  implicit val getClarification = GetResult(r => Clarification(new DateTime(r.nextTimestamp()), r.nextString().toUpperCase, r.nextString()))

  implicit val getClarificationRequest = GetResult(
    r => ClarificationRequest(new DateTime(r.nextTimestamp()), r.nextString(), r.nextString(), r.nextString())
  )

  def getClarificationsQuery(contestId: Int) =
    sql"""select cl_date, cl_task, cl_text from clarifications where cl_is_hidden = '0' and cl_contest_idf = $contestId""".as[Clarification]

  def getClarifications(contestId: Int) =
    db.db.run(getClarificationsQuery(contestId))

  def getClarificationRequests(contestId: Int, teamId: Int) =
    db.db.run(
      sql"""select Arrived, Problem, Request, Answer from ClarificationRequests
           where Contest = $contestId and Team = $teamId""".as[ClarificationRequest]
    )


  val clarificationReqForm = Form {
    mapping("problem" -> text, "text" -> text)(ClarificationReqData.apply)(ClarificationReqData.unapply)
  }

  def clarifications = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
      val loggedInTeam = loggedIn
    getProblems(loggedInTeam.contest.id).flatMap { problems =>
    getClarifications(loggedInTeam.contest.id).flatMap { clarifications =>
      getClarificationRequests(loggedInTeam.contest.id, loggedInTeam.team.localId).map { clReq =>
        Ok(html.clarifications(loggedInTeam, clarifications, clReq, problems, clarificationReqForm))
      }
    }
  }
  }

  def clarificationPost = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn

    clarificationReqForm.bindFromRequest.fold(
      formWithErrors => {
        getProblems(loggedInTeam.contest.id).flatMap { problems =>
          getClarifications(loggedInTeam.contest.id).flatMap { clarifications =>
            getClarificationRequests(loggedInTeam.contest.id, loggedInTeam.team.localId).map { clReq =>
              BadRequest(html.clarifications(loggedInTeam, clarifications, clReq, problems, formWithErrors))
            }
          }
        }
      },
      clrData => {
        db.db.run(
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

    getProblems(loggedInTeam.contest.id).flatMap { problems =>
      getCompilers(loggedInTeam.contest.id).flatMap { compilers =>
        val parsed = submitForm.bindFromRequest
        val solutionOpt = request.body.file("file").map { solution =>
          FileUtils.readFileToByteArray(solution.ref.file)
        }

        val parsed0 = if (solutionOpt.isDefined) parsed
          else parsed.withGlobalError("can't open the file")

        parsed0.fold(
          formWithErrors => {
            Future.successful(BadRequest(html.sendsolution(loggedInTeam, formWithErrors, problems, compilers)))
          },
          submitData => {
            db.db.run(submitInsertQuery(loggedInTeam.contest.id, loggedInTeam.team.localId, submitData.problem, submitData.compiler.toInt, solutionOpt.get, request.remoteAddress)).map { _ =>
              Redirect(routes.Application.index)
            }
          }
        )
      }
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