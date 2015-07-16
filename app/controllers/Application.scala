package controllers

import javax.inject.Inject

import jp.t2v.lab.play2.auth.{AuthenticationElement, AuthElement, LoginLogout}
import models._
import org.apache.commons.io.FileUtils
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
import slick.jdbc.GetResult
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
      chatChannel.push("blah")
    }
  }
}

class Application @Inject() (override val dbConfigProvider: DatabaseConfigProvider, system: ActorSystem, lifecycle: ApplicationLifecycle, val messagesApi: MessagesApi) extends Controller with AuthElement with AuthConfigImpl with I18nSupport{

  val helloActor = system.actorOf(HelloActor.props, "hello-actor")

  val monitorModel = new Monitor(dbConfigProvider.get[JdbcProfile])
  monitorModel.rebuildMonitorsLoop

  lifecycle.addStopHook { () =>
    monitorModel.done = true
    Future.successful()
  }

  def monitor(id: Int) = Action.async { implicit request =>
    val np = Promise[(AnyStatus, AnyStatus)]
    monitorModel.contestMonitorsFu.putIfAbsent(id, np).getOrElse(np).future.map(x => Ok(html.monitor(x._2)))
  }

  def monitorDefault = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn
    val np = Promise[(AnyStatus, AnyStatus)]
    monitorModel.contestMonitorsFu.putIfAbsent(loggedInTeam.contest.id, np).getOrElse(np).future.map(x => {
      val v = if (loggedInTeam.contest.frozen) x._1 else x._2
      Ok(html.loggedinmonitor(v, loggedInTeam))})
  }

  private def anyUser(account: LoggedInTeam): Future[Boolean] = Future.successful(true)

  import slick.driver.MySQLDriver.api._

  private def getSubmits(team: LoggedInTeam) =
    db.db.run(Submits.getContestTeamSubmits(team.contest.id, team.team.localId))

  private def indexSubmits(submits: Seq[Submit]) =
    Submits.indexSubmits[Submit, SchoolCell](submits, SchoolCell.empty).sortBy(_.submit.arrivedSeconds).reverse

  def index = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn
    getSubmits(loggedInTeam).map { subs =>
      Ok(html.index(loggedInTeam, indexSubmits(subs)))
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
            println(loggedInTeam.contest, loggedInTeam.team, submitData.problem, submitData.compiler, solutionOpt.get.length)
              Future.successful(Redirect(routes.Application.index))
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