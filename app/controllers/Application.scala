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
import play.api.mvc.{Action, Controller}
import slick.driver.JdbcProfile
import slick.jdbc.GetResult
import spire.math.Rational
import views.html

import scala.concurrent.{Promise, Future}

case class SubmitData(problem: String, compiler: String)

class Application @Inject() (override val dbConfigProvider: DatabaseConfigProvider, lifecycle: ApplicationLifecycle, val messagesApi: MessagesApi) extends Controller with AuthElement with AuthConfigImpl with I18nSupport{

  val monitorModel = new Monitor(dbConfigProvider.get[JdbcProfile])
  monitorModel.rebuildMonitorsLoop

  lifecycle.addStopHook { () =>
    monitorModel.done = true
    Future.successful()
  }

  def monitor(id: Int) = Action.async { implicit request =>
    val np = Promise[(AnyStatus, AnyStatus)]
    monitorModel.contestMonitorsFu.putIfAbsent(id, np).getOrElse(np).future.map(x => Ok(html.monitor(x._2, 0)))
  }

  def monitorDefault = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn
    val np = Promise[(AnyStatus, AnyStatus)]
    monitorModel.contestMonitorsFu.putIfAbsent(loggedInTeam.contest.id, np).getOrElse(np).future.map(x => Ok(html.monitor(x._2, loggedInTeam.team.localId)))
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

}