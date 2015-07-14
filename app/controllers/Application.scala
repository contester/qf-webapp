package controllers

import javax.inject.Inject

import jp.t2v.lab.play2.auth.{AuthenticationElement, AuthElement, LoginLogout}
import models._
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.inject.ApplicationLifecycle
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{Action, Controller}
import slick.driver.JdbcProfile
import slick.jdbc.GetResult
import views.html

import scala.concurrent.{Promise, Future}

case class Submit0(arrived: Int, problem: String, finished: Boolean, compiled: Boolean, passed: Int, taken: Int) {
  def resultStr =
    if (!finished) "..."
    else if (!compiled) "Не скомпилировалось"
    else if (passed == taken) "Полное решение"
    else s"Неполное решение: $passed из $taken"
}

class Application @Inject() (override val dbConfigProvider: DatabaseConfigProvider, lifecycle: ApplicationLifecycle) extends Controller with AuthElement with AuthConfigImpl {

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
    monitorModel.contestMonitorsFu.putIfAbsent(loggedInTeam.contest.id, np).getOrElse(np).future.map(x => Ok(html.monitor(x._2, loggedInTeam.team.id)))
  }

  private def anyUser(account: LoggedInTeam): Future[Boolean] = Future.successful(true)

  import slick.driver.MySQLDriver.api._

  implicit val toSubmit0=GetResult(r => Submit0(r.nextInt(), r.nextString(), r.nextBoolean(), r.nextBoolean(), r.nextInt(), r.nextInt()))

  private def getSubmitsQuery(contest: Int, team: Int) =
    sql"""select UNIX_TIMESTAMP(Submits.Arrived) - UNIX_TIMESTAMP(Contests.Start) as arrived0,
          Submits.Task,
           Submits.Finished, Submits.Compiled, Submits.Passed, Submits.Taken from Submits, Contests
         where Submits.Team=$team and Submits.Contest=$contest and Submits.Contest=Contests.ID order by arrived0
       """.as[Submit0]

  private def getSubmits(team: LoggedInTeam) =
    db.db.run(getSubmitsQuery(team.contest.id, team.team.id))

  def index = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn
    getSubmits(loggedInTeam).map { subs =>
      Ok(html.index(loggedInTeam, subs))
    }
  }

}