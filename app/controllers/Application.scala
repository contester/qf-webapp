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
import views.html

import scala.concurrent.{Promise, Future}

class Application @Inject() (override val dbConfigProvider: DatabaseConfigProvider, lifecycle: ApplicationLifecycle) extends Controller with AuthElement with AuthConfigImpl {

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

  private def anyUser(account: LoggedInTeam): Future[Boolean] = Future.successful(true)

  def index = AsyncStack(AuthorityKey -> anyUser) { implicit request =>

    Future.successful(Ok(html.index(loggedIn)))
  }

}