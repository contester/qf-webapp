package controllers

import javax.inject.Inject

import jp.t2v.lab.play2.auth.LoginLogout
import models.{AdminAuthConfigImpl, AuthConfigImpl, Users}
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{Action, Controller}
import slick.driver.JdbcProfile
import views.html

import scala.concurrent.Future

case class AuthData(username: String, password: String)

object AuthData {
  val form = Form {
    mapping("username" -> text, "password" -> text)(AuthData.apply)(AuthData.unapply)
  }
}

class AuthForms @Inject() (val messagesApi: MessagesApi, val auth: AuthWrapper)
  extends Controller with LoginLogout with AuthConfigImpl with I18nSupport {

  def login = Action { implicit request =>
    Ok(html.login(AuthData.form))
  }

  def logout = Action.async { implicit request =>
    gotoLogoutSucceeded.map(_.flashing("success" -> "You've been logged out"))
  }

  def doAuth(username: String, password: String) =
    auth.resolve(username)
    //Users.authenticate(db.db, username, password)

  def authenticate = Action.async { implicit request =>
    AuthData.form.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(html.login(formWithErrors))),
      user => doAuth(user.username, user.password).flatMap {
        case Some(found) =>
          gotoLoginSucceeded(found.username)

        case None => Future.successful(BadRequest(html.login(AuthData.form.fill(AuthData(user.username, ""))
          .withGlobalError("Неверное имя пользователя или пароль"))))
      }
    )
  }
}

class AdminAuthForms @Inject() (val messagesApi: MessagesApi, val auth: AuthWrapper)
  extends Controller with LoginLogout with AdminAuthConfigImpl with I18nSupport {

  def login = Action { implicit request =>
    Ok(html.admin.login(AuthData.form))
  }

  def logout = Action.async { implicit request =>
    gotoLogoutSucceeded.map(_.flashing("success" -> "You've been logged out"))
  }

  def doAuth(username: String, password: String) =
    auth.authAdmin(username, password)

  def authenticate = Action.async { implicit request =>
    AuthData.form.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(html.login(formWithErrors))),
      user => doAuth(user.username, user.password).flatMap {
        case Some(found) =>
          gotoLoginSucceeded(found.toId)

        case None => Future.successful(BadRequest(html.admin.login(AuthData.form.fill(AuthData(user.username, ""))
          .withGlobalError("Неверное имя пользователя или пароль"))))
      }
    )
  }
}