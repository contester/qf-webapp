package controllers

import javax.inject.Inject

import jp.t2v.lab.play2.auth.{AuthElement, LoginLogout}
import models.{Users, AuthConfigImpl}
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

class AuthForms @Inject() (val messagesApi: MessagesApi, val dbConfigProvider: DatabaseConfigProvider, val auth: AuthWrapper)
  extends Controller with LoginLogout with AuthConfigImpl with I18nSupport {

  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import dbConfig.driver.api._

  private val loginForm = Form {
    mapping("username" -> text, "password" -> text)(AuthData.apply)(AuthData.unapply)
  }

  def login = Action { implicit request =>
    Ok(html.login(loginForm))
  }

  def logout = Action.async { implicit request =>
    gotoLogoutSucceeded.map(_.flashing("success" -> "You've been logged out"))
  }

  def doAuth(username: String, password: String) =
    //Users.authenticate(db.db, username, password)
    Users.resolve(db, username)

  def authenticate = Action.async { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(html.login(formWithErrors))),
      user => doAuth(user.username, user.password).flatMap {
        case Some(found) =>
          println(found)
          gotoLoginSucceeded(found.username)

        case None => Future.successful(BadRequest(html.login(loginForm.fill(AuthData(user.username, ""))
          .withGlobalError("Неверное имя пользователя или пароль"))))
      }
    )
  }
}