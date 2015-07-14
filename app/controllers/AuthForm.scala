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
import views.html

import scala.concurrent.Future

case class AuthData(username: String, password: String)

class AuthForms @Inject() (val messagesApi: MessagesApi, override val dbConfigProvider: DatabaseConfigProvider) extends Controller with LoginLogout with AuthConfigImpl with I18nSupport {
  /** Your application's login form.  Alter it to fit your application */
  val loginForm = Form {
    mapping("username" -> text, "password" -> text)(AuthData.apply)(AuthData.unapply)
  }

  /** Alter the login page action to suit your application. */
  def login = Action { implicit request =>
    Ok(html.login(loginForm))
  }

  /**
   * Return the `gotoLogoutSucceeded` method's result in the logout action.
   *
   * Since the `gotoLogoutSucceeded` returns `Future[Result]`,
   * you can add a procedure like the following.
   *
   *   gotoLogoutSucceeded.map(_.flashing(
   *     "success" -> "You've been logged out"
   *   ))
   */
  def logout = Action.async { implicit request =>
    // do something...
    gotoLogoutSucceeded
  }

  def doAuth(username: String, password: String) =
    //Users.authenticate(db.db, username, password)
    Users.resolve(db.db, username)

  /**
   * Return the `gotoLoginSucceeded` method's result in the login action.
   *
   * Since the `gotoLoginSucceeded` returns `Future[Result]`,
   * you can add a procedure like the `gotoLogoutSucceeded`.
   */
  def authenticate = Action.async { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(html.login(formWithErrors))),
      user => doAuth(user.username, user.password).flatMap {
        case Some(found) =>
          gotoLoginSucceeded(found.username)

        case None => Future.successful(BadRequest(html.login(loginForm.fill(AuthData(user.username, ""))
          .withGlobalError("Неверное имя пользователя или пароль"))))
      }
    )
  }
}