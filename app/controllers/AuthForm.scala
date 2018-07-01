package controllers

import com.mohiva.play.silhouette.api.{EventBus, LogoutEvent, Silhouette}
import com.mohiva.play.silhouette.api.actions.SecuredErrorHandler
import com.mohiva.play.silhouette.api.services.AuthenticatorService
import com.mohiva.play.silhouette.impl.authenticators.SessionAuthenticator
import javax.inject.Inject
import models._
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc._
import slick.driver.JdbcProfile
import utils.auth.TeamsEnv
import views.html

import scala.concurrent.Future

class CustomSecuredErrorHandler extends SecuredErrorHandler {
  import play.api.mvc.Results._
  override def onNotAuthenticated(implicit request: RequestHeader): Future[Result] =
    Future.successful(Redirect(routes.AuthForms.login()))

  override def onNotAuthorized(implicit request: RequestHeader): Future[Result] =
    Future.successful(Unauthorized)
}

case class AuthData(username: String, password: String)

object AuthData {
  val form = Form {
    mapping("username" -> text, "password" -> text)(AuthData.apply)(AuthData.unapply)
  }
}

class AuthForms @Inject() (cc: ControllerComponents,
                           silhouette: Silhouette[TeamsEnv],
                           override val messagesApi: MessagesApi,
                           configuration: Configuration,
                           eventBus: EventBus,
                           teamsService: TeamsService,
                           credentialsProvider: TeamsProvider,
                           authenticatorService: AuthenticatorService[SessionAuthenticator]) extends AbstractController(cc) with I18nSupport {

  def login = Action { implicit request =>
    Ok(html.login(AuthData.form))
  }

  def logout = silhouette.SecuredAction.async { implicit request =>
    eventBus.publish(LogoutEvent(request.identity, request))
    authenticatorService.discard(request.authenticator, Redirect(routes.LoginController.index))
  }

      def doAuth(username: String, password: String) =
    auth.authenticateUser(username, password)

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