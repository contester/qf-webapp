package controllers

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.actions.SecuredErrorHandler
import com.mohiva.play.silhouette.api.services.AuthenticatorService
import com.mohiva.play.silhouette.api.util.Credentials
import com.mohiva.play.silhouette.impl.authenticators.SessionAuthenticator
import models._
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc._
import utils.auth.{AdminEnv, TeamsEnv}
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

class AuthForms (cc: ControllerComponents,
                           silhouette: Silhouette[TeamsEnv],
                           configuration: Configuration,
                           eventBus: EventBus,
                           teamsService: TeamsService,
                           credentialsProvider: TeamsProvider,
                           authenticatorService: AuthenticatorService[SessionAuthenticator]) extends AbstractController(cc) with I18nSupport {

  def login = silhouette.UnsecuredAction { implicit request =>
    Ok(html.login(AuthData.form))
  }

  def logout = silhouette.SecuredAction.async { implicit request =>
    eventBus.publish(LogoutEvent(request.identity, request))
    authenticatorService.discard(request.authenticator, Redirect(routes.AuthForms.login))
  }

  def authenticate = silhouette.UnsecuredAction.async { implicit request =>
    AuthData.form.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(html.login(formWithErrors))),
      user => credentialsProvider.authenticate(Credentials(user.username, user.password)).flatMap {
        loginInfo: LoginInfo =>
          teamsService.retrieve(loginInfo).flatMap {
            case Some(user) => authenticatorService.create(loginInfo).flatMap {
              authenticator => {
                eventBus.publish(LoginEvent(user, request))
                authenticatorService.init(authenticator).flatMap { v =>
                  authenticatorService.embed(v, Redirect(routes.Application.index))
                }
              }
            }
            case None => Future.successful(BadRequest(html.login(AuthData.form.fill(AuthData(user.username, ""))
              .withGlobalError("Неверное имя пользователя или пароль"))))
          }
      }
    )
  }
}

class AdminAuthForms (cc: ControllerComponents,
                 silhouette: Silhouette[AdminEnv],
                 override val messagesApi: MessagesApi,
                 configuration: Configuration,
                 eventBus: EventBus,
                      adminService: AdminsService,
                 credentialsProvider: AdminsProvider,
                 authenticatorService: AuthenticatorService[SessionAuthenticator]) extends AbstractController(cc) with I18nSupport {

  def login = silhouette.UnsecuredAction { implicit request =>
    Ok(html.admin.login(AuthData.form))
  }

  def logout = silhouette.SecuredAction.async { implicit request =>
    eventBus.publish(LogoutEvent(request.identity, request))
    authenticatorService.discard(request.authenticator, Redirect(routes.AdminAuthForms.login))
  }

  def authenticate = silhouette.UnsecuredAction.async { implicit request =>
    AuthData.form.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(html.login(formWithErrors))),
      user => credentialsProvider.authenticate(Credentials(user.username, user.password)).flatMap {
        loginInfo: LoginInfo =>
          adminService.retrieve(loginInfo).flatMap {
            case Some(user) => authenticatorService.create(loginInfo).flatMap {
              authenticator => {
                eventBus.publish(LoginEvent(user, request))
                authenticatorService.init(authenticator).flatMap { v =>
                  authenticatorService.embed(v, Redirect(routes.Application.index))
                }
              }
            }
            case None => Future.successful(BadRequest(html.login(AuthData.form.fill(AuthData(user.username, ""))
              .withGlobalError("Неверное имя пользователя или пароль"))))
          }
      }
    )
  }
}