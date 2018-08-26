package controllers

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.actions.SecuredErrorHandler
import com.mohiva.play.silhouette.api.util.Credentials
import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.mvc._
import utils.auth.{AdminEnv, TeamsEnv}
import views.html

import scala.concurrent.{ExecutionContext, Future}

class CustomSecuredErrorHandler extends MySecuredErrorHandler {
  override val loginForm: () => Call = routes.AuthForms.login _
}

class AdminSecuredErrorHandler extends MySecuredErrorHandler {
  override val loginForm: () => Call = routes.AdminAuthForms.login _
}

trait MySecuredErrorHandler extends SecuredErrorHandler {
  def loginForm: () => Call

  import play.api.mvc.Results._
  override def onNotAuthenticated(implicit request: RequestHeader): Future[Result] =
    Future.successful(Redirect(loginForm()))

  override def onNotAuthorized(implicit request: RequestHeader): Future[Result] =
    Future.successful(Unauthorized)
}


case class AuthData(username: String, password: String)

object AuthData {
  val form = Form {
    mapping("username" -> text, "password" -> text)(AuthData.apply)(AuthData.unapply)
  }
}

// TODO: fix this copy&paste
class AuthForms (cc: ControllerComponents,
                           silhouette: Silhouette[TeamsEnv],
                           credentialsProvider: TeamsProvider)(implicit ec: ExecutionContext) extends AbstractController(cc) with I18nSupport {
  private def authenticatorService = silhouette.env.authenticatorService
  private def eventBus = silhouette.env.eventBus
  private def teamsService = silhouette.env.identityService

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
            case Some(vuser) => authenticatorService.create(loginInfo).flatMap {
              authenticator => {
                eventBus.publish(LoginEvent(vuser, request))
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
                 credentialsProvider: AdminsProvider)(implicit ec: ExecutionContext) extends AbstractController(cc) with I18nSupport {
  private def authenticatorService = silhouette.env.authenticatorService
  private def eventBus = silhouette.env.eventBus
  private def adminService = silhouette.env.identityService

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
            case Some(vuser) => authenticatorService.create(loginInfo).flatMap {
              authenticator => {
                eventBus.publish(LoginEvent(vuser, request))
                authenticatorService.init(authenticator).flatMap { v =>
                  authenticatorService.embed(v, Redirect(routes.AdminApplication.index))
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