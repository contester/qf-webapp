package controllers

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.actions.SecuredErrorHandler
import com.mohiva.play.silhouette.api.util.Credentials
import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.mvc._
import play.twirl.api.HtmlFormat
import utils.auth.{AdminEnv, BaseEnv, TeamsEnv}
import views.html
import views.html.login

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

trait CommonAuthForms[E <: BaseEnv, P <: OneUserProvider] {
  this: AbstractController with I18nSupport =>

  def silhouette: Silhouette[E]
  def credentialsProvider: P

  private def authenticatorService = silhouette.env.authenticatorService
  private def eventBus = silhouette.env.eventBus
  private def teamsService = silhouette.env.identityService

  def loginForm(f: Form[AuthData])(implicit request: RequestHeader): HtmlFormat.Appendable
  def loginRoute: Call
  def indexRoute: Call
  implicit def ec: ExecutionContext

  def login = silhouette.UnsecuredAction { implicit request =>
    Ok(loginForm(AuthData.form))
  }

  def logout = silhouette.SecuredAction.async { implicit request =>
    eventBus.publish(LogoutEvent(request.identity, request))
    authenticatorService.discard(request.authenticator, Redirect(loginRoute))
  }

  def authenticate = silhouette.UnsecuredAction.async { implicit request =>
    AuthData.form.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(loginForm(formWithErrors))),
      user => credentialsProvider.authenticate(Credentials(user.username.toLowerCase, user.password)).flatMap {
        case Some(loginInfo) => teamsService.retrieve(loginInfo).flatMap {
          case Some(vuser) => authenticatorService.create(loginInfo).flatMap {
            authenticator => {
              eventBus.publish(LoginEvent(vuser, request))
              authenticatorService.init(authenticator).flatMap { v =>
                authenticatorService.embed(v, Redirect(indexRoute)).map(Some(_))
              }
            }
          }
          case None => Future.successful(None)
        }
        case None => Future.successful(None)
      }.map {
        case Some(v) => v
        case None => BadRequest(loginForm(AuthData.form.fill(AuthData(user.username, ""))
          .withGlobalError("Неверное имя пользователя или пароль")))
      }
    )
  }

}

// TODO: fix this copy&paste
class AuthForms (cc: ControllerComponents,
                 val silhouette: Silhouette[TeamsEnv],
                 val credentialsProvider: TeamsProvider)(implicit val ec: ExecutionContext) extends AbstractController(cc) with I18nSupport with CommonAuthForms[TeamsEnv, TeamsProvider] {
  override def loginForm(f: Form[AuthData])(implicit request: RequestHeader): HtmlFormat.Appendable = html.login(f)

  override def loginRoute: Call = routes.AuthForms.login

  override def indexRoute: Call = routes.Application.index
}

class AdminAuthForms (cc: ControllerComponents,
                      val silhouette: Silhouette[AdminEnv],
                      val credentialsProvider: AdminsProvider)(implicit val ec: ExecutionContext) extends AbstractController(cc) with I18nSupport with CommonAuthForms[AdminEnv, AdminsProvider] {


  override def loginForm(f: Form[AuthData])(implicit request: RequestHeader): HtmlFormat.Appendable = html.admin.login(f)

  override def loginRoute: Call = routes.AdminAuthForms.login

  override def indexRoute: Call = routes.AdminApplication.index
}