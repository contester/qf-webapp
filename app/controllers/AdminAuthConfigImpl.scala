package models

import controllers.{AuthWrapper, routes}
import jp.t2v.lab.play2.auth._
import play.api.db.slick.DatabaseConfigProvider
import play.api.mvc.{Result, RequestHeader}
import play.api.mvc.Results.{Redirect, Forbidden}
import slick.driver.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.{ClassTag, classTag}

trait AdminAuthConfigImpl extends AuthConfig {
  def auth: AuthWrapper

  /**
   * A type that is used to identify a user.
   * `String`, `Int`, `Long` and so on.
   */
  type Id = Admin

  /**
   * A type that represents a user in your application.
   * `User`, `Account` and so on.
   */
  type User = Admin

  /**
   * A type that is defined by every action for authorization.
   * This sample uses the following trait:
   *
   * sealed trait Role
   * case object Administrator extends Role
   * case object NormalUser extends Role
   */
  type Authority = User => Future[Boolean]

  /**
   * A `ClassTag` is used to retrieve an id from the Cache API.
   * Use something like this:
   */
  val idTag: ClassTag[Id] = classTag[Id]

  /**
   * The session timeout in seconds
   */
  val sessionTimeoutInSeconds: Int = 3600

  /**
   * A function that returns a `User` object from an `Id`.
   * You can alter the procedure to suit your application.
   */
  def resolveUser(id: Id)(implicit ctx: ExecutionContext): Future[Option[User]] =
    auth.resolveAdmin(id)

  /**
   * Where to redirect the user after a successful login.
   */
  def loginSucceeded(request: RequestHeader)(implicit ctx: ExecutionContext): Future[Result] =
    Future.successful(Redirect(routes.AdminApplication.index))

  /**
   * Where to redirect the user after logging out
   */
  def logoutSucceeded(request: RequestHeader)(implicit ctx: ExecutionContext): Future[Result] =
    Future.successful(Redirect(routes.AdminApplication.index))

  /**
   * If the user is not logged in and tries to access a protected resource then redirct them as follows:
   */
  def authenticationFailed(request: RequestHeader)(implicit ctx: ExecutionContext): Future[Result] =
    Future.successful(Redirect(routes.AdminAuthForms.login))

  /**
   * If authorization failed (usually incorrect password) redirect the user as follows:
   */
  override def authorizationFailed(request: RequestHeader, user: User,
                                   authority: Option[Authority])(implicit context: ExecutionContext): Future[Result] = {
    Future.successful(Forbidden("no permission"))
  }

  /**
   * This method is kept for compatibility.
   * It will be removed in a future version
   * Override `authorizationFailed(RequestHeader, User, Option[Authority])` instead of this
   */
  def authorizationFailed(request: RequestHeader)(implicit ctx: ExecutionContext): Future[Result] = throw new AssertionError

  /**
   * A function that determines what `Authority` a user has.
   * You should alter this procedure to suit your application.
   */
  def authorize(user: User, authority: Authority)(implicit ctx: ExecutionContext): Future[Boolean] = authority(user)

  override lazy val tokenAccessor: TokenAccessor = new CookieTokenAccessor(
    cookieName = "QFADMIN_SESS_ID",
    cookieSecureOption = false,
    cookieHttpOnlyOption = true,
    cookieDomainOption = None,
    cookiePathOption = "/",
    cookieMaxAge = Some(sessionTimeoutInSeconds)
  )

  implicit val adminToString = ToString((x: Admin) => x.toString)
  implicit val adminFromString = new FromString[Admin] {
    override def apply(id: String): Option[Admin] = Admin.fromString(id)
  }

  override lazy val idContainer: AsyncIdContainer[Id] = AsyncIdContainer(new CookieIdContainer[Id])
}