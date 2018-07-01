import com.mohiva.play.silhouette.api.actions._
import com.mohiva.play.silhouette.api.crypto.{Base64AuthenticatorEncoder, CrypterAuthenticatorEncoder}
import com.mohiva.play.silhouette.api.services.AuthenticatorService
import com.mohiva.play.silhouette.api.util.Clock
import com.mohiva.play.silhouette.api.{Environment, EventBus, Silhouette, SilhouetteProvider}
import com.mohiva.play.silhouette.crypto.{JcaCrypter, JcaCrypterSettings}
import com.mohiva.play.silhouette.impl.authenticators.{SessionAuthenticator, SessionAuthenticatorService, SessionAuthenticatorSettings}
import com.mohiva.play.silhouette.impl.util.{DefaultFingerprintGenerator, SecureRandomIDGenerator}
import models.{TeamsProvider, TeamsService, TeamsServiceImpl}
import play.api._
import play.api.db.slick.{DbName, SlickComponents}
import play.api.mvc.{BodyParsers, DefaultSessionCookieBaker}
import play.api.routing.Router
import play.filters.HttpFiltersComponents
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import utils.auth.{DefaultEnv, TeamsEnv}

class MyApplicationLoader extends ApplicationLoader {
  private var components: MyComponents = _

  def load(context: ApplicationLoader.Context): Application = {
    components = new MyComponents(context)
    components.application
  }
}

class MyComponents(context: ApplicationLoader.Context)
  extends BuiltInComponentsFromContext(context)
    with HttpFiltersComponents
    with SlickComponents
    with _root_.controllers.AssetsComponents {

  import com.softwaremill.macwire.wire

  lazy val clock = wire[Clock]
  lazy val authenticatorDecoder = wire[Base64AuthenticatorEncoder]
  lazy val idGenerator = new SecureRandomIDGenerator()
  lazy val eventBus = wire[EventBus]

  private lazy val authenticatorService: AuthenticatorService[SessionAuthenticator] = {
    val config = SessionAuthenticatorSettings()

    val crypter = {
      val settings = new JcaCrypterSettings("foo")
      new JcaCrypter(settings)
    }

    new SessionAuthenticatorService(config,
      new DefaultFingerprintGenerator(),
      new CrypterAuthenticatorEncoder(crypter),
      new DefaultSessionCookieBaker(),
      Clock())
  }


  lazy val teamsEnv: Environment[TeamsEnv] = Environment[TeamsEnv](
    teamService, authenticatorService, List(), eventBus)

  lazy val securedErrorHandler: SecuredErrorHandler = new _root_.controllers.CustomSecuredErrorHandler
  lazy val unSecuredErrorHandler: UnsecuredErrorHandler = wire[DefaultUnsecuredErrorHandler]

  lazy val securedRequestHandler: SecuredRequestHandler = wire[DefaultSecuredRequestHandler]
  lazy val unsecuredRequestHandler: UnsecuredRequestHandler = wire[DefaultUnsecuredRequestHandler]
  lazy val userAwareRequestHandler: UserAwareRequestHandler = wire[DefaultUserAwareRequestHandler]

  lazy val securedAction: SecuredAction = wire[DefaultSecuredAction]
  lazy val unsecuredAction: UnsecuredAction = wire[DefaultUnsecuredAction]
  lazy val userAwareAction: UserAwareAction = wire[DefaultUserAwareAction]

  lazy val bpDefault: BodyParsers.Default = wire[BodyParsers.Default]

  lazy val teamService: TeamsService = wire[TeamsServiceImpl]

  lazy val teamsProvider = wire[TeamsProvider]

  lazy val silhouetteTeamsEnv: Silhouette[TeamsEnv] = wire[SilhouetteProvider[TeamsEnv]]

  lazy val dbConfig: DatabaseConfig[JdbcProfile] = slickApi.dbConfig[JdbcProfile](DbName("default"))

  lazy val homeController = wire[_root_.controllers.AsyncController]

  lazy val loginController = wire[_root_.controllers.LoginController]

  lazy val iPrefix: String = "/"

  lazy val router: Router = wire[_root_.router.Routes]
}
