import com.mohiva.play.silhouette.api.{Environment, EventBus, Silhouette, SilhouetteProvider}
import com.mohiva.play.silhouette.api.actions._
import com.mohiva.play.silhouette.api.crypto.{Base64AuthenticatorEncoder, CrypterAuthenticatorEncoder}
import com.mohiva.play.silhouette.api.services.{AuthenticatorService, IdentityService}
import com.mohiva.play.silhouette.api.util.Clock
import com.mohiva.play.silhouette.crypto.{JcaCrypter, JcaCrypterSettings}
import com.mohiva.play.silhouette.impl.authenticators.{SessionAuthenticator, SessionAuthenticatorService, SessionAuthenticatorSettings}
import com.mohiva.play.silhouette.impl.util.{DefaultFingerprintGenerator, SecureRandomIDGenerator}
import info.faljse.SDNotify.SDNotify
import models._
import play.api._
import play.api.db.slick.{DbName, SlickComponents}
import play.api.libs.ws.ahc.AhcWSComponents
import play.api.mvc.{BodyParsers, DefaultSessionCookieBaker}
import play.api.routing.Router
import play.filters.HttpFiltersComponents
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import utils.auth.{AdminEnv, BaseEnv, TeamsEnv}

import scala.concurrent.ExecutionContext
import scala.io.Source

class MyApplicationLoader extends ApplicationLoader {
  private var components: MyComponents = _

  def load(context: ApplicationLoader.Context): Application = {
    LoggerConfigurator(context.environment.classLoader).foreach {
      _.configure(context.environment, context.initialConfiguration, Map.empty)
    }
    components = new MyComponents(context)
    val result = components.application
    SDNotify.sendNotify
    result
  }
}

trait MakeEnv {
  import com.mohiva.play.silhouette.api.Environment

  def makeEnv[A <: BaseEnv](idService: IdentityService[A#I], clock: Clock, eventBus: EventBus, key: String = "authenticator")(implicit ec: ExecutionContext): Environment[A] =
    Environment[A](
    idService, {
      val config = SessionAuthenticatorSettings(key)

      val crypter = {
        val settings = JcaCrypterSettings("bar")
        new JcaCrypter(settings)
      }

      new SessionAuthenticatorService(config,
        new DefaultFingerprintGenerator(),
        new CrypterAuthenticatorEncoder(crypter),
        new DefaultSessionCookieBaker(),
        clock)
    }, List(), eventBus)
}

class MyComponents(context: ApplicationLoader.Context)
  extends BuiltInComponentsFromContext(context)
    with HttpFiltersComponents
    with SlickComponents
    with AhcWSComponents
    with MakeEnv
    with _root_.controllers.AssetsComponents {

  import com.softwaremill.macwire.wire

  lazy val clock = wire[Clock]
  lazy val authenticatorDecoder = wire[Base64AuthenticatorEncoder]
  lazy val idGenerator = new SecureRandomIDGenerator()
  lazy val eventBus = wire[EventBus]

  lazy val teamsEnv: Environment[TeamsEnv] = makeEnv[TeamsEnv](
    teamService, clock, eventBus)

  lazy val adminsEnv: Environment[AdminEnv] = makeEnv[AdminEnv](adminService, clock, eventBus, "adminkey")

  lazy val unSecuredErrorHandler: UnsecuredErrorHandler = wire[DefaultUnsecuredErrorHandler]

  lazy val unsecuredRequestHandler: UnsecuredRequestHandler = wire[DefaultUnsecuredRequestHandler]
  lazy val userAwareRequestHandler: UserAwareRequestHandler = wire[DefaultUserAwareRequestHandler]

  lazy val unsecuredAction: UnsecuredAction = wire[DefaultUnsecuredAction]
  lazy val userAwareAction: UserAwareAction = wire[DefaultUserAwareAction]

  lazy val bpDefault: BodyParsers.Default = wire[BodyParsers.Default]

  lazy val teamService: TeamsService = wire[TeamsServiceImpl]
  lazy val teamsProvider = wire[TeamsProvider]

  lazy val adminService = wire[AdminsServiceImpl]
  lazy val adminsProvider = wire[AdminsProvider]

  lazy val silhouetteTeamsEnv: Silhouette[TeamsEnv] = {
    lazy val securedErrorHandler: SecuredErrorHandler = new _root_.controllers.CustomSecuredErrorHandler
    lazy val securedRequestHandler: SecuredRequestHandler = wire[DefaultSecuredRequestHandler]
    lazy val securedAction: SecuredAction = wire[DefaultSecuredAction]

    wire[SilhouetteProvider[TeamsEnv]]
  }

  lazy val silhouetteAdminEnv: Silhouette[AdminEnv] = {
    lazy val securedErrorHandler: SecuredErrorHandler = new _root_.controllers.AdminSecuredErrorHandler
    lazy val securedRequestHandler: SecuredRequestHandler = wire[DefaultSecuredRequestHandler]
    lazy val securedAction: SecuredAction = wire[DefaultSecuredAction]

    wire[SilhouetteProvider[AdminEnv]]
  }

  lazy val dbConfig: DatabaseConfig[JdbcProfile] = slickApi.dbConfig[JdbcProfile](DbName("default"))

  val pwList = Source.fromFile(configuration.get[String]("passwordlist")).getLines().toSeq

  lazy val externalDatabases = ExternalDatabases(
    slickApi.dbConfig[JdbcProfile](DbName("studentWeb")),
    slickApi.dbConfig[JdbcProfile](DbName("schoolWeb")),
    slickApi.dbConfig[JdbcProfile](DbName("netmap")),
    pwList)

  lazy val monitorModel = wire[Monitor]
  lazy val rabbitModel = wire[RabbitMqModel]
  lazy val statusActorModel = wire[StatusActorModel]
  lazy val printingModel = wire[PrintingModel]

  val subscriptionsModel = wire[SubscriptionsModel]

  lazy val homeController = wire[_root_.controllers.Application]
  lazy val qAndAController = wire[_root_.controllers.QandA]
  lazy val sseController = wire[_root_.controllers.ServerSideEval]
  lazy val printingController = wire[_root_.controllers.Printing]
  lazy val adminController = wire[_root_.controllers.AdminApplication]

  lazy val authFormsController = wire[_root_.controllers.AuthForms]
  lazy val adminAuthFormsController = wire[_root_.controllers.AdminAuthForms]

  lazy val iPrefix: String = "/"

  lazy val router: Router = wire[_root_.router.Routes]
}
