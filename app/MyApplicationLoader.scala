import com.mohiva.play.silhouette.api.{Environment, EventBus, Silhouette, SilhouetteProvider}
import com.mohiva.play.silhouette.api.actions._
import com.mohiva.play.silhouette.api.crypto.{Base64AuthenticatorEncoder, CrypterAuthenticatorEncoder}
import com.mohiva.play.silhouette.api.services.{AuthenticatorService, IdentityService}
import com.mohiva.play.silhouette.api.util.Clock
import com.mohiva.play.silhouette.crypto.{JcaCrypter, JcaCrypterSettings}
import com.mohiva.play.silhouette.impl.authenticators.{SessionAuthenticator, SessionAuthenticatorService, SessionAuthenticatorSettings}
import com.mohiva.play.silhouette.impl.util.{DefaultFingerprintGenerator, SecureRandomIDGenerator}
import akka.actor.ActorRef
import info.faljse.SDNotify.SDNotify
import java.io.File
import models._
import play.api._
import play.api.i18n.{Lang, MessagesImpl}
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
import scala.util.Try

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

  val pwList = Try(Source.fromFile(configuration.get[String]("passwordlist")).getLines().toSeq).toOption.toSeq.flatten

  lazy val externalDatabases = ExternalDatabases(
    slickApi.dbConfig[JdbcProfile](DbName("studentWeb")),
    slickApi.dbConfig[JdbcProfile](DbName("schoolWeb")),
    slickApi.dbConfig[JdbcProfile](DbName("netmap")),
    pwList)

  lazy val palevoSink: palevo.PalevoSink = palevo.PalevoSink.fromConfig(configuration, wsClient, actorSystem)(executionContext)

  // Read-only query API for palevo's chat bot.
  lazy val queryConfig: palevo.query.QueryConfig = palevo.query.QueryConfig(configuration)
  lazy val querySource: palevo.query.QuerySource = new palevo.query.DbQuerySource(dbConfig.db, liveMonitor,
    id => statusActorModel.problemClient.getProblems(id)(akka.util.Timeout(5, java.util.concurrent.TimeUnit.SECONDS)),
    () => printingModel.currentPrinterProblems)(executionContext)

  // Live monitor. When monitor.disable_new_monitor = true nothing below is constructed beyond the config and the
  // no-op hooks, and Monitor starts the old MonitorActor as before.
  lazy val liveConfig: live.LiveMonitorConfig = live.LiveMonitorConfig(configuration)

  private lazy val liveBuilt: live.LiveWiring.Built = live.LiveWiring.build(liveConfig, () => {
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    val messages = MessagesImpl(Lang("en"), messagesApi)
    actorSystem.actorOf(actors.LiveMonitorActor.props(
      dbConfig.db, liveConfig, configuration.getOptional[String]("monitor.static_location").map(new File(_)),
      statusActorModel.teamClient, statusActorModel.problemClient,
      () => dbConfig.db.run(org.stingray.contester.dbmodel.SlickModel.allContests.result),
      (c, b) => live.StaticRender.render(c, b)(messages), palevoSink), "live-monitor-actor")
  })

  lazy val liveHooks: live.LiveHooks = liveBuilt.hooks
  lazy val liveMonitor: Option[ActorRef] = liveBuilt.monitor
  lazy val liveMonitorRef: live.LiveMonitorRef = live.LiveMonitorRef(liveMonitor)
  lazy val liveContestLookup: live.LiveContestLookup =
    live.LiveContestLookup(id => statusActorModel.getContest(id)(akka.util.Timeout(5, java.util.concurrent.TimeUnit.SECONDS), executionContext))

  lazy val monitorModel = wire[Monitor]
  lazy val rabbitModel = wire[RabbitMqModel]
  // Deferred: the live monitor itself needs statusActorModel's team and problem clients.
  lazy val statusActorModel = new StatusActorModel(dbConfig, actorSystem, palevoSink, live.LiveWiring.deferred(liveHooks))
  lazy val printingModel = wire[PrintingModel]

  val subscriptionsModel = wire[SubscriptionsModel]

  lazy val homeController = wire[_root_.controllers.Application]
  lazy val qAndAController = wire[_root_.controllers.QandA]
  lazy val sseController = wire[_root_.controllers.ServerSideEval]
  lazy val printingController = wire[_root_.controllers.Printing]
  lazy val adminController = wire[_root_.controllers.AdminApplication]
  lazy val publicMonitorController = wire[_root_.controllers.PublicMonitor]
  lazy val palevoQueryController = wire[_root_.controllers.PalevoQuery]

  lazy val authFormsController = wire[_root_.controllers.AuthForms]
  lazy val adminAuthFormsController = wire[_root_.controllers.AdminAuthForms]

  lazy val iPrefix: String = "/"

  lazy val router: Router = wire[_root_.router.Routes]

  // Start the live monitor (when enabled) with the application, not on the first event.
  liveMonitor
}
