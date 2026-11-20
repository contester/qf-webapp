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
import scala.util.Try

object I18nDefaults {
  val DefaultLang: play.api.i18n.Lang = play.api.i18n.Lang("ru")

  def resolveLangs(configured: play.api.i18n.Langs): play.api.i18n.Langs =
    if (configured.availables.nonEmpty) configured
    else new play.api.i18n.DefaultLangs(Seq(DefaultLang))
}

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

  /**
    * CSRF: a form POST must carry the token. The admin buttons and the message ack post through jQuery with an
    * `X-Requested-With: XMLHttpRequest` header instead (main.js); a cross-site page cannot send that header without a
    * CORS preflight, which this application never grants, so such a request is let through. Play's own
    * `play.filters.csrf.header.bypassHeaders` is empty by default, hence the rule is set here, not in the deployment.
    */
  override lazy val csrfConfig: play.filters.csrf.CSRFConfig = {
    val base = play.filters.csrf.CSRFConfig.fromConfiguration(configuration)
    base.copy(shouldProtect = rh =>
      base.shouldProtect(rh) && !rh.headers.get("X-Requested-With").exists(_.equalsIgnoreCase("XMLHttpRequest")))
  }

  /**
    * Supported languages: defaults to Russian (`ru`) when `play.i18n.langs` is not set, so every page renders from
    * `conf/messages.ru` regardless of the browser's Accept-Language header. A deployment can override this with
    * `play.i18n.langs` (e.g. `["en"]` for an English contest).
    */
  override lazy val langs: play.api.i18n.Langs = I18nDefaults.resolveLangs(
    new play.api.i18n.DefaultLangsProvider(configuration).get)
  lazy val defaultMessages: play.api.i18n.Messages = messagesApi.preferred(langs.availables)

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

  lazy val palevoSink: palevo.PalevoSink = palevo.PalevoSink.fromConfig(configuration, wsClient, actorSystem)(executionContext)

  // Minutes before the start and the end at which palevo is told "in N minutes"; edges of the status actor's round clock.
  lazy val countdownConfig: palevo.CountdownConfig = palevo.CountdownConfig(configuration)

  // The admin live monitor (the only live board) and the public board cache; both hear every change of a board
  // through the live hooks. Nothing writes static files.
  lazy val liveMonitor: ActorRef = actorSystem.actorOf(actors.LiveMonitorActor.props(dbConfig.db,
    () => models.Rounds.currentRounds(dbConfig.db)(executionContext), palevoSink,
    actors.LiveMonitorActor.DbStatusRows.fromDb(dbConfig.db, statusActorModel.teamClient, actorSystem.scheduler, defaultMessages)(executionContext)),
    "live-monitor-actor")
  lazy val publicBoardCache: PublicBoardCache = new PublicBoardCache
  lazy val liveHooks: live.LiveHooks = live.LiveHooks.all(new live.ActorLiveHooks(liveMonitor), publicBoardCache.hooks)
  lazy val liveMonitorRef: live.LiveMonitorRef = live.LiveMonitorRef(Some(liveMonitor))
  lazy val connectionStats: live.ConnectionStats = new live.ConnectionStats

  lazy val monitorModel = wire[Monitor]
  lazy val boardSource: models.BoardSource = monitorModel.source
  // Deferred: the live monitor itself needs statusActorModel's team client (status rows).
  // The status actor's round clock produces palevo's phase, countdown and reschedule events; the live monitor hears
  // its edges on the event stream for the standings.
  lazy val statusActorModel = new StatusActorModel(dbConfig, actorSystem, palevoSink, live.LiveWiring.deferred(liveHooks),
    countdownConfig, defaultMessages)

  // Database notifications (testing/eval results, round and team changes) for the actors; see NotifyListener.
  lazy val notifyListener: ActorRef = actorSystem.actorOf(NotifyListener.props(
    NotifyListener.ConnectionSettings.fromConfig(configuration), dbConfig.db,
    NotifyListener.Targets(statusActorModel.statusActor, statusActorModel.teamStateActor,
      statusActorModel.contestChanged, statusActorModel.contestRemoved, statusActorModel.teamsChanged,
      // After every (re)connect: notifications missed while disconnected must not leave caches stale.
      Some(NotifyListener.CatchUp(knownStatusRounds, () => {
        publicBoardCache.invalidateAll()
        liveMonitor ! actors.LiveMonitorActor.ReloadAll
      })))), "notify-listener")

  /** The rounds the status actor holds (for the listener's catch-up). */
  private def knownStatusRounds(): scala.concurrent.Future[Seq[Int]] = {
    implicit val timeout: akka.util.Timeout = akka.util.Timeout(scala.concurrent.duration.Duration(10, "seconds"))
    utils.Ask[actors.StatusActor.AllContests](statusActorModel.statusActor, actors.StatusActor.GetAllContests)
      .map(_.contests.map(_.id))
  }

  lazy val homeController = wire[_root_.controllers.Application]
  lazy val qAndAController = wire[_root_.controllers.QandA]
  lazy val sseController = wire[_root_.controllers.ServerSideEval]
  lazy val adminController = wire[_root_.controllers.AdminApplication]
  lazy val publicMonitorController = wire[_root_.controllers.PublicMonitor]

  lazy val authFormsController = wire[_root_.controllers.AuthForms]
  lazy val adminAuthFormsController = wire[_root_.controllers.AdminAuthForms]

  lazy val iPrefix: String = "/"

  lazy val router: Router = wire[_root_.router.Routes]

  // Start the live monitor with the application, not on the first event.
  liveMonitor
  // Listen from the start. The actor system stops with the application, and the listener's postStop closes its connection.
  notifyListener
}
