package models

import com.mohiva.play.silhouette.api.{Authorization, Identity, LoginInfo, Provider}
import com.mohiva.play.silhouette.api.services.IdentityService
import com.mohiva.play.silhouette.api.util.Credentials
import com.mohiva.play.silhouette.impl.authenticators.SessionAuthenticator
import com.mohiva.play.silhouette.impl.exceptions.InvalidPasswordException
import controllers.{PasswordHash, routes}
import play.api.mvc.Request
import slick.basic.DatabaseConfig
import slick.jdbc.{GetResult, JdbcBackend, JdbcProfile}
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/** What the session cookie carries: the username and a fingerprint of the stored hash (never the hash). */
case class AdminId(username: String, fingerprint: String) {
  override def toString = s"$username:$fingerprint"
}

trait WaiterPermissions {
  def canCreateTasks: Boolean
  def filter(room: String): Boolean
}

case class Admin(username: String, passwordHash: String, spectator: Set[Int], administrator: Set[Int],
                 locations: Set[String], unrestricted: Set[Int]) extends Identity with WaiterPermissions {
  override def toString = s"$username:$passwordHash"

  def toId = AdminId(username, PasswordHash.fingerprint(passwordHash))

  def canSpectate(contestId: Int) =
    spectator.contains(contestId) || spectator.contains(-1) || canModify(contestId)

  def canSeeAll(contestId: Int) =
    unrestricted.contains(contestId) || unrestricted.contains(-1) || canModify(contestId)

  def canModify(contestId: Int) =
    administrator.contains(contestId) || administrator.contains(-1)

  def isSuperAdmin =
    administrator.contains(-1)

  def canSeeTeamPasswords(contestId: Int) =
    canModify(contestId)

  lazy val defaultContest = {
    val t = spectator.union(administrator).min
    if (t == -1) 1 else t
  }

  val canCreateTasks =
    locations.contains("*")

  override def filter(room: String): Boolean =
    canCreateTasks || locations.contains(room)
}

object AdminId {
  def fromString(s: String): Option[AdminId] = {
    val i = s.lastIndexOf(':')
    if (i > 0 && i < s.length - 1) Some(AdminId(s.substring(0, i), s.substring(i + 1)))
    else None
  }
}

object Admin {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel._
  import org.stingray.contester.dbmodel.SlickModel._

  private[this] def parseSingleAcl(s: String): Option[Int] =
    if (s == "*")
      Some(-1)
    else Try(s.toInt).toOption

  private[this] def parseAcl(s: String): Set[Int] =
    s.split(',').flatMap(parseSingleAcl).toSet

  private[this] def parseStringAcl(s: String): Set[String] =
    s.split(',').toSet

  private[this] def buildAdmin(x: AdminEntry): Admin =
    Admin(x.username, x.password, parseAcl(x.spectator), parseAcl(x.administrator), parseStringAcl(x.locations),
      parseAcl(x.unrestricted))

  private[this] val byUsername = Compiled((username: Rep[String]) => admins.filter(_.username === username).take(1))

  def byName(db: JdbcBackend#DatabaseDef, username: String)(implicit ec: ExecutionContext): Future[Option[Admin]] =
    db.run(byUsername(username).result.headOption).map(_.map(buildAdmin))

  /** Rewrites a legacy SHA-1 value as bcrypt; false if the stored value changed in the meantime. */
  def upgradeHash(db: JdbcBackend#DatabaseDef, username: String, oldHash: String, newHash: String)(implicit ec: ExecutionContext): Future[Boolean] =
    db.run(admins.filter(a => a.username === username && a.password === oldHash).map(_.password).update(newHash)).map(_ == 1)
}

object AdminPermissions {
  case class withModify(contestId: Int) extends Authorization[Admin, SessionAuthenticator] {
    override def isAuthorized[B](identity: Admin, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] =
      Future.successful(identity.canModify(contestId))
  }

  case class withSpectate(contestId: Int) extends Authorization[Admin, SessionAuthenticator] {
    override def isAuthorized[B](identity: Admin, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] =
      Future.successful(identity.canSpectate(contestId))
  }

  case object withSuperAdmin extends Authorization[Admin, SessionAuthenticator] {
    override def isAuthorized[B](identity: Admin, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] =
      Future.successful(identity.isSuperAdmin)
  }

  case object withCreateTasks extends Authorization[Admin, SessionAuthenticator] {
    override def isAuthorized[B](identity: Admin, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] =
      Future.successful(identity.canCreateTasks)
  }
}



object AdminNavlinkMatch {
  def apply(tab: String): Function[Int, play.api.mvc.Call] =
    tab match {
      case "status" => routes.AdminApplication.submits(_, None, None, None)
      case "monitor" => routes.AdminApplication.monitor(_)
      case "qanda" => routes.AdminApplication.showQandA(_)
      case "rejudge" => routes.AdminApplication.rejudgePage(_)
      case "tasks" => routes.AdminApplication.tasks(_)
      case "print" => routes.AdminApplication.listPrintJobs(_)
      case "teams" => routes.AdminApplication.listTeams(_)
      case "contests" => routes.AdminApplication.showContestList(_)
      case _ => routes.AdminApplication.submits(_, None, None, None)
    }
}

trait AdminsService extends IdentityService[Admin]

class AdminsServiceImpl(dbConfig: DatabaseConfig[JdbcProfile])(implicit val ec: ExecutionContext) extends AdminsService {
  override def retrieve(loginInfo: LoginInfo): Future[Option[Admin]] =
    AdminId.fromString(loginInfo.providerKey) match {
      case Some(adminId) =>
        Admin.byName(dbConfig.db, adminId.username).map(_.filter(_.toId == adminId))
      case None => Future.successful(None)
    }
}

class AdminsProvider(dbConfig: DatabaseConfig[JdbcProfile]) extends OneUserProvider {
  override def id: String = "admin"
  def authenticate(credentials: Credentials)(implicit ec: ExecutionContext): Future[Option[LoginInfo]] = {
    Admin.byName(dbConfig.db, credentials.identifier).flatMap {
      case Some(a) if PasswordHash.verify(credentials.password, a.passwordHash) =>
        if (PasswordHash.isLegacy(a.passwordHash)) {
          val upgraded = PasswordHash.hash(credentials.password)
          Admin.upgradeHash(dbConfig.db, a.username, a.passwordHash, upgraded).map {
            case true => Some(LoginInfo(id, a.copy(passwordHash = upgraded).toId.toString))
            case false => None
          }
        } else Future.successful(Some(LoginInfo(id, a.toId.toString)))
      case Some(_) => Future.successful(None)
      case None =>
        PasswordHash.verifyDummy(credentials.password)
        Future.successful(None)
    }
  }
}