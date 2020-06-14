package models

import com.mohiva.play.silhouette.api.{Authorization, Identity, LoginInfo, Provider}
import com.mohiva.play.silhouette.api.services.IdentityService
import com.mohiva.play.silhouette.api.util.Credentials
import com.mohiva.play.silhouette.impl.authenticators.SessionAuthenticator
import com.mohiva.play.silhouette.impl.exceptions.InvalidPasswordException
import controllers.{Hasher, routes}
import play.api.mvc.Request
import slick.basic.DatabaseConfig
import slick.jdbc.{GetResult, JdbcBackend, JdbcProfile}
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class AdminId(username: String, passwordHash: String) {
  override def toString = s"$username:$passwordHash"
}

trait WaiterPermissions {
  def canCreateTasks: Boolean
  def filter(room: String): Boolean
}

case class Admin(username: String, passwordHash: String, spectator: Set[Int], administrator: Set[Int],
                 locations: Set[String], unrestricted: Set[Int]) extends Identity with WaiterPermissions {
  override def toString = s"$username:$passwordHash"

  def toId = AdminId(username, passwordHash)

  def canSpectate(contestId: Int) =
    spectator.contains(contestId) || spectator.contains(-1) || canModify(contestId)

  def canSeeAll(contestId: Int) =
    unrestricted.contains(contestId) || unrestricted.contains(-1) || canModify(contestId)

  def canModify(contestId: Int) =
    administrator.contains(contestId) || administrator.contains(-1)

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
  def fromString(s: String) = {
    val splits = s.split(':')
    if (splits.length == 2)
      Some(AdminId(splits(0), splits(1)))
    else
      None
  }
}

object Admin {
  import utils.MyPostgresProfile.api._

  private[this] def parseSingleAcl(s: String): Option[Int] =
    if (s == "*")
      Some(-1)
    else Try(s.toInt).toOption

  private[this] def parseAcl(s: String): Set[Int] =
    s.split(',').flatMap(parseSingleAcl).toSet

  private[this] def parseStringAcl(s: String): Set[String] =
    s.split(',').toSet

  private[this] def buildAdmin(x: AdminModel.AdminEntry): Admin =
    Admin(x.username, x.password, parseAcl(x.spectator), parseAcl(x.administrator), parseStringAcl(x.locations),
      parseAcl(x.unrestricted))

  def query(db: JdbcBackend#DatabaseDef, username: String, passwordHash: String)(implicit ec: ExecutionContext) =
    db.run(AdminModel.adminQuery(username,passwordHash).result.headOption)
      .map(_.map(buildAdmin))
}

object AdminModel {
  import slick.jdbc.PostgresProfile.api._

  case class AdminEntry(username: String, password: String, spectator: String, administrator: String,
                        locations: String, unrestricted: String)

  case class Admins(tag: Tag) extends Table[AdminEntry](tag, "admins") {
    def username = column[String]("username", O.PrimaryKey)
    def password = column[String]("password")
    def spectator = column[String]("spectator")
    def administrator = column[String]("administrator")
    def locations = column[String]("locations")
    def unrestricted = column[String]("unrestricted_view")

    override def * = (username, password, spectator, administrator, locations, unrestricted) <> (AdminEntry.tupled, AdminEntry.unapply)
  }

  private[this] val admins = TableQuery[Admins]
  val adminQuery = Compiled((username: Rep[String], passwordHash: Rep[String]) =>
    admins.filter(x => x.username === username && x.password === passwordHash).take(1))
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

  case object withCreateTasks extends Authorization[Admin, SessionAuthenticator] {
    override def isAuthorized[B](identity: Admin, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] =
      Future.successful(identity.canCreateTasks)
  }
}



object AdminNavlinkMatch {
  def apply(tab: String): Function[Int, play.api.mvc.Call] =
    tab match {
      case "status" => routes.AdminApplication.submits(_)
      case "monitor" => routes.AdminApplication.monitor(_)
      case "qanda" => routes.AdminApplication.showQandA(_)
      case "rejudge" => routes.AdminApplication.rejudgePage(_)
      case "tasks" => routes.AdminApplication.tasks(_)
      case "print" => routes.AdminApplication.listPrintJobs(_)
      case "teams" => routes.AdminApplication.listTeams(_)
      case "contests" => routes.AdminApplication.showContestList(_)
      case _ => routes.AdminApplication.submits(_)
    }
}

trait AdminsService extends IdentityService[Admin]

class AdminsServiceImpl(dbConfig: DatabaseConfig[JdbcProfile])(implicit val ec: ExecutionContext) extends AdminsService {
  override def retrieve(loginInfo: LoginInfo): Future[Option[Admin]] =
    AdminId.fromString(loginInfo.providerKey) match {
      case Some(adminId) => Admin.query(dbConfig.db, adminId.username, adminId.passwordHash)
      case None => Future.successful(None)
    }
}

class AdminsProvider(dbConfig: DatabaseConfig[JdbcProfile]) extends OneUserProvider {
  override def id: String = "admin"
  def authenticate(credentials: Credentials)(implicit ec: ExecutionContext): Future[Option[LoginInfo]] = {
    val pw = Hasher.getSha1(credentials.password)
    Admin.query(dbConfig.db, credentials.identifier, pw).map {
      case Some(v) => Some(LoginInfo(id, AdminId(v.username, pw).toString))
      case None => None
    }
  }
}