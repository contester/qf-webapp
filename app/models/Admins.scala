package models

import com.mohiva.play.silhouette.api.{Authorization, Identity, LoginInfo}
import com.mohiva.play.silhouette.api.services.IdentityService
import com.mohiva.play.silhouette.api.util.Credentials
import com.mohiva.play.silhouette.impl.authenticators.SessionAuthenticator
import controllers.{PasswordHash, routes}
import play.api.mvc.Request
import slick.basic.DatabaseConfig
import slick.jdbc.{JdbcBackend, JdbcProfile}

import scala.concurrent.{ExecutionContext, Future}

/** What the session cookie carries: the username and a fingerprint of the stored hash (never the hash). */
case class AdminId(username: String, fingerprint: String) {
  override def toString = s"$username:$fingerprint"
}

/**
  * An administrator of teacher-web (admins table). Every admin may do everything in every round, so the permission
  * methods are constants; they stay methods so that the pages keep asking the same questions.
  */
case class Admin(email: String, passwordHash: String) extends Identity {
  override def toString = s"$email:${PasswordHash.fingerprint(passwordHash)}"

  def toId = AdminId(email, PasswordHash.fingerprint(passwordHash))

  def canSpectate(contestId: Int) = true

  def canModify(contestId: Int) = true

  def isSuperAdmin = true
}

object AdminId {
  def fromString(s: String): Option[AdminId] = {
    val i = s.lastIndexOf(':')
    if (i > 0 && i < s.length - 1) Some(AdminId(s.substring(0, i), s.substring(i + 1)))
    else None
  }
}

object Admins {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel

  // teacher-web enforces uniqueness on LOWER(BTRIM(email)).
  private[this] def normalize(email: String) = email.trim.toLowerCase(java.util.Locale.ROOT)

  private[this] val byEmailQ = Compiled((email: Rep[String]) =>
    SlickModel.admins.filter(_.email.trim.toLowerCase === email).take(1))

  def byEmail(db: JdbcBackend#DatabaseDef, email: String)(implicit ec: ExecutionContext): Future[Option[Admin]] =
    db.run(byEmailQ(normalize(email)).result.headOption).map(_.map(x => Admin(x._1, x._2)))

  def authenticate(db: JdbcBackend#DatabaseDef, email: String, password: String)(implicit ec: ExecutionContext): Future[Option[Admin]] =
    byEmail(db, email).map {
      case Some(a) if PasswordHash.verify(password, a.passwordHash) => Some(a)
      case Some(_) => None
      case None =>
        PasswordHash.verifyDummy(password)
        None
    }
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

}



object AdminNavlinkMatch {
  def apply(tab: String): Function[Int, play.api.mvc.Call] =
    tab match {
      case "status" => routes.AdminApplication.submits(_, None, None, None)
      case "customtests" => routes.AdminApplication.customTests(_, None, None, None)
      case "monitor" => routes.AdminApplication.monitor(_)
      case "qanda" => routes.AdminApplication.showQandA(_)
      case "rejudge" => routes.AdminApplication.rejudgePage(_)
      case "contests" => routes.AdminApplication.showContestList(_)
      case "extrainfo" => routes.AdminApplication.extraInfoPage(_)
      case "contester" => routes.AdminApplication.contester(_)
      case "participants" => routes.AdminApplication.participants(_)
      case "languages" => cid => routes.AdminApplication.languages(Some(cid))
      case _ => routes.AdminApplication.submits(_, None, None, None)
    }
}

trait AdminsService extends IdentityService[Admin]

class AdminsServiceImpl(dbConfig: DatabaseConfig[JdbcProfile])(implicit val ec: ExecutionContext) extends AdminsService {
  override def retrieve(loginInfo: LoginInfo): Future[Option[Admin]] =
    AdminId.fromString(loginInfo.providerKey) match {
      case Some(adminId) =>
        Admins.byEmail(dbConfig.db, adminId.username).map(_.filter(_.toId == adminId))
      case None => Future.successful(None)
    }
}

class AdminsProvider(dbConfig: DatabaseConfig[JdbcProfile]) extends OneUserProvider {
  override def id: String = "admin"
  def authenticate(credentials: Credentials)(implicit ec: ExecutionContext): Future[Option[LoginInfo]] =
    Admins.authenticate(dbConfig.db, credentials.identifier, credentials.password).map(_.map(a => LoginInfo(id, a.toId.toString)))
}
