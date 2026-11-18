package models

import com.mohiva.play.silhouette.api.{Identity, LoginInfo, Provider}
import com.mohiva.play.silhouette.api.services.IdentityService
import com.mohiva.play.silhouette.api.util.Credentials
import com.mohiva.play.silhouette.impl.exceptions.InvalidPasswordException
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{Contest, Extrainfo, Team}
import play.api.Logger
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import slick.jdbc.{GetResult, JdbcBackend}

import scala.concurrent.{ExecutionContext, Future}

trait TeamsService extends IdentityService[LoggedInTeam]

class TeamsServiceImpl(dbConfig: DatabaseConfig[JdbcProfile])(implicit val ec: ExecutionContext) extends TeamsService {
  override def retrieve(loginInfo: LoginInfo): Future[Option[LoggedInTeam]] =
    Users.resolve(dbConfig.db, loginInfo.providerKey)
}

case class LoggedInTeam(username: String, contest: Contest, team: Team, einfo: Seq[Extrainfo]) extends Identity

trait OneUserProvider extends Provider {
  def authenticate(credentials: Credentials)(implicit ec: ExecutionContext): Future[Option[LoginInfo]]
}

class TeamsProvider(dbConfig: DatabaseConfig[JdbcProfile]) extends OneUserProvider {
  override def id: String = "team"
  def authenticate(credentials: Credentials)(implicit ec: ExecutionContext): Future[Option[LoginInfo]] = {
    Users.authenticate(dbConfig.db, credentials.identifier, credentials.password).map(_.map(v => LoginInfo(id, v.username)))
  }
}

/**
  * Team login against teacher-web's round_credentials. loginQuery already restricts to active credentials of
  * non-deleted participants in the rounds of the current season. The login is matched like teacher-web's
  * uniqueness index (LOWER(BTRIM(login))); the password is the stored plaintext, compared exactly (teacher-web
  * prints it on slips). The round need not be running: teams may log in to see their results after it ends.
  */
object Users {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel

  private[this] def extraInfoQuery(contest: Int) =
    SlickModel.extraInfos.filter(_.contest === contest).sortBy(_.num).result

  private[this] def normalize(login: String) = login.trim.toLowerCase

  private[this] val resolveQ = Compiled((username: Rep[String]) =>
    SlickModel.loginQuery.filter(_._1.trim.toLowerCase === username).take(1)
  )

  def resolveQuery(username: String) =
    resolveQ(normalize(username)).result.headOption

  def toLoginInfo(x: SlickModel.LoggedInTeam0): LoggedInTeam =
    LoggedInTeam(x.username, x.contest, x.team, Seq())

  private[this] val authQ = Compiled((username: Rep[String], password: Rep[String]) =>
    SlickModel.loginQuery.filter(a => (a._1.trim.toLowerCase === username) && (a._2 === password)).take(1)
  )

  def authQuery(username: String, password: String) =
    authQ(normalize(username), password).result.headOption

  def authenticate(db: JdbcBackend#DatabaseDef, username: String, password: String)(implicit ec: ExecutionContext): Future[Option[LoggedInTeam]] =
    db.run(authQuery(username, password)).map(_.map(x => toLoginInfo(SlickModel.toLoggedInTeam0(x))))

  def resolve(db: JdbcBackend#DatabaseDef, username: String)(implicit ec: ExecutionContext): Future[Option[LoggedInTeam]] =
    db.run(resolveQuery(username)).flatMap { opt =>
      opt.map(x => toLoginInfo(SlickModel.toLoggedInTeam0(x))).map { lt =>
        db.run(extraInfoQuery(lt.contest.id)).map { einfo =>
          Some(LoggedInTeam(lt.username, lt.contest, lt.team, einfo))
        }
      }.getOrElse(Future.successful(None))
    }
}

case class ContestTeamIds(contestId: Int, teamId: Int)