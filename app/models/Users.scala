package models

import com.mohiva.play.silhouette.api.{Identity, LoginInfo, Provider}
import com.mohiva.play.silhouette.api.services.IdentityService
import com.mohiva.play.silhouette.api.util.Credentials
import com.mohiva.play.silhouette.impl.exceptions.InvalidPasswordException
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{Contest, Extrainfo, LocalTeam}
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

case class LoggedInTeam(username: String, contest: Contest, team: LocalTeam, einfo: Seq[Extrainfo]) extends Identity

trait OneUserProvider extends Provider {
  def authenticate(credentials: Credentials)(implicit ec: ExecutionContext): Future[Option[LoginInfo]]
}

class TeamsProvider(dbConfig: DatabaseConfig[JdbcProfile]) extends OneUserProvider {
  override def id: String = "team"
  def authenticate(credentials: Credentials)(implicit ec: ExecutionContext): Future[Option[LoginInfo]] = {
    Users.authenticate(dbConfig.db, credentials.identifier, credentials.password).map(_.map(v => LoginInfo(id, v.username)))
  }
}

object Users {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel

  private[this] def extraInfoQuery(contest: Int) =
    SlickModel.extraInfos.filter(_.contest === contest).result

  def resolveQuery(username: String) =
    SlickModel.joinedLoginQuery.filter(_.username === username).result

  def toLoginInfo(x: SlickModel.LoggedInTeam0): LoggedInTeam =
    LoggedInTeam(x.username, x.contest, x.team, Seq())

  def authQuery(username: String, password: String) =
    SlickModel.joinedLoginQuery.filter(a => (a.username === username) && (a.password === password)).result

  def authenticate(db: JdbcBackend#DatabaseDef, username: String, password: String)(implicit ec: ExecutionContext): Future[Option[LoggedInTeam]] =
    db.run(authQuery(username, password)).map(_.headOption.map(toLoginInfo))

  def resolve(db: JdbcBackend#DatabaseDef, username: String)(implicit ec: ExecutionContext): Future[Option[LoggedInTeam]] =
    db.run(resolveQuery(username)).map(_.headOption).flatMap { opt =>
      opt.map(toLoginInfo).map { lt =>
        db.run(extraInfoQuery(lt.contest.id)).map { einfo =>
          Some(LoggedInTeam(lt.username, lt.contest, lt.team, einfo))
        }
      }.getOrElse(Future.successful(None))
    }
}

case class ContestTeamIds(contestId: Int, teamId: Int)