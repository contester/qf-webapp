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

case class LoggedInTeam(username: String, contest: Contest, team: Team, einfo: Seq[Extrainfo]) extends Identity {
  /** The session key: the credential, "<round id>:<participant id>". A login string is not unambiguous across rounds. */
  def sessionKey: String = LoggedInTeam.key(contest.id, team.id)
}

object LoggedInTeam {
  def key(roundId: Int, participantId: Int): String = s"$roundId:$participantId"

  def parseKey(s: String): Option[(Int, Int)] = s.split(':') match {
    case Array(r, p) => scala.util.Try((r.toInt, p.toInt)).toOption
    case _ => None
  }
}

trait OneUserProvider extends Provider {
  def authenticate(credentials: Credentials)(implicit ec: ExecutionContext): Future[Option[LoginInfo]]
}

class TeamsProvider(dbConfig: DatabaseConfig[JdbcProfile]) extends OneUserProvider {
  override def id: String = "team"
  def authenticate(credentials: Credentials)(implicit ec: ExecutionContext): Future[Option[LoginInfo]] = {
    Users.authenticate(dbConfig.db, credentials.identifier, credentials.password).map(_.map(v => LoginInfo(id, v.sessionKey)))
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

  private[this] def normalize(login: String) = login.trim.toLowerCase(java.util.Locale.ROOT)

  private[this] val resolveQ = Compiled((roundId: Rep[Int], participantId: Rep[Int]) =>
    SlickModel.loginQuery.filter(x => x._3._1 === roundId && x._4.participantId === participantId).take(1)
  )

  def resolveQuery(roundId: Int, participantId: Int) =
    resolveQ(roundId, participantId).result.headOption

  def toLoginInfo(x: SlickModel.LoggedInTeam0): LoggedInTeam =
    LoggedInTeam(x.username, x.contest, x.team, Seq())

  private[this] val authQ = Compiled((username: Rep[String], password: Rep[String]) =>
    SlickModel.loginQuery.filter(a => (a._1.trim.toLowerCase === username) && (a._2 === password)).take(2)
  )

  def authQuery(username: String, password: String) =
    authQ(normalize(username), password).result

  private[this] val logger = Logger(getClass)

  /** More than one credential with this login and password: fail closed rather than guess. */
  def authenticate(db: JdbcBackend#DatabaseDef, username: String, password: String)(implicit ec: ExecutionContext): Future[Option[LoggedInTeam]] =
    db.run(authQuery(username, password)).map {
      case Seq(x) => Some(toLoginInfo(SlickModel.toLoggedInTeam0(x)))
      case Seq() => None
      case _ =>
        logger.warn(s"login '${normalize(username)}' with this password matches several credentials of the current season; rejected")
        None
    }

  /** Resolves a session key ("<round id>:<participant id>"); a deactivated or deleted credential no longer resolves. */
  def resolve(db: JdbcBackend#DatabaseDef, key: String)(implicit ec: ExecutionContext): Future[Option[LoggedInTeam]] =
    LoggedInTeam.parseKey(key) match {
      case None => Future.successful(None)
      case Some((roundId, participantId)) =>
        db.run(resolveQuery(roundId, participantId)).flatMap { opt =>
          opt.map(x => toLoginInfo(SlickModel.toLoggedInTeam0(x))).map { lt =>
            db.run(extraInfoQuery(lt.contest.id)).map { einfo =>
              Some(LoggedInTeam(lt.username, lt.contest, lt.team, einfo))
            }
          }.getOrElse(Future.successful(None))
        }
    }
}

case class ContestTeamIds(contestId: Int, teamId: Int)
