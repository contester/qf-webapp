package models

import com.mohiva.play.silhouette.api.{Identity, LoginInfo, Provider}
import com.mohiva.play.silhouette.api.services.IdentityService
import com.mohiva.play.silhouette.api.util.Credentials
import com.mohiva.play.silhouette.impl.exceptions.InvalidPasswordException
import org.joda.time.DateTime
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import slick.jdbc.{GetResult, JdbcBackend}

import scala.concurrent.{ExecutionContext, Future}

trait Team {
  def schoolName: String
  def teamNum: Option[Int]
  def teamName: String
  def notRated: Boolean
  def id: Int

  def schoolNameWithNum: String = s"$schoolName" + teamNum.map(x => s" #$x").getOrElse("")
  def teamFullName: String = {
    schoolNameWithNum +
      (if (!teamName.isEmpty) s": $teamName"
      else "")
  }
}

case class LocalTeam(localId: Int, schoolName: String, teamNum: Option[Int], teamName: String,
                     notRated: Boolean, noPrint: Boolean, disabled: Boolean) extends Team {
  override def id: Int = localId
}

trait TeamsService extends IdentityService[LoggedInTeam]

class TeamsServiceImpl(dbConfig: DatabaseConfig[JdbcProfile])(implicit val ec: ExecutionContext) extends TeamsService {
  override def retrieve(loginInfo: LoginInfo): Future[Option[LoggedInTeam]] =
    Users.resolve(dbConfig.db, loginInfo.providerKey)
}

case class LoggedInTeam(username: String, contest: Contest, team: LocalTeam, einfo: Seq[Extrainfo]) extends Identity {
  def matching(ctid: ContestTeamIds) =
    ctid.contestId == contest.id && ctid.teamId == team.localId
}

case class Extrainfo(contest: Int, num: Int, heading: String, data: String)

object Extrainfo {
  import slick.jdbc.GetResult

  implicit val getResult = GetResult(r =>
    Extrainfo(r.nextInt(), r.nextInt(), r.nextString(), r.nextString())
  )
}

class TeamsProvider(dbConfig: DatabaseConfig[JdbcProfile]) extends Provider {
  override def id: String = "team"
  def authenticate(credentials: Credentials)(implicit ec: ExecutionContext): Future[LoginInfo] = {
    Users.authenticate(dbConfig.db, credentials.identifier, credentials.password).map {
      case Some(v) => LoginInfo(id, v.username)
      case None => throw new InvalidPasswordException("foo")
    }
  }
}

object Users {
  import slick.driver.MySQLDriver.api._
  import slick.jdbc.GetResult

  implicit val getLoggedInTeam = GetResult(r => LoggedInTeam(
    r.nextString(),
    Contest(r.nextInt(), r.nextString(), r.nextBoolean(),
      new DateTime(r.nextTimestamp()), new DateTime(r.nextTimestamp()), new DateTime(r.nextTimestamp()),
      new DateTime(r.nextTimestamp())),
    LocalTeam(r.nextInt(), r.nextString(), r.nextIntOption(), r.nextString(), r.nextBoolean(),
    r.nextBoolean(), r.nextBoolean()), Seq()))


  def authQuery(username: String, password: String) =
    sql"""select Assignments.Username,
          Assignments.Contest as ContestID,
          Contests.Name as ContestName,
          Contests.SchoolMode as SchoolMode,
          Contests.Start, Contests.End, Contests.Finish, Contests.Expose,
          Assignments.LocalID as LocalID,
          Schools.Name as SchoolName,
          Teams.Num as TeamNum,
          Teams.Name as TeamName,
          Participants.NotRated,
          Participants.NoPrint,
          Participants.Disabled
          from Assignments, Participants, Contests, Teams, Schools
         where Assignments.Username = $username and Assignments.Password = $password
         and Assignments.Contest = Contests.ID and Participants.Contest = Contests.ID and
         Participants.LocalID = Assignments.LocalID and Participants.Team = Teams.ID and
         Teams.School = Schools.ID
       """.as[LoggedInTeam]

  def authenticate(db: JdbcBackend#DatabaseDef, username: String, password: String)(implicit ec: ExecutionContext): Future[Option[LoggedInTeam]] =
    db.run(authQuery(username, password)).map(_.headOption)

  def resolveQuery(username: String) =
    sql"""select Assignments.Username,
          Assignments.Contest as ContestID,
          Contests.Name as ContestName,
          Contests.SchoolMode as SchoolMode,
          Contests.Start, Contests.End, Contests.Finish, Contests.Expose,
          Assignments.LocalID as LocalID,
          Schools.Name as SchoolName,
          Teams.Num as TeamNum,
          Teams.Name as TeamName,
          Participants.NotRated,
          Participants.NoPrint,
          Participants.Disabled
          from Assignments, Participants, Contests, Teams, Schools
         where Assignments.Username = $username
         and Assignments.Contest = Contests.ID and Participants.Contest = Contests.ID and
         Participants.LocalID = Assignments.LocalID and Participants.Team = Teams.ID and
         Teams.School = Schools.ID
       """.as[LoggedInTeam]

  def extraInfoQuery(contest: Int) =
    sql"""select Contest, Num, Heading, Data from Extrainfo where Contest = $contest order by Num""".as[Extrainfo]

  def resolve(db: JdbcBackend#DatabaseDef, username: String)(implicit ec: ExecutionContext): Future[Option[LoggedInTeam]] =
    db.run(resolveQuery(username)).map(_.headOption).flatMap { opt =>
      opt.map { lt =>
        db.run(extraInfoQuery(lt.contest.id)).map { einfo =>
          Some(LoggedInTeam(lt.username, lt.contest, lt.team, einfo))
        }
      }.getOrElse(Future.successful(None))
    }
}

case class ContestTeamIds(contestId: Int, teamId: Int)

object ContestTeamIds {
  implicit val getResult = GetResult(r =>
    ContestTeamIds(r.nextInt(), r.nextInt())
  )
}

class UserPermissions(db: JdbcBackend#DatabaseDef) {
  import UserPermissions._
  import slick.driver.MySQLDriver.api._

  def submit(submitId: Int)(account: LoggedInTeam): Future[Boolean] =
    matching {
      db.run(sql"select Contest, Team from NewSubmits where ID = $submitId".as[ContestTeamIds])
    }(account)

}

object UserPermissions {
  def any(account: LoggedInTeam): Future[Boolean] = Future.successful(true)

  def matching(f: => Future[Seq[ContestTeamIds]])(account: LoggedInTeam) = {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext

    f.map(_.exists(account.matching))
  }
}

object Permissions {
  def any[T](account: T): Future[Boolean] = Future.successful(true)
}