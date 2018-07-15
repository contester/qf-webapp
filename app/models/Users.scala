package models

import com.mohiva.play.silhouette.api.{Identity, LoginInfo, Provider}
import com.mohiva.play.silhouette.api.services.IdentityService
import com.mohiva.play.silhouette.api.util.Credentials
import com.mohiva.play.silhouette.impl.exceptions.InvalidPasswordException
import org.joda.time.DateTime
import play.api.Logger
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

case class LocalTeam(teamId: Int, contest: Int, localId: Int, schoolName: String, teamNum: Option[Int], teamName: String,
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
  import slick.jdbc.MySQLProfile.api._

/*
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
*/


  private def extraInfoQuery(contest: Int) =
    SlickModel.extraInfos.filter(_.contest === contest).result

  def resolveQuery(username: String) =
    SlickModel.joinedLoginQuery.filter(_.username === username).result

  def toLoginInfo(x: SlickModel.LoggedInTeam0): LoggedInTeam =
    LoggedInTeam(x.username, x.contest, x.team, Seq())

  def authQuery(username: String, password: String) =
    SlickModel.joinedLoginQuery.filter {
      case  a => (a.username === username) && (a.password === password)
    }.result

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

object ContestTeamIds {
  implicit val getResult = GetResult(r =>
    ContestTeamIds(r.nextInt(), r.nextInt())
  )
}