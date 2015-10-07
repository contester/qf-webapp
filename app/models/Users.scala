package models

import com.github.nscala_time.time.Imports._
import org.joda.time.DateTime
import slick.jdbc.{GetResult, JdbcBackend}

import scala.concurrent.{ExecutionContext, Future}

trait Team {
  def schoolName: String
  def teamNum: Option[Int]
  def teamName: String

  def schoolNameWithNum: String = s"$schoolName" + teamNum.map(x => s" #$x").getOrElse("")
  def teamFullName: String = {
    schoolNameWithNum +
      (if (!teamName.isEmpty) s": $teamName"
      else "")
  }
}

case class LocalTeam(localId: Int, schoolName: String, teamNum: Option[Int], teamName: String,
                     notRated: Boolean, noPrint: Boolean, disabled: Boolean) extends Team

case class LoggedInTeam(username: String, contest: Contest, team: LocalTeam) {
  def matching(ctid: ContestTeamIds) =
    ctid.contestId == contest.id && ctid.teamId == team.localId
}

object Users {
  import slick.jdbc.GetResult
  import slick.driver.MySQLDriver.api._

  implicit val getLoggedInTeam = GetResult(r => LoggedInTeam(
    r.nextString(),
    Contest(r.nextInt(), r.nextString(), r.nextBoolean(),
      new DateTime(r.nextTimestamp()), new DateTime(r.nextTimestamp()), new DateTime(r.nextTimestamp()),
      new DateTime(r.nextTimestamp())),
    LocalTeam(r.nextInt(), r.nextString(), r.nextIntOption(), r.nextString(), r.nextBoolean(),
    r.nextBoolean(), r.nextBoolean())))


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

  def resolve(db: JdbcBackend#DatabaseDef, username: String)(implicit ec: ExecutionContext): Future[Option[LoggedInTeam]] =
    db.run(resolveQuery(username)).map(_.headOption)
}


case class ContestTeamIds(contestId: Int, teamId: Int)

object ContestTeamIds {
  implicit val getResult = GetResult(r =>
    ContestTeamIds(r.nextInt(), r.nextInt())
  )
}

class UserPermissions(db: JdbcBackend#DatabaseDef) {
  import slick.driver.MySQLDriver.api._
  import play.api.libs.concurrent.Execution.Implicits.defaultContext
  import UserPermissions._

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