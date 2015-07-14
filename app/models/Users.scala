package models


import slick.jdbc.JdbcBackend
import slick.profile.SqlStreamingAction

import scala.concurrent.{ExecutionContext, Future}

case class Contest(id: Int, name: String, schoolMode: Boolean)
//case class Team(val id: Int, val name: String, val notRated: Boolean)

case class LocalTeam(id: Int, schoolName: String, num: Option[Int], teamName: String)

case class LoggedInTeam(username: String, contest: Contest, team: LocalTeam)

object Users {
  import slick.jdbc.GetResult
  import slick.driver.MySQLDriver.api._

  implicit val getLoggedInTeam = GetResult(r => LoggedInTeam(
    r.nextString(),
    Contest(r.nextInt(), r.nextString(), r.nextBoolean()),
    LocalTeam(r.nextInt(), r.nextString(), r.nextIntOption(), r.nextString())))


  def authQuery(username: String, password: String) =
    sql"""select Assignments.Username,
          Assignments.Contest as ContestID,
          Contests.Name as ContestName,
          Contests.SchoolMode as SchoolMode,
          Assignments.LocalID as LocalID,
          Schools.Name as SchoolName,
          Teams.Num as TeamNum,
          Teams.Name as TeamName
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
          Assignments.LocalID as LocalID,
          Schools.Name as SchoolName,
          Teams.Num as TeamNum,
          Teams.Name as TeamName
          from Assignments, Participants, Contests, Teams, Schools
         where Assignments.Username = $username
         and Assignments.Contest = Contests.ID and Participants.Contest = Contests.ID and
         Participants.LocalID = Assignments.LocalID and Participants.Team = Teams.ID and
         Teams.School = Schools.ID
       """.as[LoggedInTeam]

  def resolve(db: JdbcBackend#DatabaseDef, username: String)(implicit ec: ExecutionContext): Future[Option[LoggedInTeam]] =
    db.run(resolveQuery(username)).map(_.headOption)
}