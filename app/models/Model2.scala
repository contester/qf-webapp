package org.stingray.qf.models

import akka.actor.ActorRef
import models.Team
import org.stingray.qf.actors.TeamStateActor

import scala.concurrent.Future

case class TeamSchool(id: Int, name: String)

case class GlobalTeamState(id: Int, school: TeamSchool, num: Int, name: String)

case class LocalTeamState(contest: Int, team: GlobalTeamState, id: Int,
                          disabled: Boolean, noPrint: Boolean, notRated: Boolean) extends Team {
  override def schoolName: String = team.school.name

  override def teamNum: Option[Int] = if (team.num != 0) Some(team.num) else None

  override def teamName: String = team.name
}

class TeamClient(teamActor: ActorRef) {
  import akka.pattern.ask
  def getTeam(contest: Int, team: Int)(implicit timeout: akka.util.Timeout): Future[Option[LocalTeamState]] =
    (teamActor ? TeamStateActor.GetTeam(contest, team)).mapTo[Option[LocalTeamState]]

  def getTeams(contest: Int)(implicit timeout: akka.util.Timeout): Future[Map[Int, LocalTeamState]] =
    (teamActor ? TeamStateActor.GetTeams(contest)).mapTo[Map[Int, LocalTeamState]]
}