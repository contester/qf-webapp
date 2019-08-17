package org.stingray.qf.models

import akka.actor.ActorRef
import models.{LocalTeam, Team}
import org.stingray.qf.actors.TeamStateActor

import scala.concurrent.Future

class TeamClient(teamActor: ActorRef) {
  import akka.pattern.ask
  def getTeam(contest: Int, team: Int)(implicit timeout: akka.util.Timeout): Future[Option[LocalTeam]] =
    (teamActor ? TeamStateActor.GetTeam(contest, team)).mapTo[Option[LocalTeam]]

  def getTeams(contest: Int)(implicit timeout: akka.util.Timeout): Future[Map[Int, LocalTeam]] =
    (teamActor ? TeamStateActor.GetTeams(contest)).mapTo[Map[Int, LocalTeam]]
}