package org.stingray.qf.models

import akka.actor.ActorRef
import org.stingray.contester.dbmodel.Team
import org.stingray.qf.actors.TeamStateActor

import scala.concurrent.Future

class TeamClient(teamActor: ActorRef) {
  import akka.pattern.ask
  def getTeam(contest: Int, team: Int)(implicit timeout: akka.util.Timeout): Future[Option[Team]] =
    (teamActor ? TeamStateActor.GetTeam(contest, team)).mapTo[Option[Team]]

  def put(contest: Int, team: Team): Unit = teamActor ! TeamStateActor.PutTeam(contest, team)

  def getTeams(contest: Int)(implicit timeout: akka.util.Timeout): Future[Map[Int, Team]] =
    (teamActor ? TeamStateActor.GetTeams(contest)).mapTo[Map[Int, Team]]
}