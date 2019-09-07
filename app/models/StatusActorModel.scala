package models

import actors.StatusActor.GetSingleContestResult
import actors.{StatusActor, WaiterActor}
import akka.actor.ActorSystem
import org.stingray.qf.actors.{ProblemStateActor, TeamStateActor}
import org.stingray.qf.models.TeamClient
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import utils.Ask

import scala.concurrent.{ExecutionContext, Future}

class StatusActorModel (dbConfig: DatabaseConfig[JdbcProfile], system: ActorSystem) {
  private val db = dbConfig.db
  val statusActor = system.actorOf(StatusActor.props(db), "status-actor")
  val waiterActor = system.actorOf(WaiterActor.props(db), "waiter-actor")
  val teamStateActor = system.actorOf(TeamStateActor.props(db), "team-state-actor")
  val teamClient = new TeamClient(teamStateActor)
  val problemStateActor = system.actorOf(ProblemStateActor.props(db), "problem-state-actor")
  val problemClient = new ProblemClient(problemStateActor)

  def getContest(contest: Int)(implicit timeout: akka.util.Timeout, ec: ExecutionContext): Future[Option[Contest]] =
    Ask[GetSingleContestResult](statusActor, StatusActor.GetSingleContest(contest)).map(_.c)
}