package models

import actors.StatusActor.GetSingleContestResult
import actors.{StatusActor, WaiterActor}
import akka.actor.ActorSystem
import org.stingray.qf.actors.{ProblemStateActor, TeamStateActor}
import palevo.PalevoSink
import org.stingray.qf.models.TeamClient
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import utils.Ask
import org.stingray.contester.dbmodel._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class StatusActorModel (dbConfig: DatabaseConfig[JdbcProfile], system: ActorSystem, palevo: PalevoSink, liveHooks: live.LiveHooks,
                        timeEvents: Boolean, countdown: _root_.palevo.CountdownConfig) {
  private val db = dbConfig.db
  private val teamLookupTimeout: akka.util.Timeout = akka.util.Timeout(5.seconds)
  // teamClient is defined below; the lookup is only called after construction.
  val statusActor = system.actorOf(StatusActor.props(db, palevo, (c, t) => teamClient.getTeam(c, t)(teamLookupTimeout), liveHooks, timeEvents, countdown), "status-actor")
  val waiterActor = system.actorOf(WaiterActor.props(db, palevo), "waiter-actor")
  val teamStateActor = system.actorOf(TeamStateActor.props(db), "team-state-actor")
  val teamClient = new TeamClient(teamStateActor)
  val problemStateActor = system.actorOf(ProblemStateActor.props(db), "problem-state-actor")
  val problemClient = new ProblemClient(problemStateActor)

  def getContest(contest: Int)(implicit timeout: akka.util.Timeout, ec: ExecutionContext): Future[Option[Contest]] =
    Ask[GetSingleContestResult](statusActor, StatusActor.GetSingleContest(contest)).map(_.c)

  /** A contest row was changed through the application: the status actor and the live monitor learn of it at once. */
  def contestChanged(c: Contest): Unit = {
    statusActor ! StatusActor.ContestChanged(c)
    scala.util.Try(liveHooks.contestChanged(c))
    ()
  }
}
