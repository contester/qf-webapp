package models

import actors.StatusActor.GetSingleContestResult
import actors.StatusActor
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
                        countdown: _root_.palevo.CountdownConfig) {
  private val db = dbConfig.db
  private val teamLookupTimeout: akka.util.Timeout = akka.util.Timeout(5.seconds)
  // teamClient is defined below; the lookup is only called after construction.
  val statusActor = system.actorOf(StatusActor.props(db, palevo, (c, t) => teamClient.getTeam(c, t)(teamLookupTimeout), liveHooks, countdown), "status-actor")
  val teamStateActor = system.actorOf(TeamStateActor.props(db), "team-state-actor")
  val teamClient = new TeamClient(teamStateActor)
  val problemStateActor = system.actorOf(ProblemStateActor.props(db), "problem-state-actor")
  val problemClient = new ProblemClient(problemStateActor)

  def getContest(contest: Int)(implicit timeout: akka.util.Timeout, ec: ExecutionContext): Future[Option[Contest]] =
    Ask[GetSingleContestResult](statusActor, StatusActor.GetSingleContest(contest)).map(_.c)

  /**
    * The round-change hooks of the notification listener, besides the status actor (which feeds its round clock) and
    * the team state actor it tells itself: the problems, and the live hooks (the live monitor, the public board cache).
    */
  def contestChanged(c: Contest): Unit = {
    problemStateActor ! StatusActor.ContestChanged(c)
    scala.util.Try(liveHooks.contestChanged(c))
    ()
  }

  def contestRemoved(roundId: Int): Unit = {
    problemStateActor ! StatusActor.ContestRemoved(roundId)
    scala.util.Try(liveHooks.contestRemoved(roundId))
    ()
  }

  /** The listener's hook for every TeamsChanged it sends the team state actor: the live monitor and the public board cache. */
  def teamsChanged(roundId: Int): Unit = {
    scala.util.Try(liveHooks.teamsChanged(roundId))
    ()
  }
}
