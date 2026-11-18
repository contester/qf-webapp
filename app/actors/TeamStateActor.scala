package org.stingray.qf.actors

import akka.actor.Props
import org.stingray.contester.dbmodel._
import play.api.Logging
import slick.jdbc.JdbcBackend

import scala.concurrent.Future

object TeamStateActor {
  type TeamState = Map[Int, Map[Int, Team]]

  case class GetTeam(contest: Int, team: Int)
  case class GetTeams(contest: Int)
  /** Replaces (or adds) a team in its round right away; the periodic refresh keeps it in line with the database. */
  case class PutTeam(contest: Int, team: Team)
  /** The teams of one round as just read; None (the round is no longer current) drops it. */
  private case class RoundTeams(contest: Int, teams: Option[Seq[Team]])

  def props(db: JdbcBackend#DatabaseDef) = Props(new TeamStateActor(db))
}

class TeamStateActor(db: JdbcBackend#DatabaseDef) extends AnyStateActor[TeamStateActor.TeamState] with Logging {
  import AnyStateActor._
  import TeamStateActor._
  import context.dispatcher

  private[this] var teams: TeamState = Map.empty

  override def loadStart(): Future[TeamState] =
    models.Rounds.teamsOfCurrentRounds(db).map(_.map { case (round, teams) => round -> teams.map(t => t.id -> t).toMap })

  override def setState(v: TeamState): Unit = {
    teams = v
  }

  override def initialized: Receive = {
    case Refresh => doRefresh()
    case State(m) => setState(m)
    case actors.StatusActor.TeamsChanged(round) =>
      // The same read as the full refresh, for one round; a round that is no longer current is dropped.
      models.Rounds.currentRound(db, round).flatMap {
        case Some(_) => models.Rounds.teamsOfRound(db, round).map(Some(_))
        case None => Future.successful(None)
      }.map(RoundTeams(round, _)).onComplete {
        case scala.util.Success(m) => self ! m
        case scala.util.Failure(e) => logger.error(s"reloading the teams of round $round", e)
      }
    case RoundTeams(round, loaded) =>
      teams = loaded match {
        case Some(ts) => teams.updated(round, ts.map(t => t.id -> t).toMap)
        case None => teams - round
      }
    case PutTeam(contest, team) =>
      teams = teams.updated(contest, teams.getOrElse(contest, Map.empty[Int, Team]) + (team.id -> team))
    case GetTeam(contest, team) => {
      sender() ! teams.get(contest).flatMap(_.get(team))
    }
    case GetTeams(contest) => {
      sender() ! teams.getOrElse(contest, Map.empty)
    }
  }
}