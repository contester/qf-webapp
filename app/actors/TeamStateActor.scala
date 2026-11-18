package org.stingray.qf.actors

import akka.actor.Props
import org.stingray.contester.dbmodel._
import play.api.Logging
import slick.jdbc.JdbcBackend

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

object TeamStateActor {
  type TeamState = Map[Int, Map[Int, Team]]

  case class GetTeam(contest: Int, team: Int)
  case class GetTeams(contest: Int)
  /** Replaces (or adds) a team in its round right away. */
  case class PutTeam(contest: Int, team: Team)
  /** The teams of one round as just read; None (the round is no longer current) drops it. */
  private case class RoundTeams(contest: Int, teams: scala.util.Try[Option[Seq[Team]]])

  /** How the actor reads: every current round's teams, and one round's (None: not a current round). */
  trait Loader {
    def all()(implicit ec: ExecutionContext): Future[TeamState]
    def round(id: Int)(implicit ec: ExecutionContext): Future[Option[Seq[Team]]]
  }

  class DbLoader(db: JdbcBackend#DatabaseDef) extends Loader {
    override def all()(implicit ec: ExecutionContext): Future[TeamState] =
      models.Rounds.teamsOfCurrentRounds(db).map(_.map { case (round, teams) => round -> teams.map(t => t.id -> t).toMap })

    // The same read as the full load, for one round.
    override def round(id: Int)(implicit ec: ExecutionContext): Future[Option[Seq[Team]]] =
      models.Rounds.currentRound(db, id).flatMap {
        case Some(_) => models.Rounds.teamsOfRound(db, id).map(Some(_))
        case None => Future.successful(None)
      }
  }

  def props(db: JdbcBackend#DatabaseDef): Props = propsWith(new DbLoader(db))
  def propsWith(loader: Loader): Props = Props(new TeamStateActor(loader))
}

class TeamStateActor(loader: TeamStateActor.Loader) extends AnyStateActor[TeamStateActor.TeamState] with Logging {
  import AnyStateActor._
  import TeamStateActor._
  import context.dispatcher

  private[this] var teams: TeamState = Map.empty
  // Per round: a reload is running / another one was asked for while it ran (exactly one more follows).
  private[this] val roundLoading = mutable.Set.empty[Int]
  private[this] val roundDirty = mutable.Set.empty[Int]

  override def loadStart(): Future[TeamState] = loader.all()

  override def setState(v: TeamState): Unit = {
    teams = v
  }

  private def reloadRound(round: Int): Unit =
    if (roundLoading(round)) roundDirty += round
    else {
      roundLoading += round
      val me = self
      Future.fromTry(scala.util.Try(loader.round(round))).flatten.onComplete(r => me ! RoundTeams(round, r))
    }

  /**
    * Loaded once at start-up; afterwards a round is re-read on TeamsChanged (credential_changed, participant_changed,
    * and round_changed, which also brings in a round that just became usable) and dropped on ContestRemoved. Reloads of
    * one round never overlap, so an older read cannot overwrite a newer one.
    */
  override def initialized: Receive = {
    case Refresh => doRefresh()
    case actors.StatusActor.ContestRemoved(round) =>
      teams = teams - round
      // A read already running may predate the removal: read once more.
      if (roundLoading(round)) roundDirty += round
    case actors.StatusActor.TeamsChanged(round) => reloadRound(round)
    case RoundTeams(round, result) =>
      roundLoading -= round
      result match {
        case scala.util.Success(Some(ts)) => teams = teams.updated(round, ts.map(t => t.id -> t).toMap)
        case scala.util.Success(None) => teams = teams - round
        case scala.util.Failure(e) => logger.error(s"reloading the teams of round $round", e)
      }
      if (roundDirty.remove(round)) reloadRound(round)
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
