package org.stingray.qf.actors

import akka.actor.Props
import org.stingray.contester.dbmodel._
import play.api.Logging
import slick.jdbc.JdbcBackend

import scala.concurrent.Future

object TeamStateActor {
  type TeamState = Map[Int, Map[Int, LocalTeam]]

  case class GetTeam(contest: Int, team: Int)
  case class GetTeams(contest: Int)
  /** Replaces (or adds) a team in its contest right away; the periodic refresh keeps it in line with the database. */
  case class PutTeam(team: LocalTeam)

  def props(db: JdbcBackend#DatabaseDef) = Props(new TeamStateActor(db))
}

class TeamStateActor(db: JdbcBackend#DatabaseDef) extends AnyStateActor[TeamStateActor.TeamState] with Logging {
  import AnyStateActor._
  import TeamStateActor._
  import context.dispatcher

  private[this] var teams: TeamState = Map.empty

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel

  private[this] val allSchools = Compiled(SlickModel.schools.map(identity))
  private[this] val allTeams = Compiled(SlickModel.teams.map(identity))
  private[this] val allParticipants = Compiled(SlickModel.participants.map(identity))

  override def loadStart(): Future[TeamState] =
    db.run(allSchools.result zip allTeams.result zip allParticipants.result).map {
      case ((schoolRows, teamRows), localRows) =>
        val schoolMap = schoolRows.map(x => x.id -> x).toMap
        val globalTeamMap = teamRows.flatMap{x =>
          schoolMap.get(x.school).map{ teamSchool =>
            x.id -> x
          }}.toMap
        localRows.flatMap { x =>
          globalTeamMap.get(x.team).flatMap { globalTeam =>
            schoolMap.get(globalTeam.school).map { school =>
              x.team -> LocalTeam(x.team, x.contest, school.fullName, school.shortName, globalTeam.num, globalTeam.name, x.notRated || x.disqualified, x.noPrint, x.disabled, x.disqualified)
            }
          }
        }.groupBy(_._2.contest).mapValues(_.toMap)
    }

  override def setState(v: TeamState): Unit = {
    teams = v
  }

  override def initialized: Receive = {
    case Refresh => doRefresh()
    case State(m) => setState(m)
    case PutTeam(team) =>
      teams = teams.updated(team.contest, teams.getOrElse(team.contest, Map.empty[Int, LocalTeam]) + (team.id -> team))
    case GetTeam(contest, team) => {
      sender() ! teams.get(contest).flatMap(_.get(team))
    }
    case GetTeams(contest) => {
      sender() ! teams.getOrElse(contest, Map.empty)
    }
  }
}