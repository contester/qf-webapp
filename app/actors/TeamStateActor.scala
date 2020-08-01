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

  def props(db: JdbcBackend#DatabaseDef) = Props(new TeamStateActor(db))
}

class TeamStateActor(db: JdbcBackend#DatabaseDef) extends AnyStateActor[TeamStateActor.TeamState] with Logging {
  import AnyStateActor._
  import TeamStateActor._
  import context.dispatcher

  private[this] var teams: TeamState = Map.empty

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel

  override def loadStart(): Future[TeamState] =
    db.run(SlickModel.schools.result zip SlickModel.teams.result zip SlickModel.participants.result).map {
      case ((schoolRows, teamRows), localRows) =>
        val schoolMap = schoolRows.map(x => x.id -> x).toMap
        val globalTeamMap = teamRows.flatMap{x =>
          schoolMap.get(x.school).map{ teamSchool =>
            x.id -> x
          }}.toMap
        localRows.flatMap { x =>
          globalTeamMap.get(x.team).flatMap { globalTeam =>
            schoolMap.get(globalTeam.school).map { school =>
              x.team -> LocalTeam(x.team, x.contest, school.name, globalTeam.num, globalTeam.name, x.notRated, x.noPrint, x.disabled)
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
    case GetTeam(contest, team) => {
      sender() ! teams.get(contest).flatMap(_.get(team))
    }
    case GetTeams(contest) => {
      sender() ! teams.getOrElse(contest, Map.empty)
    }
  }
}