package org.stingray.qf.actors

import akka.actor.Props
import org.stingray.qf.models.{GlobalTeamState, LocalTeamState, TeamSchool}
import slick.jdbc.JdbcBackend

import scala.concurrent.Future

object TeamStateActor {
  type TeamState = Map[Int, Map[Int, LocalTeamState]]

  case class GetTeam(contest: Int, team: Int)
  case class GetTeams(contest: Int)

  def props(db: JdbcBackend#DatabaseDef) = Props(new TeamStateActor(db))
}

class TeamStateActor(db: JdbcBackend#DatabaseDef) extends AnyStateActor[TeamStateActor.TeamState] {
  import AnyStateActor._
  import TeamStateActor._
  import context.dispatcher

  private var teams: TeamState = Map.empty

  import slick.jdbc.MySQLProfile.api._

  private val dbioCombined =
    sql"""select ID, Name from Schools""".as[(Int, String)].zip(
      sql"""select ID, School, Num, Name from Teams""".as[(Int, Int, Int, String)]
    ).zip(sql"""select Contest, Team, LocalID, Disabled, NoPrint, NotRated from Participants"""
      .as[(Int, Int, Int, Boolean, Boolean, Boolean)])

  override def loadStart(): Future[TeamState] =
    db.run(dbioCombined).map {
      case ((schoolRows, teamRows), localRows) =>
        val schoolMap = schoolRows.map(x => x._1 -> TeamSchool(x._1, x._2)).toMap
        val globalTeamMap = teamRows.flatMap{x =>
          schoolMap.get(x._2).map{ teamSchool =>
            x._1 -> GlobalTeamState(x._1, teamSchool, x._3, x._4)
          }}.toMap
        localRows.flatMap { x =>
          globalTeamMap.get(x._2).map { globalTeam =>
            x._3 -> LocalTeamState(x._1, globalTeam, x._3, x._4, x._5, x._6)
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