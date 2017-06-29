package org.stingray.qf.actors

import akka.actor.{Actor, Stash}
import slick.jdbc.{GetResult, JdbcBackend}

import scala.concurrent.{ExecutionContext, Future}

case class TeamSchool(id: Int, name: String)

case class GlobalTeamState(id: Int, school: TeamSchool, num: Int, name: String)

case class LocalTeamState(contest: Int, team: GlobalTeamState, id: Int, disabled: Boolean, noPrint: Boolean, notRated: Boolean)

object TeamStateActor {
  type TeamState = Map[Int, Map[Int, LocalTeamState]]

  case object Refresh
  case class State(m: TeamState)
  case class GetTeam(contest: Int, team: Int)
  case class GetTeams(contest: Int)
}

class TeamStateActor(db: JdbcBackend#DatabaseDef, staticLocation: Option[String]) extends Actor with Stash {
  import TeamStateActor._
  import context.dispatcher

  @throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    self ! Refresh
  }

  private var teams: TeamState = Map.empty

  import slick.jdbc.MySQLProfile.api._

  private val dbioCombined =
    sql"""select ID, Name from Schools""".as[(Int, String)].zip(
      sql"""select ID, School, Num, Name from Teams""".as[(Int, Int, Int, String)]
    ).zip(sql"""select Contest, Team, LocalID, Disabled, NoPrint, NotRated from Participants""".as[(Int, Int, Int, Boolean, Boolean, Boolean)])

  private def buildState()(implicit ec: ExecutionContext): Future[TeamState] =
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

  private def loadState()(implicit ec: ExecutionContext) = {
    val fu = buildState()
    fu.foreach(x => self ! State(x))
    fu.onComplete { _ =>
      import scala.concurrent.duration._
      import scala.language.postfixOps
      context.system.scheduler.scheduleOnce(20 seconds, self, Refresh)
    }
    fu
  }

  override def receive: Receive = {
    case Refresh => loadState()
    case State(m) => {
      teams = m
      unstashAll()
      context.become(initialized)
    }
  }

  private def initialized: Receive = {
    case Refresh => loadState()
    case State(m) => {
      teams = m
    }
    case GetTeam(contest, team) => {
      sender() ! teams.get(contest).flatMap(_.get(team))
    }
    case GetTeams(contest) => {
      sender() ! teams.getOrElse(contest, Map.empty)
    }
  }
}