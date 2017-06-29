package org.stingray.qf.actors

import akka.actor.Props
import models.Contest
import slick.jdbc.JdbcBackend

import scala.concurrent.Future

object ContestStateActor {
  type ContestState = Map[Int, Contest]

  def props(db: JdbcBackend#DatabaseDef) = Props(new TeamStateActor(db))
}

class TeamStateActor(db: JdbcBackend#DatabaseDef) extends AnyStateActor[ContestStateActor.ContestState] {
  import AnyStateActor._
  import ContestStateActor._
  import context.dispatcher

  private var contests: ContestState = Map.empty

  import slick.jdbc.MySQLProfile.api._

  private val dbioCombined =
    sql"""select ID, Name from Schools""".as[(Int, String)].zip(
      sql"""select ID, School, Num, Name from Teams""".as[(Int, Int, Int, String)]
    ).zip(sql"""select Contest, Team, LocalID, Disabled, NoPrint, NotRated from Participants"""
      .as[(Int, Int, Int, Boolean, Boolean, Boolean)])

  override def loadStart(): Future[ContestState] = ???

  override def setState(v: ContestState): Unit = {
    contests = v
  }

  override def initialized: Receive = {
    case Refresh => doRefresh()
    case State(m) => setState(m)
  }
}