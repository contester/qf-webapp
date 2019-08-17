package org.stingray.qf.actors

import akka.actor.Props
import com.github.nscala_time.time.Imports.DateTime
import models.{Contest, SlickModel}
import slick.jdbc.{GetResult, JdbcBackend}

import scala.concurrent.Future

object ContestStateActor {
  type ContestState = Map[Int, Contest]

  def props(db: JdbcBackend#DatabaseDef) = Props(new TeamStateActor(db))
}

class ContestStateActor(db: JdbcBackend#DatabaseDef) extends AnyStateActor[ContestStateActor.ContestState] {
  import AnyStateActor._
  import ContestStateActor._
  import context.dispatcher

  private var contests: ContestState = Map.empty

  import slick.jdbc.MySQLProfile.api._


  override def loadStart(): Future[ContestState] =
    db.run(SlickModel.contests.result).map { rows =>
      rows.map(x => x.id -> x).toMap
    }

  override def setState(v: ContestState): Unit = {
    contests = v
  }

  override def initialized: Receive = {
    case Refresh => doRefresh()
    case State(m) => setState(m)
  }
}