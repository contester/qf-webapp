package org.stingray.qf.actors

import akka.actor.Props
import models.Problem
import slick.jdbc.JdbcBackend

import scala.concurrent.Future

object ProblemStateActor {
  type ProblemState = Map[Int, Map[String, Problem]]

  case class GetProblems(contest: Int)

  def props(db: JdbcBackend#DatabaseDef) = Props(new ProblemStateActor(db))
}

class ProblemStateActor(db: JdbcBackend#DatabaseDef) extends AnyStateActor[ProblemStateActor.ProblemState] {
  import AnyStateActor._
  import ProblemStateActor._
  import context.dispatcher

  private var problems: ProblemState = Map.empty

  import slick.jdbc.MySQLProfile.api._

  private val dbioCombined =
    sql"""select Contest, ID, Tests, Name, Rating from Problems""".as[(Int, String, Int, String, Int)]

  override def loadStart(): Future[ProblemState] =
    db.run(dbioCombined).map { problemRows =>
      problemRows.groupBy(_._1).mapValues { rows =>
        rows.map { row =>
          row._2 -> Problem(row._2, row._4, row._3, row._5)
        }.toMap
      }
    }

  override def setState(v: ProblemState): Unit = {
    problems = v
  }

  override def initialized: Receive = {
    case Refresh => doRefresh()
    case State(m) => setState(m)
    case GetProblems(contest) => {
      sender() ! problems.getOrElse(contest, Map.empty)
    }
  }
}