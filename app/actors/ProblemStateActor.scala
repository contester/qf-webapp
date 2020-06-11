package org.stingray.qf.actors

import akka.actor.Props
import models.{Problem, SlickModel}
import slick.jdbc.JdbcBackend

import scala.concurrent.Future

object ProblemStateActor {
  type ProblemState = Map[Int, Seq[Problem]]

  case class GetProblems(contest: Int)

  def props(db: JdbcBackend#DatabaseDef) = Props(new ProblemStateActor(db))
}

class ProblemStateActor(db: JdbcBackend#DatabaseDef) extends AnyStateActor[ProblemStateActor.ProblemState] {
  import AnyStateActor._
  import ProblemStateActor._
  import context.dispatcher

  private[this] var problems: ProblemState = Map.empty

  override def loadStart(): Future[ProblemState] = {
    import utils.MyPostgresProfile.api._

    db.run(SlickModel.problems.result).map { problemRows =>
      problemRows.groupBy(_.contest).mapValues { rows =>
        rows.map { row =>
          row.copy(id = row.id.toUpperCase)
        }.sortBy(_.id)
      }
    }
  }

  override def setState(v: ProblemState): Unit = {
    problems = v
  }

  override def initialized: Receive = {
    case Refresh => doRefresh()
    case State(m) => setState(m)
    case GetProblems(contest) => {
      sender() ! problems.getOrElse(contest, Seq.empty)
    }
  }
}