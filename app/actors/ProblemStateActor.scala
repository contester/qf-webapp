package org.stingray.qf.actors

import akka.actor.Props
import org.stingray.contester.dbmodel.Problem
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
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    import org.stingray.contester.dbmodel.SlickModel

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

  /**
    * Loaded at start-up and again on every ContestChanged; a removed round is dropped. A change of the `problems`
    * table (the bridge's sync at its start-up and on director's ContestEvent) fires round_changed for that round
    * (teacher-web 00009), which arrives here as ContestChanged; nothing is polled.
    */
  override def initialized: Receive = {
    case Refresh => doRefresh()
    case actors.StatusActor.ContestChanged(_) => doRefresh()
    case actors.StatusActor.ContestRemoved(round) => problems = problems - round
    case GetProblems(contest) => {
      sender() ! problems.getOrElse(contest, Seq.empty)
    }
  }
}