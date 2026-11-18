package org.stingray.qf.actors

import akka.actor.Props
import org.stingray.contester.dbmodel.Problem
import slick.jdbc.JdbcBackend

import scala.concurrent.{ExecutionContext, Future}

object ProblemStateActor {
  type ProblemState = Map[Int, Seq[Problem]]

  case class GetProblems(contest: Int)

  private def normalize(rows: Seq[Problem]): Seq[Problem] = rows.map(row => row.copy(id = row.id.toUpperCase)).sortBy(_.id)

  /** Every round's problems, ids upper-cased and sorted. */
  def loadAll(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[ProblemState] = {
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    import org.stingray.contester.dbmodel.SlickModel
    db.run(SlickModel.problems.result).map(_.groupBy(_.contest).map { case (k, v) => k -> normalize(v) })
  }

  /** One round's problems, as loadAll has them. */
  def loadRound(db: JdbcBackend#DatabaseDef, round: Int)(implicit ec: ExecutionContext): Future[Seq[Problem]] = {
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    import org.stingray.contester.dbmodel.SlickModel
    db.run(SlickModel.problems.filter(_.contestID === round).result).map(rows => normalize(rows))
  }

  def props(db: JdbcBackend#DatabaseDef): Props = Props(new ProblemStateActor(ec => loadAll(db)(ec)))

  /** With any loader (tests). */
  def propsWith(load: ExecutionContext => Future[ProblemState]): Props = Props(new ProblemStateActor(load))
}

class ProblemStateActor(load: ExecutionContext => Future[ProblemStateActor.ProblemState])
  extends AnyStateActor[ProblemStateActor.ProblemState] {
  import AnyStateActor._
  import ProblemStateActor._

  private[this] var problems: ProblemState = Map.empty

  override def loadStart(): Future[ProblemState] = load(context.dispatcher)

  override def setState(v: ProblemState): Unit = {
    problems = v
  }

  /**
    * Loaded at start-up and again on every ContestChanged (coalesced, see AnyStateActor); a removed round is dropped.
    * A change of the `problems` table (the bridge's sync at its start-up and on director's ContestEvent) fires
    * round_changed for that round (teacher-web 00009), which arrives here as ContestChanged; nothing is polled.
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
