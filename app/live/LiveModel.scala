package live

import models.{Arrived, PointsCell, RatedProblem, Submit, SubmitId}
import org.joda.time.DateTime

/** Where a submit stands: in flight (`Pending`), or finished with its current testing's points (None: no points). */
sealed trait SubmitState
case object Pending extends SubmitState
case class Done(compiled: Boolean, points: Option[Double]) extends SubmitState

case class SubmitRec(id: Int, team: Int, problem: String, arrivedAt: DateTime, arrivedSeconds: Int, state: SubmitState) {
  /** The submit as the board's PointsScorer sees it (for comparing the live cells with the static board). */
  def toSubmit: Submit = {
    val (finished, compiled, points) = state match {
      case Done(c, p) => (true, c, p)
      case Pending => (false, false, None)
    }
    Submit(SubmitId(id, Arrived(arrivedAt, arrivedSeconds), team, 0, RatedProblem(problem, 0), ""),
      finished, compiled, 0, 0, None, points)
  }

  /** In flight: not finished yet, or finished and compiled without points (a rejudge reset, a testing being redone). */
  def inFlight: Boolean = state match {
    case Pending => true
    case Done(compiled, points) => compiled && points.isEmpty
  }
}

object SubmitRec {
  def fromSubmit(s: Submit): SubmitRec =
    SubmitRec(s.submitId.id, s.submitId.teamId, s.submitId.problem.id, s.submitId.arrived.timestamp,
      s.submitId.arrived.seconds,
      if (s.finished) Done(s.compiled, s.points) else Pending)
}

/**
  * One team's cell of the live board: the points board's cell (`best`, `attempts`, `full`, the same rules as
  * PointsScorer) plus `pending`, the number of the team's submits for the problem that are in flight (shown as a
  * marker, never as the best), and `fullAt`, the seconds from the start of the earliest submit that reached the
  * maximum (for palevo's first-solve announcements).
  */
case class LiveCell(best: Option[Double], attempts: Int, full: Boolean, pending: Int, fullAt: Option[Int]) {
  def points: PointsCell = PointsCell(best, attempts, full, pending)
  def finishedAttempts: Int = (attempts - pending).max(0)
  def isEmpty: Boolean = attempts == 0
}

object LiveCell {
  val Empty: LiveCell = LiveCell(None, 0, full = false, 0, None)
}

object LiveCells {
  def cellOf(recs: Seq[SubmitRec], maxPoints: Double): LiveCell =
    if (recs.isEmpty) LiveCell.Empty
    else {
      val scored = recs.flatMap(r => r.state match {
        case Done(true, Some(p)) => Some(r -> p)
        case _ => None
      })
      val best = if (scored.isEmpty) None else Some(scored.map(_._2).max)
      val full = maxPoints > 0 && best.contains(maxPoints)
      val fullAt = if (full) Some(scored.filter(_._2 == maxPoints).map(_._1.arrivedSeconds).min) else None
      LiveCell(best, recs.size, full, recs.count(_.inFlight), fullAt)
    }
}
