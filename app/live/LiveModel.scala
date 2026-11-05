package live

import models.{ACMCell, ACMScorer, Arrived, RatedProblem, Submit, SubmitId}
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest

sealed trait SubmitState
case object Pending extends SubmitState
case class Done(compiled: Boolean, passed: Int, taken: Int) extends SubmitState

case class SubmitRec(id: Int, team: Int, problem: String, arrivedAt: DateTime, arrivedSeconds: Int, state: SubmitState) {
  private[live] def toSubmit: Submit = {
    val (finished, compiled, passed, taken) = state match {
      case Done(c, p, t) => (true, c, p, t)
      case Pending => (false, false, 0, 0)
    }
    Submit(SubmitId(id, Arrived(arrivedAt, arrivedSeconds, afterFreeze = false), team, 0, RatedProblem(problem, 0), ""),
      finished, compiled, passed, taken, None)
  }
}

object SubmitRec {
  def fromSubmit(s: Submit): SubmitRec =
    SubmitRec(s.submitId.id, s.submitId.teamId, s.submitId.problem.id, s.submitId.arrived.timestamp,
      s.submitId.arrived.seconds,
      if (s.finished) Done(s.compiled, s.passed, s.taken) else Pending)
}

sealed trait BoardKind
case object Freezing extends BoardKind
case object Real extends BoardKind

case class FreezeRules(freezeTime: DateTime, endTime: DateTime, exposeTime: DateTime) {
  def hides(arrivedAt: DateTime, now: DateTime): Boolean =
    freezeTime.isBefore(endTime) && arrivedAt.isAfter(freezeTime) && now.isBefore(exposeTime)
}

object FreezeRules {
  def of(c: Contest): FreezeRules = FreezeRules(c.freezeTime, c.endTime, c.exposeTime)
}

sealed trait LiveCell {
  def solved: Boolean
  def text: String
}

object LiveCell {
  case object Empty extends LiveCell {
    val solved = false
    val text = ""
  }
  case class Failed(attempts: Int) extends LiveCell {
    val solved = false
    def text: String = ACMCell(attempts, 0, fullSolution = false).toString
  }
  case class Solved(attempts: Int, seconds: Int) extends LiveCell {
    val solved = true
    def text: String = ACMCell(attempts, seconds, fullSolution = true).toString
  }
  case class Pending(failures: Int) extends LiveCell {
    val solved = false
    def text: String = if (failures == 0) "?" else s"?$failures"
  }
}

object LiveCells {
  import LiveCell._

  def cellOf(recs: Seq[SubmitRec], kind: BoardKind, rules: FreezeRules, now: DateTime): LiveCell = {
    val sorted = recs.sortBy(r => (r.arrivedAt.getMillis, r.id))
    var cell = ACMCell.empty
    val it = sorted.iterator
    while (it.hasNext && !cell.fullSolution) {
      val r = it.next()
      if (r.state == live.Pending || (kind == Freezing && rules.hides(r.arrivedAt, now)))
        return LiveCell.Pending(cell.attempt)
      cell = ACMScorer(cell, r.toSubmit)._1
    }
    if (cell.attempt == 0) Empty
    else if (cell.fullSolution) Solved(cell.attempt, cell.arrivedSeconds)
    else Failed(cell.attempt)
  }
}
