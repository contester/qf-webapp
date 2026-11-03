package models

import protos.Tickets.{BinaryJob, PrintJobReport}

/** Matching of print reports to print attempts. No I/O here: PrintingModel does the database work. */
object PrintAttempts {
  sealed trait JobRef
  /** "a-N": the attempt with id N. */
  case class AttemptRef(id: Long) extends JobRef
  /** "s-N": the old form, the job with id N (its latest attempt). */
  case class LegacyJobRef(jobId: Long) extends JobRef

  private val AttemptId = "a-(\\d{1,18})".r
  private val LegacyId = "s-(\\d{1,18})".r

  def parseJobId(s: String): Option[JobRef] = s match {
    case AttemptId(n) => Some(AttemptRef(n.toLong))
    case LegacyId(n) => Some(LegacyJobRef(n.toLong))
    case _ => None
  }

  object Status {
    val Queued = "queued"
    val Rendered = "rendered"
    val Printed = "printed"
    val Failed = "failed"
  }

  def isTerminal(status: String): Boolean = status == Status.Printed || status == Status.Failed

  case class AttemptState(status: String, hasRender: Boolean)

  sealed trait Action
  case object Ignore extends Action
  /** Store the render; advanceStatus says whether to move the attempt to "rendered". */
  case class AttachRender(render: BinaryJob, advanceStatus: Boolean) extends Action
  case class Finish(printed: Boolean) extends Action

  /** The agent gives up on a job that is still in the Windows queue; a printer coming back may print it later. */
  def mayStillPrint(error: String): Boolean = error.contains("did not finish in time")

  def decide(state: AttemptState, report: PrintJobReport): Action =
    report.stage match {
      case PrintJobReport.Stage.RENDERED =>
        report.render match {
          case Some(render) if !state.hasRender && !render.payload.isEmpty => AttachRender(render, advanceStatus = !isTerminal(state.status))
          case _ => Ignore
        }
      case PrintJobReport.Stage.FINAL =>
        if (isTerminal(state.status)) Ignore
        else Finish(printed = report.errorMessage.isEmpty)
      case _ => Ignore // a stage from a newer sender
    }
}
