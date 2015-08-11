package models

import controllers.FinishedTesting
import play.api.Logger
import play.api.libs.json.{JsValue, Writes, Json}
import slick.jdbc.JdbcBackend

import scala.concurrent.{ExecutionContext, Future}

case class AnnoSubmit(contest: Int, team: Int, problem: String, result: SubmitResult)

trait SubmitResult {
  def success: Boolean
  def message: String
}

trait SubmitStats {
  def timeMs: Int
  def memory: Long
}

case object SubmitAccepted extends SubmitResult {
  override val success = true
  override val message = "Accepted"
}

case object SubmitCompileError extends SubmitResult {
  override val success = false
  override val message = "Compilation failed"
}

case class SubmitPartialResult(passed: Int, taken: Int) extends SubmitResult {
  override val success = false

  override val message =
    s"$passed из $taken"
}

case class SubmitACMPartialResult(text: String, test: Option[Int]) extends SubmitResult {
  override val success = false

  override def message =
    s"${text}${test.map(x => s" on test $x").getOrElse("")}"
}

object SubmitResult {
  implicit val submitResultWrites = new Writes[SubmitResult] {
    override def writes(o: SubmitResult): JsValue = Json.obj(
      "success" -> o.success,
      "message" -> o.message
    )
  }

  def annotate2(schoolMode: Boolean, submit: Submit, details: Seq[ResultEntry]): SubmitResult =
    if (submit.success)
      SubmitAccepted
    else if (!submit.status.compiled)
      SubmitCompileError
    else if (schoolMode)
      SubmitPartialResult(submit.status.passed, submit.status.taken)
    else {
      val lr = Submits.getTestingLastResult(details).map(x => (x.resultString, x.test)).getOrElse(("unknown", 0))
      SubmitACMPartialResult(lr._1, Some(lr._2))
    }


  def annotate(db: JdbcBackend#DatabaseDef, schoolMode: Boolean, submit: Submit)(implicit ec: ExecutionContext): Future[SubmitResult] =
    submit.testingId.map { tid =>
      Submits.loadSubmitDetails(db, tid)
    }.getOrElse(Future.successful(Nil)).map { details =>
      annotate2(schoolMode, submit, details)
    }

  def annotateFinished(db: JdbcBackend#DatabaseDef, finished: FinishedTesting)(implicit ec: ExecutionContext): Future[AnnoSubmit] =
    Submits.loadSubmitByID(db, finished.submit.id).map(_.get).flatMap { submit =>
      annotate(db, finished.submit.schoolMode, submit).map { submitResult =>
        AnnoSubmit(finished.submit.contest, finished.submit.team, finished.submit.problem, submitResult)
      }
    }

  val message = Map(
    1 -> "Compilation successful",
    2 -> "Compilation failed",
    10 -> "Accepted",
    11 -> "Time limit exceeded",
    12 -> "Runtime error",
    13 -> "Wrong answer",
    14 -> "Wrong answer",
    15 -> "Memory limit exceeded",
    16 -> "Tester error",
    21 -> "Rejected"
  )

  def success(code: Int) =
    code == 10 || code == 2
}

object AnnoSubmit {
  implicit val formatAnnoSubmit = Json.writes[AnnoSubmit]
}
