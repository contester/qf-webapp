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

case class SchoolSubmitResult(compiled: Boolean, passed: Int, taken: Int) extends SubmitResult {
  override val success = compiled && (passed == taken) && (passed > 0)

  override def message =
    if (success)
      "Полное решение"
    else if (!compiled)
      "Ошибка компиляции"
    else s"$passed из $taken"
}

case class ACMSubmitResult(success: Boolean, text: String, test: Option[Int]) extends SubmitResult {
  override def message =
    if (success)
      "Accepted"
    else s"${text}${test.map(x => s" on test $x").getOrElse("")}"
}

object SubmitResult {
  implicit val submitResultWrites = new Writes[SubmitResult] {
    override def writes(o: SubmitResult): JsValue = Json.obj(
      "success" -> o.success,
      "message" -> o.message
    )
  }

  def annotate(db: JdbcBackend#DatabaseDef, finished: FinishedTesting)(implicit ec: ExecutionContext): Future[AnnoSubmit] = {
    val result =
      if (finished.submit.schoolMode)
        Future.successful(SchoolSubmitResult(finished.compiled, finished.passed, finished.taken))
      else
        Submits.loadSubmitDetails(db, finished.testingId).map { details =>
          val lastResultOption = Submits.getTestingLastResult(details)
          Logger.info(s"$lastResultOption")
          lastResultOption.map { lastResult =>
            ACMSubmitResult(success(lastResult.result), message(lastResult.result), Some(lastResult.test))
          }.getOrElse(ACMSubmitResult(false, "", None))
        }

    result.map(AnnoSubmit(finished.submit.contest, finished.submit.team, finished.submit.problem, _))
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
