package models

import models.ContesterResults.FinishedTesting
import play.api.i18n.Messages
import play.api.libs.json.{JsValue, Writes, Json}
import slick.jdbc.JdbcBackend

import scala.concurrent.{ExecutionContext, Future}

case class AnnoSubmit(submitId: Int, contest: Int, team: Int, problem: String, result: SubmitResult)

trait SubmitResult {
  def success: Boolean
  def message: String
}

case class SubmitStats(timeMs: Int, memory: Memory)

case object SubmitWaiting extends SubmitResult {
  override val success: Boolean = false

  override val message: String = "..."
}

case object SubmitAccepted extends SubmitResult {
  override val success = true
  override val message = "Успешно"
}

case object SubmitCompileError extends SubmitResult {
  override val success = false
  override val message = "Не скомпилировалось"
}

case class SubmitPartialResult(passed: Int, taken: Int) extends SubmitResult {
  override val success = false

  override val message =
    s"$passed из $taken"
}

case class SubmitACMPartialResult(text: String, test: Option[Int]) extends SubmitResult {
  override val success = false

  override def message =
    s"${text}${test.map(x => s" на тесте $x").getOrElse("")}"
}

object SubmitResult {
  implicit val submitResultWrites = new Writes[SubmitResult] {
    override def writes(o: SubmitResult): JsValue = Json.obj(
      "success" -> o.success,
      "message" -> o.message
    )
  }

  private def annotate2(schoolMode: Boolean, submit: Submit, details: Seq[ResultEntry]): SubmitResult =
    if (!submit.finished)
      SubmitWaiting
    else if (submit.success)
      SubmitAccepted
    else if (!submit.compiled)
      SubmitCompileError
    else if (schoolMode)
      SubmitPartialResult(submit.passed, submit.taken)
    else {
      val lr = Submits.getTestingLastResult(details).map(x => (x.resultString, x.test)).getOrElse(("unknown", 0))
      SubmitACMPartialResult(lr._1, Some(lr._2))
    }


  def annotate(db: JdbcBackend#DatabaseDef, schoolMode: Boolean, submit: Submit)(implicit ec: ExecutionContext): Future[(SubmitResult, Seq[ResultEntry], SubmitStats)] =
    submit.testingId.map { tid =>
      Submits.loadSubmitDetails(db, tid)
    }.getOrElse(Future.successful(Nil)).map { details =>
      val sr = annotate2(schoolMode, submit, details)
      val st = Submits.getTestingStats(details)
      (sr, details, st)
    }

  def annotateFinished(db: JdbcBackend#DatabaseDef, finished: FinishedTesting)(implicit ec: ExecutionContext): Future[AnnoSubmit] =
    Submits.loadSubmitByID(db, finished.submit.id).map(_.get).flatMap { submit =>
      annotate(db, finished.submit.schoolMode, submit).map { submitResult =>
        AnnoSubmit(finished.submit.id, finished.submit.contest, finished.submit.team, finished.submit.problem, submitResult._1)
      }
    }

/*  val message = Map(
    1 -> "compiled",
    2 -> "compilationFailed",
    10 -> "accepted",
    11 -> "timeLimitExceeded",
    12 -> "runtimeError",
    13 -> "wrongAnswer",
    14 -> "wrongAnswer",
    15 -> "memoryLimitExceeded",
    16 -> "testerError",
    21 -> "rejected"
  ).mapValues(v => Messages(s"verdict.$v"))
  */

  val message = Map(
    1 -> "Скомпилировалось",
    2 -> "Ошибка компиляции",
    10 -> "Успешно",
    11 -> "Превышен лимит времени",
    12 -> "Ошибка выполнения",
    13 -> "Неверный ответ",
    14 -> "Неверный ответ",
    15 -> "Превышен лимит памяти",
    16 -> "Ошибка тестера",
    21 -> "Отвергнуто"
  )

  def success(code: Int) =
    code == 10 || code == 2
}

object AnnoSubmit {
  implicit val formatAnnoSubmit = Json.writes[AnnoSubmit]
}
