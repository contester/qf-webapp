package models

import models.ContesterResults.FinishedTesting
import models.Submits.{indexGrouped, scoreGrouped}
import play.api.{Logger, Logging}
import play.api.i18n.Messages
import play.api.libs.EventSource.{EventDataExtractor, EventNameExtractor}
import play.api.libs.json.{JsValue, Json, Writes}
import slick.jdbc.{GetResult, JdbcBackend}
import slick.lifted.MappedTo

import scala.concurrent.{ExecutionContext, Future}

case class AnnoSubmit(submitId: Int, contest: Int, team: Int, problem: String, result: SubmitResult)

trait SubmitResult {
  def success: Boolean
  def message: String
}

case class TimeMs(underlying: Long) extends AnyVal with MappedTo[Long] {
  override def toString: String = s"${underlying}"

  override def value: Long = underlying
}

object TimeMs {
  implicit val ordering: Ordering[TimeMs] = Ordering.by(_.underlying)
}

case class SubmitStats(timeMs: TimeMs, memory: Memory, timeLimitExceeded: Boolean = false)

case object SubmitWaiting extends SubmitResult {
  override val success: Boolean = false

  override val message: String = "Running..."
}

case object SubmitAccepted extends SubmitResult {
  override val success = true
  override val message = "ACCEPTED"
}

case object SubmitCompileError extends SubmitResult {
  override val success = false
  override val message = "Compilation error"
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

object SubmitResult extends Logging {
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

  private def finishedSuccess(finished: FinishedTesting) =
    finished.compiled && finished.taken != 0 && finished.passed == finished.taken

  def annotateFinished2(db: JdbcBackend#DatabaseDef, finished: FinishedTesting)(implicit ec: ExecutionContext): Future[AnnoSubmit] = {
    import slick.jdbc.MySQLProfile.api._

    val sr = if (finishedSuccess(finished))
      Future.successful(SubmitAccepted)
    else if (!finished.compiled)
      Future.successful(SubmitCompileError)
    else if (finished.submit.schoolMode)
      Future.successful(SubmitPartialResult(finished.passed, finished.taken))
    else {
      val xtid = finished.testingId.toLong
      db.run(SlickModel.results.filter(_.testingID === xtid).sortBy(_.testID.desc).take(1).result.headOption).map { lastResultOption =>
        lastResultOption.map { lastResult =>
          SubmitACMPartialResult(message.getOrElse(lastResult.resultCode, "wtf"), Some(lastResult.testID))
        }.getOrElse(SubmitACMPartialResult("unknown", None))
      }
    }

    sr.map(AnnoSubmit(finished.submit.id, finished.submit.contest, finished.submit.team, finished.submit.problem, _))
  }

  val message = Map(
    1 -> "Compilation successful",
    2 -> "Compilation error",
    10 -> "Ok",
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

  implicit val eventNameExtractor = EventNameExtractor[AnnoSubmit](_ => Some("submit"))
  implicit val eventDataExtractor = EventDataExtractor[AnnoSubmit] { c =>
    Json.stringify(Json.toJson(c))
  }
}