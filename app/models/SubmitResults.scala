package models

import models.ContesterResults.FinishedTesting
import models.Submits.indexGrouped
import org.stingray.contester.dbmodel.{Memory, TimeMs}
import play.api.{Logger, Logging}
import play.api.i18n.Messages
import play.api.libs.EventSource.{EventDataExtractor, EventNameExtractor}
import play.api.libs.json.{JsValue, Json, OWrites, Writes}
import slick.jdbc.{GetResult, JdbcBackend}
import slick.lifted.MappedTo

import scala.concurrent.{ExecutionContext, Future}

case class AnnoSubmit(submitId: Int, contest: Int, team: Int, problem: String, result: SubmitResult)

/** A rendered row of the team's submit table, ready to be put in place of the row with the same id. */
case class SubmitRow(id: Int, html: String)

/**
  * The team's attempts on a problem that a finished testing changed: that attempt and the later ones. Only for the
  * pages that are open (never stored), a page loaded later renders the table anew.
  */
case class SubmitRows(contest: Int, team: Int, rows: Seq[SubmitRow])

object SubmitRows {
  implicit val writesSubmitRow: OWrites[SubmitRow] = Json.writes[SubmitRow]
  implicit val writesSubmitRows: OWrites[SubmitRows] = Json.writes[SubmitRows]

  implicit val eventNameExtractor = EventNameExtractor[SubmitRows](_ => Some("submitRows"))
  implicit val eventDataExtractor = EventDataExtractor[SubmitRows] { c =>
    Json.stringify(Json.toJson(c))
  }
}

trait SubmitResult {
  def success: Boolean
  def message: String
  def display(implicit messages: Messages): String = message
  def stateTag: String = "finished"
}

case class SubmitStats(timeMs: TimeMs, memory: Memory, timeLimitExceeded: Boolean = false)

case object SubmitWaiting extends SubmitResult {
  override val success: Boolean = false

  override val message: String = "Running..."

  override val stateTag: String = "waiting"

  override def display(implicit messages: Messages): String = SubmitResult.tr("status.running", message)
}

case object SubmitCompileError extends SubmitResult {
  override val success = false
  override val message = "Compilation error"

  override def display(implicit messages: Messages): String = SubmitResult.tr("status.2", message)
}

case class SubmitPointsResult(points: Double, max: Double) extends SubmitResult {
  override val success: Boolean = points == max && max > 0

  override val message: String = s"Баллы: ${TestRows.formatPoints(points)} из ${TestRows.formatPoints(max)}"

  override def display(implicit messages: Messages): String =
    SubmitResult.tr("points.of", message, TestRows.formatPoints(points), TestRows.formatPoints(max))
}

case object BeingRetestedResult extends SubmitResult {
  override val success: Boolean = false

  override val message: String = "Testing in progress..."

  override val stateTag: String = "retesting"

  override def display(implicit messages: Messages): String = SubmitResult.tr("status.retesting", message)
}

object SubmitResult extends Logging {
  private[models] def tr(key: String, fallback: => String, args: Any*)(implicit messages: Messages): String =
    if (messages.isDefinedAt(key)) messages(key, args: _*) else fallback

  def statusMessage(code: Int)(implicit messages: Messages): String =
    tr(s"status.$code", message.getOrElse(code, tr("status.unknown", "Unknown")))

  def toJson(o: SubmitResult)(implicit messages: Messages): JsValue = Json.obj(
    "success" -> o.success,
    "message" -> o.display,
    "state" -> o.stateTag
  )

  /** English/default fallback Messages for tests constructing actors without a Play MessagesApi. */
  val fallbackMessages: Messages =
    new play.api.i18n.DefaultMessagesApi(Map("en" -> Map(
      "admin.rejudge.one" -> "Rejudge",
      "label.teamWithId" -> "Participant {0}",
      "status.running" -> "Running...",
      "status.2" -> "Compilation error",
      "status.retesting" -> "Testing in progress...",
      "points.of" -> "Баллы: {0} из {1}"
    )), new play.api.i18n.DefaultLangs(Seq(play.api.i18n.Lang("en")))).preferred(Seq(play.api.i18n.Lang("en")))

  implicit val submitResultWrites = new Writes[SubmitResult] {
    override def writes(o: SubmitResult): JsValue = Json.obj(
      "success" -> o.success,
      "message" -> o.message,
      "state" -> o.stateTag
    )
  }

  /**
    * The verdict from the submit: waiting, a compile error, or the points it earned of the problem's maximum. Points
    * still NULL on a finished, compiled submit mean its (re)testing is in flight: in progress, as in annotateFinished2.
    */
  def fromSubmit(submit: Submit): SubmitResult =
    if (!submit.finished)
      SubmitWaiting
    else if (!submit.compiled)
      SubmitCompileError
    else
      submit.points.map(SubmitPointsResult(_, submit.maxPoints)).getOrElse(BeingRetestedResult)

  private[models] def annotate2(submit: Submit): SubmitResult = fromSubmit(submit)

  def annotate(db: JdbcBackend#DatabaseDef, submit: Submit)(implicit ec: ExecutionContext): Future[(SubmitResult, Seq[ResultEntry], SubmitStats)] =
    submit.testingId.map { tid =>
      Submits.loadSubmitDetails(db, tid)
    }.getOrElse(Future.successful(Nil)).map { details =>
      val sr = annotate2(submit)
      val st = Submits.getTestingStats(details)
      (sr, details, st)
    }

  def annotateFinished2(db: JdbcBackend#DatabaseDef, finished: FinishedTesting)(implicit ec: ExecutionContext): Future[AnnoSubmit] = {
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    import org.stingray.contester.dbmodel.SlickModel

    val sr =
      if (!finished.compiled)
        Future.successful(SubmitCompileError)
      else
        db.run((for {
          s <- SlickModel.submits if s.id === finished.submit.id.toLong
          p <- SlickModel.problems if p.contestID === s.contest && p.id === s.problem
        } yield (s.points, p.maxPoints)).result.headOption).map {
          case Some((Some(points), max)) => SubmitPointsResult(points, max)
          case _ => BeingRetestedResult
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
    18 -> "не запускался",
    21 -> "Rejected"
  )

  def success(code: Int) =
    code == 10 || code == 2
}

object AnnoSubmit {
  implicit val formatAnnoSubmit = Json.writes[AnnoSubmit]

  def toJson(a: AnnoSubmit)(implicit messages: Messages): JsValue = Json.obj(
    "submitId" -> a.submitId,
    "contest" -> a.contest,
    "team" -> a.team,
    "problem" -> a.problem,
    "result" -> SubmitResult.toJson(a.result)
  )

  implicit val eventNameExtractor = EventNameExtractor[AnnoSubmit](_ => Some("submit"))
  implicit val eventDataExtractor = EventDataExtractor[AnnoSubmit] { c =>
    Json.stringify(Json.toJson(c))
  }
}