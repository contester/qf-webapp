package palevo

import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.ISODateTimeFormat
import play.api.libs.json._

/** Who may see an event. Jury events carry real-state information (e.g. standings during a freeze). */
sealed trait Audience
case object PublicAudience extends Audience
case object JuryAudience extends Audience

final case class PContest(id: Int, name: String)
final case class PTeam(id: Int, name: String)

/** A structured event for palevo's webhook (see palevo's design doc). Events carry data only; palevo renders the text. */
sealed trait PalevoEvent {
  def id: String
  def eventType: String
  def contest: PContest
  def ts: DateTime
  def data: JsObject
  def audience: Audience = PublicAudience
}

object PalevoEvent {
  private implicit val teamWrites: OWrites[PTeam] = Json.writes[PTeam]

  private val tsFormat = ISODateTimeFormat.dateTimeNoMillis().withZoneUTC()

  def toJson(e: PalevoEvent): JsValue = Json.obj(
    "id" -> e.id,
    "type" -> e.eventType,
    "ts" -> tsFormat.print(e.ts.withZone(DateTimeZone.UTC)),
    "contest" -> Json.obj("id" -> e.contest.id, "name" -> e.contest.name),
    "data" -> e.data
  ) ++ (e.audience match {
    case JuryAudience => Json.obj("audience" -> "jury")
    case PublicAudience => Json.obj()
  })

  private[palevo] def team(t: PTeam): JsValue = Json.toJson(t)

  /** Jury ids get a prefix so they can never collide with the public id of the same fact. */
  private[palevo] def idPrefix(a: Audience): String = a match {
    case JuryAudience => "qf-jury-"
    case PublicAudience => "qf-"
  }
}

/** Events that have no contest (waiter tasks) use this placeholder. */
object NoContest {
  val value = PContest(0, "")
}

final case class ClarificationRequested(requestId: Long, contest: PContest, ts: DateTime, team: PTeam,
                                        problem: String, text: String) extends PalevoEvent {
  def id = s"qf-clr-req-$requestId-${ts.getMillis}"
  def eventType = "clarification_requested"
  def data = Json.obj("team" -> PalevoEvent.team(team), "problem" -> problem, "text" -> text)
}

final case class ClarificationPosted(clarificationId: Long, contest: PContest, ts: DateTime, updated: Boolean,
                                     problem: Option[String], text: String) extends PalevoEvent {
  def id = s"qf-clr-$clarificationId-${ts.getMillis}"
  def eventType = "clarification_posted"
  def data = Json.obj("text" -> text, "updated" -> updated) ++
    problem.fold(Json.obj())(p => Json.obj("problem" -> p))
}

final case class TaskCreated(taskId: Long, ts: DateTime, text: String, rooms: Seq[String]) extends PalevoEvent {
  def id = s"qf-task-$taskId-${ts.getMillis}"
  def eventType = "task_created"
  def contest = NoContest.value
  def data = Json.obj("text" -> text, "rooms" -> rooms)
}

final case class ContestStarted(contest: PContest, ts: DateTime) extends PalevoEvent {
  def id = s"qf-contest_started-${contest.id}"
  def eventType = "contest_started"
  def data = Json.obj()
}

final case class ContestFrozen(contest: PContest, ts: DateTime) extends PalevoEvent {
  def id = s"qf-contest_frozen-${contest.id}"
  def eventType = "contest_frozen"
  def data = Json.obj()
}

final case class ContestFinished(contest: PContest, ts: DateTime) extends PalevoEvent {
  def id = s"qf-contest_finished-${contest.id}"
  def eventType = "contest_finished"
  def data = Json.obj()
}

final case class FirstSolveContest(contest: PContest, ts: DateTime, team: PTeam, problem: String,
                                   elapsedSeconds: Int, override val audience: Audience = PublicAudience) extends PalevoEvent {
  def id = s"${PalevoEvent.idPrefix(audience)}first-contest-${contest.id}"
  def eventType = "first_solve_contest"
  def data = Json.obj("team" -> PalevoEvent.team(team), "problem" -> problem, "elapsed_seconds" -> elapsedSeconds)
}

final case class FirstSolveProblem(contest: PContest, ts: DateTime, team: PTeam, problem: String,
                                   elapsedSeconds: Int, override val audience: Audience = PublicAudience) extends PalevoEvent {
  def id = s"${PalevoEvent.idPrefix(audience)}first-problem-${contest.id}-$problem"
  def eventType = "first_solve_problem"
  def data = Json.obj("team" -> PalevoEvent.team(team), "problem" -> problem, "elapsed_seconds" -> elapsedSeconds)
}

final case class NewLeader(contest: PContest, ts: DateTime, team: PTeam, solved: Int,
                           override val audience: Audience = PublicAudience) extends PalevoEvent {
  def id = s"${PalevoEvent.idPrefix(audience)}leader-${contest.id}-${team.id}-$solved"
  def eventType = "new_leader"
  def data = Json.obj("team" -> PalevoEvent.team(team), "solved" -> solved)
}

/** A printer cannot print (open) or works again. About a printer, not a contest, so like a waiter task it goes to every
  * route, public ones included. */
final case class PrinterProblem(printer: String, areas: Seq[String], open: Boolean, condition: String,
                                waitingJobs: Int, ts: DateTime) extends PalevoEvent {
  def id = s"qf-printer-$printer-${ts.getMillis}-${if (open) "open" else "cleared"}"
  def eventType = "printer_problem"
  def contest = NoContest.value
  def data = Json.obj("printer" -> printer, "areas" -> areas, "state" -> (if (open) "open" else "cleared")) ++
    (if (open) Json.obj("condition" -> condition, "waiting_jobs" -> waitingJobs) else Json.obj())
}

final case class PrintError(jobId: Long, contest: PContest, ts: DateTime, team: Option[PTeam],
                            error: String) extends PalevoEvent {
  def id = s"qf-print-$jobId-${ts.getMillis}"
  def eventType = "print_error"
  def data = Json.obj("error" -> error) ++ team.fold(Json.obj())(t => Json.obj("team" -> PalevoEvent.team(t)))
}
