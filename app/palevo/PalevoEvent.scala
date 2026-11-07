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

  /** The wire format of an instant: UTC, no millis. */
  private[palevo] def iso(d: DateTime): String = tsFormat.print(d.withZone(DateTimeZone.UTC))

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

/** Shared by the three phase events: `inMinutes == 0` means "now"; otherwise the event is about the moment `at`. */
private[palevo] object PhaseEvent {
  def id(eventType: String, contest: PContest, inMinutes: Int, at: Option[DateTime]): String =
    if (inMinutes == 0) s"qf-$eventType-${contest.id}"
    else s"qf-$eventType-${contest.id}-in$inMinutes-${at.get.getMillis}"

  def data(inMinutes: Int, at: Option[DateTime]): JsObject =
    if (inMinutes == 0) Json.obj()
    else Json.obj("in_minutes" -> inMinutes, "at" -> PalevoEvent.iso(at.get))

  def check(inMinutes: Int, at: Option[DateTime]): Unit =
    require(inMinutes >= 0 && (inMinutes == 0 || at.isDefined), s"phase event in $inMinutes minutes needs a target time")
}

final case class ContestStarting(contest: PContest, ts: DateTime, inMinutes: Int = 0,
                                 at: Option[DateTime] = None) extends PalevoEvent {
  PhaseEvent.check(inMinutes, at)
  def id = PhaseEvent.id(eventType, contest, inMinutes, at)
  def eventType = "contest_starting"
  def data = PhaseEvent.data(inMinutes, at)
}

final case class ContestFreezing(contest: PContest, ts: DateTime, inMinutes: Int = 0,
                                 at: Option[DateTime] = None) extends PalevoEvent {
  PhaseEvent.check(inMinutes, at)
  def id = PhaseEvent.id(eventType, contest, inMinutes, at)
  def eventType = "contest_freezing"
  def data = PhaseEvent.data(inMinutes, at)
}

final case class ContestEnding(contest: PContest, ts: DateTime, inMinutes: Int = 0,
                               at: Option[DateTime] = None) extends PalevoEvent {
  PhaseEvent.check(inMinutes, at)
  def id = PhaseEvent.id(eventType, contest, inMinutes, at)
  def eventType = "contest_ending"
  def data = PhaseEvent.data(inMinutes, at)
}

/** One time of a contest that changed: `what` is "start", "freeze" or "end". */
final case class TimeChange(what: String, from: DateTime, to: DateTime)

/** The schedule of a contest was edited; one event per edit, listing every changed time. */
final case class ContestRescheduled(contest: PContest, ts: DateTime, changes: Seq[TimeChange]) extends PalevoEvent {
  def id = s"qf-contest_rescheduled-${contest.id}-${ts.getMillis}"
  def eventType = "contest_rescheduled"
  def data = Json.obj("changes" -> changes.map(c =>
    Json.obj("what" -> c.what, "old" -> PalevoEvent.iso(c.from), "new" -> PalevoEvent.iso(c.to))))
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
