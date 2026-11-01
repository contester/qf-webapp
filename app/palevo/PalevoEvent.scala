package palevo

import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.ISODateTimeFormat
import play.api.libs.json._

final case class PContest(id: Int, name: String)
final case class PTeam(id: Int, name: String)

/** A structured event for palevo's webhook (see palevo's design doc). Events carry data only; palevo renders the text. */
sealed trait PalevoEvent {
  def id: String
  def eventType: String
  def contest: PContest
  def ts: DateTime
  def data: JsObject
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
  )

  private[palevo] def team(t: PTeam): JsValue = Json.toJson(t)
}

/** Events that have no contest (waiter tasks) use this placeholder. */
object NoContest {
  val value = PContest(0, "")
}

final case class ClarificationRequested(requestId: Long, contest: PContest, ts: DateTime, team: PTeam,
                                        problem: String, text: String) extends PalevoEvent {
  def id = s"qf-clr-req-$requestId"
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
  def id = s"qf-task-$taskId"
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
                                   elapsedSeconds: Int) extends PalevoEvent {
  def id = s"qf-first-contest-${contest.id}"
  def eventType = "first_solve_contest"
  def data = Json.obj("team" -> PalevoEvent.team(team), "problem" -> problem, "elapsed_seconds" -> elapsedSeconds)
}

final case class FirstSolveProblem(contest: PContest, ts: DateTime, team: PTeam, problem: String,
                                   elapsedSeconds: Int) extends PalevoEvent {
  def id = s"qf-first-problem-${contest.id}-$problem"
  def eventType = "first_solve_problem"
  def data = Json.obj("team" -> PalevoEvent.team(team), "problem" -> problem, "elapsed_seconds" -> elapsedSeconds)
}

final case class NewLeader(contest: PContest, ts: DateTime, team: PTeam, solved: Int) extends PalevoEvent {
  def id = s"qf-leader-${contest.id}-${team.id}-$solved"
  def eventType = "new_leader"
  def data = Json.obj("team" -> PalevoEvent.team(team), "solved" -> solved)
}

final case class PrintError(jobId: Long, contest: PContest, ts: DateTime, team: Option[PTeam],
                            error: String) extends PalevoEvent {
  def id = s"qf-print-$jobId"
  def eventType = "print_error"
  def data = Json.obj("error" -> error) ++ team.fold(Json.obj())(t => Json.obj("team" -> PalevoEvent.team(t)))
}
