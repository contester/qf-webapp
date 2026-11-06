package palevo

import models.StoredWaiterTask
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{ClarificationRequest, Contest, Team}

/** Pure mappings from qf-webapp's own data to palevo events. */
object EventBuilders {
  def contestOf(c: Contest): PContest = PContest(c.id, c.name)

  def teamOf(t: Team): PTeam = PTeam(t.id, t.teamFullName)

  /** Used when the team cannot be looked up: better a placeholder name than a lost announcement. */
  def teamOrPlaceholder(id: Int, team: Option[Team]): PTeam =
    team.map(teamOf).getOrElse(PTeam(id, s"team $id"))

  def clarificationRequested(req: ClarificationRequest, contest: PContest, team: PTeam, now: DateTime): PalevoEvent =
    ClarificationRequested(req.id, contest, now, team, req.problem, req.request)

  def clarificationPosted(id: Long, contest: PContest, updated: Boolean, problem: String, text: String,
                          now: DateTime): PalevoEvent =
    ClarificationPosted(id, contest, now, updated, Some(problem).filter(_.nonEmpty), text)

  def taskCreated(task: StoredWaiterTask, now: DateTime): PalevoEvent =
    TaskCreated(task.id, now, task.message, task.rooms.toSeq.sorted)

  def printerProblem(printer: String, areas: Seq[String], open: Boolean, condition: String, waitingJobs: Int,
                     now: DateTime): PalevoEvent =
    PrinterProblem(printer, areas.sorted, open, condition, waitingJobs, now)

  def printError(jobId: Long, contest: PContest, team: Option[PTeam], error: String, now: DateTime): PalevoEvent =
    PrintError(jobId, contest, now, team, error)
}
