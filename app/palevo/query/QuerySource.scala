package palevo.query

import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest

import scala.concurrent.Future

/** A computer with the name and printer of its area (room). */
case class ComputerRow(name: String, area: String, printer: String)

/** A print job without its file. `computer` is the computer's name (its address when it is not in the location
  * table), `area` the area's name (empty when unknown); `printed` and `error` come from the job's latest attempt. */
case class PrintJobRow(id: Int, contest: Int, team: Int, computer: String, area: String, printed: Boolean, error: String,
                       pages: Int, arrived: DateTime)

/** A printer problem reported by a print agent and not yet cleared. */
case class PrinterProblem(agent: String, printer: String, condition: String)

/** Where the query API reads contest data from. Every method is read-only and bounded. */
trait QuerySource {
  def contests(): Future[Seq[Contest]]
  /** The live monitor's freezing board (`real = false`) or real board; None when it is off, slow or has no such contest. */
  def board(contestId: Int, real: Boolean): Future[Option[live.LiveBoard]]
  /** Problem id -> name. */
  def problemNames(contestId: Int): Future[Map[String, String]]
  /** Team -> time of its latest submit, for the given teams that have one. */
  def lastSubmits(contestId: Int, teams: Seq[Int]): Future[Map[Int, DateTime]]
  /** Problem -> number of submits that arrived at or after `since`, whatever their outcome. */
  def recentArrivals(contestId: Int, since: DateTime): Future[Map[String, Int]]
  /** Problem -> number of submits not yet tested. */
  def pendingCounts(contestId: Int): Future[Map[String, Int]]
  /** Team -> area name, for the given teams that can be located (see the design's "Team -> room"). */
  def teamRooms(contestId: Int, teams: Seq[Int]): Future[Map[Int, String]]
  def computers(): Future[Seq[ComputerRow]]
  /** The newest print jobs of the contests, optionally of one team and/or one area (name, any case). */
  def printJobs(contests: Seq[Int], team: Option[Int], area: Option[String], limit: Int): Future[Seq[PrintJobRow]]
  def printerProblems(): Seq[PrinterProblem]
}
