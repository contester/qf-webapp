package palevo.query

import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest

import scala.concurrent.Future

/**
  * An in-memory [[QuerySource]] for the controller specs. Everything is given up front; the per-contest maps are
  * keyed by contest id and a missing contest reads as empty. `recentArrivals` filters `arrivals` (contest, problem,
  * time) by `since`, `printJobs` filters `jobs` the way the database source does (newest first, area compared
  * ignoring case). `calls` records the method calls so a spec can check that a forbidden lookup was not made.
  */
class FakeSource(
  contestList: Seq[Contest] = Nil,
  freezingBoards: Map[Int, live.LiveBoard] = Map.empty,
  realBoards: Map[Int, live.LiveBoard] = Map.empty,
  problems: Map[Int, Map[String, String]] = Map.empty,
  submitTimes: Map[Int, Map[Int, DateTime]] = Map.empty,
  arrivals: Seq[(Int, String, DateTime)] = Nil,
  pending: Map[Int, Map[String, Int]] = Map.empty,
  rooms: Map[Int, Map[Int, String]] = Map.empty,
  computerList: Seq[ComputerRow] = Nil,
  jobs: Seq[PrintJobRow] = Nil,
  printers: Seq[PrinterProblem] = Nil,
  counts: Map[Int, TeamCounts] = Map.empty
) extends QuerySource {
  private val log = new java.util.concurrent.ConcurrentLinkedQueue[String]()
  def calls: Seq[String] = { import scala.collection.JavaConverters._; log.asScala.toList }
  private def call[T](name: String)(v: => T): Future[T] = { log.add(name); Future.successful(v) }

  def contests(): Future[Seq[Contest]] = call("contests")(contestList)

  def board(contestId: Int, real: Boolean): Future[Option[live.LiveBoard]] =
    call(s"board($contestId,$real)")((if (real) realBoards else freezingBoards).get(contestId))

  def teamCounts(contestId: Int): Future[TeamCounts] =
    call(s"teamCounts($contestId)")(counts.getOrElse(contestId, TeamCounts(0, 0)))

  def problemNames(contestId: Int): Future[Map[String, String]] =
    call(s"problemNames($contestId)")(problems.getOrElse(contestId, Map.empty))

  def lastSubmits(contestId: Int, teams: Seq[Int]): Future[Map[Int, DateTime]] =
    call(s"lastSubmits($contestId)")(submitTimes.getOrElse(contestId, Map.empty).filter(x => teams.contains(x._1)))

  def recentArrivals(contestId: Int, since: DateTime): Future[Map[String, Int]] =
    call(s"recentArrivals($contestId)")(arrivals.filter(x => x._1 == contestId && !x._3.isBefore(since))
      .groupBy(_._2).map { case (p, xs) => p -> xs.size })

  def pendingCounts(contestId: Int): Future[Map[String, Int]] =
    call(s"pendingCounts($contestId)")(pending.getOrElse(contestId, Map.empty))

  def teamRooms(contestId: Int, teams: Seq[Int]): Future[Map[Int, String]] =
    call(s"teamRooms($contestId)")(rooms.getOrElse(contestId, Map.empty).filter(x => teams.contains(x._1)))

  def computers(): Future[Seq[ComputerRow]] = call("computers")(computerList)

  def printJobs(contests: Seq[Int], team: Option[Int], area: Option[String], limit: Int): Future[Seq[PrintJobRow]] =
    call(s"printJobs")(jobs.filter(j => contests.contains(j.contest) && team.forall(_ == j.team) &&
      area.forall(_.equalsIgnoreCase(j.area))).sortBy(j => (-j.arrived.getMillis, -j.id)).take(math.max(0, limit)))

  def printerProblems(): Seq[PrinterProblem] = { log.add("printerProblems"); printers }
}
