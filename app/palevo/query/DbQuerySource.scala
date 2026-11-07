package palevo.query

import akka.actor.ActorRef
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{Contest, Problem}
import slick.jdbc.{GetResult, JdbcBackend}

import scala.concurrent.{ExecutionContext, Future}

/**
  * The query API's data: boards from the live monitor, problem names from the problem state actor, printer problems
  * from the printing model's alert state and everything else read straight from the contest database. Every query is
  * bounded by its contest (computers and areas are global) and by a LIMIT, and none reads file or source bytes.
  *
  * @param problems the problems of a contest (the problem state actor's view)
  * @param alerts the open printer problems as (agent, printer, condition)
  */
class DbQuerySource(db: JdbcBackend#DatabaseDef, liveMonitor: Option[ActorRef],
                    problems: Int => Future[Seq[Problem]], alerts: () => Seq[(String, String, String)])
                   (implicit ec: ExecutionContext) extends QuerySource {
  import DbQuerySource.Queries
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._

  def contests(): Future[Seq[Contest]] =
    db.run(org.stingray.contester.dbmodel.SlickModel.allContests.result)

  def board(contestId: Int, real: Boolean): Future[Option[live.LiveBoard]] = {
    import actors.LiveMonitorActor.JoinPublic
    controllers.LiveStreams.snapshot(liveMonitor, if (real) controllers.LiveStreams.adminJoin(contestId, canSeeAll = true) else JoinPublic(contestId))
  }

  def problemNames(contestId: Int): Future[Map[String, String]] =
    problems(contestId).map(_.map(p => p.id -> p.name).toMap).recover { case _ => Map.empty }

  def lastSubmits(contestId: Int, teams: Seq[Int]): Future[Map[Int, DateTime]] =
    if (teams.isEmpty) Future.successful(Map.empty)
    else db.run(Queries.lastSubmits(contestId, teams)).map(_.toMap)

  def recentArrivals(contestId: Int, since: DateTime): Future[Map[String, Int]] =
    db.run(Queries.recentArrivals(contestId, since)).map(_.toMap)

  def pendingCounts(contestId: Int): Future[Map[String, Int]] =
    db.run(Queries.pendingCounts(contestId)).map(_.toMap)

  def teamRooms(contestId: Int, teams: Seq[Int]): Future[Map[Int, String]] =
    if (teams.isEmpty) Future.successful(Map.empty)
    else db.run(Queries.teamRooms(contestId, teams)).map(_.collect { case (t, Some(area)) => t -> area }.toMap)

  def computers(): Future[Seq[ComputerRow]] =
    db.run(Queries.computers).map(_.map(ComputerRow.tupled))

  def printJobs(contests: Seq[Int], team: Option[Int], area: Option[String], limit: Int): Future[Seq[PrintJobRow]] =
    if (contests.isEmpty || limit <= 0) Future.successful(Nil)
    else db.run(Queries.printJobs(contests, team, area, limit))

  def printerProblems(): Seq[PrinterProblem] = alerts().map(PrinterProblem.tupled)
}

object DbQuerySource {
  /** Upper bounds on the rows any one query returns. */
  val MaxProblems = 200
  val MaxComputers = 5000
  val MaxPrintJobs = 100

  /** The SQL, kept apart so its shape can be checked without a database. Only Ints are spliced (`#$`); text is bound. */
  object Queries {
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._

    private def time(r: slick.jdbc.PositionedResult) = new DateTime(r.nextTimestamp().getTime)
    private implicit val getTeamTime: GetResult[(Int, DateTime)] = GetResult(r => (r.nextInt(), time(r)))
    private implicit val getJob: GetResult[PrintJobRow] = GetResult(r => PrintJobRow(r.nextLong().toInt, r.nextInt(),
      r.nextInt(), r.nextString(), r.nextString(), r.nextBoolean(), r.nextString(), r.nextInt(), time(r)))

    private def ints(xs: Seq[Int]) = xs.distinct.mkString(",")

    def lastSubmits(contest: Int, teams: Seq[Int]) =
      sql"""select s.team_id, max(s.submit_time_absolute) from submits s
            where s.contest = $contest and s.team_id in (#${ints(teams)})
            group by s.team_id limit #${teams.distinct.size}""".as[(Int, DateTime)]

    def recentArrivals(contest: Int, since: DateTime) = {
      val from = new java.sql.Timestamp(since.getMillis)
      sql"""select s.problem, count(*)::int from submits s
            where s.contest = $contest and s.submit_time_absolute >= $from
            group by s.problem limit #$MaxProblems""".as[(String, Int)]
    }

    def pendingCounts(contest: Int) =
      sql"""select s.problem, count(*)::int from submits s
            where s.contest = $contest and not s.tested
            group by s.problem limit #$MaxProblems""".as[(String, Int)]

    /** The area of the computer of the team's latest submit whose computer is known, else that of its latest
      * print job whose computer is known. */
    def teamRooms(contest: Int, teams: Seq[Int]) =
      sql"""select t.team, coalesce(
              (select a.name from submits s
                 join computer_locations l on l.id = s.computer_id join areas a on a.id = l.location
               where s.contest = $contest and s.team_id = t.team
               order by s.submit_time_absolute desc, s.id desc limit 1),
              (select a.name from print_jobs p
                 join computer_locations l on l.id = p.computer_id join areas a on a.id = l.location
               where p.contest = $contest and p.team = t.team
               order by p.arrived desc, p.id desc limit 1))
            from unnest(array[#${ints(teams)}]::int[]) as t(team) limit #${teams.distinct.size}""".as[(Int, Option[String])]

    val computers = {
      import org.stingray.contester.dbmodel.SlickModel.{areas, compLocations}
      (for {
        l <- compLocations
        a <- areas if a.id === l.location
      } yield (l.name, a.name, a.printer)).sortBy(x => (x._2, x._1)).take(MaxComputers).result
    }

    /** `printed` means the latest attempt printed; a job from a computer missing from the location table keeps its
      * address as the computer and an empty area (and so never matches an area filter). */
    def printJobs(contests: Seq[Int], team: Option[Int], area: Option[String], limit: Int) = {
      val teamCond = team.fold("true")(t => s"p.team = $t")
      val anyArea = area.isEmpty
      val areaName = area.getOrElse("")
      val n = math.min(math.max(limit, 0), MaxPrintJobs)
      sql"""select p.id, p.contest, p.team, coalesce(l.name, host(p.computer_id)), coalesce(a.name, ''),
                   st.status = 'printed', st.error, st.pages, p.arrived
            from print_jobs p
              join print_job_state st on st.job_id = p.id
              left join computer_locations l on l.id = p.computer_id
              left join areas a on a.id = l.location
            where p.contest in (#${ints(contests)}) and #$teamCond and (#$anyArea or lower(a.name) = lower($areaName))
            order by p.arrived desc, p.id desc limit #$n""".as[PrintJobRow]
    }
  }
}
