package palevo.query

import actors.LiveMonitorActor.{JoinAdmin, JoinPublic, Snapshot}
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.joda.time.DateTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.Problem
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * The parts of [[DbQuerySource]] that need no database (live boards, problem names, printer problems, the shape of
  * the SQL) always run. The queries themselves are checked against a copy of a contest database only when
  * ADMINS_TEST_URL is set (see models.TeamProblemSubmitsDbSpec for how to start one); each expectation is computed
  * here independently from the raw rows.
  */
class DbQuerySourceSpec extends TestKit(ActorSystem("DbQuerySourceSpec", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |akka.use-slf4j = off
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private implicit val ec: ExecutionContext = system.dispatcher
  private def await[T](f: Future[T]) = Await.result(f, 120.seconds)

  private def source(monitor: Option[ActorRef] = None,
                     problems: Int => Future[Seq[Problem]] = _ => Future.successful(Nil),
                     alerts: () => Seq[(String, String, String)] = () => Nil,
                     db: slick.jdbc.JdbcBackend#DatabaseDef = null) =
    new DbQuerySource(db, monitor, problems, alerts)

  private val emptyBoard = live.LiveBoard(Seq("A"), Vector.empty, Map("A" -> 0))
  private val realBoard = live.LiveBoard(Seq("A", "B"), Vector.empty, Map("A" -> 1, "B" -> 0))

  private def boardProbe(): TestProbe = {
    val p = TestProbe()
    p.setAutoPilot((sender: ActorRef, msg: Any) => {
      msg match {
        case Snapshot(JoinPublic(7)) => sender ! emptyBoard
        case Snapshot(JoinAdmin(7, true)) => sender ! realBoard
        case _ => // no answer: the ask times out
      }
      akka.testkit.TestActor.KeepRunning
    })
    p
  }

  // --- without a database ---

  test("board asks the live monitor for the freezing board, or the real one") {
    val s = source(monitor = Some(boardProbe().ref))
    assert(await(s.board(7, real = false)).contains(emptyBoard))
    assert(await(s.board(7, real = true)).contains(realBoard))
  }

  test("board is None when the live monitor is disabled or does not answer") {
    assert(await(source().board(7, real = false)).isEmpty)
    assert(await(source(monitor = Some(boardProbe().ref)).board(8, real = true)).isEmpty)
  }

  test("problemNames maps problem ids to names; a failed lookup gives an empty map") {
    val s = source(problems = c => Future.successful(Seq(Problem(c, "A", "Apples", 10), Problem(c, "B", "Bees", 5))))
    assert(await(s.problemNames(3)) == Map("A" -> "Apples", "B" -> "Bees"))
    assert(await(source(problems = _ => Future.failed(new RuntimeException("x"))).problemNames(3)).isEmpty)
  }

  test("printerProblems reads the current printer alerts") {
    val s = source(alerts = () => Seq(("agent1", "hp1", "paper out")))
    assert(s.printerProblems() == Seq(PrinterProblem("agent1", "hp1", "paper out")))
  }

  test("empty team or contest lists need no query") {
    val s = source() // a null database: any query would throw
    assert(await(s.lastSubmits(1, Nil)).isEmpty)
    assert(await(s.teamRooms(1, Nil)).isEmpty)
    assert(await(s.printJobs(Nil, None, None, 10)).isEmpty)
    assert(await(s.printJobs(Seq(1), None, None, 0)).isEmpty)
  }

  private def sqlOf(a: slick.dbio.DBIOAction[_, _, _]): String = a match {
    case s: slick.sql.SqlAction[_, _, _] => s.statements.mkString(" ").replaceAll("\\s+", " ").toLowerCase
    case other => fail(s"not a plain SQL action: $other")
  }

  test("every contest query is bounded by contest and a limit, and never reads file or source bytes") {
    import DbQuerySource.Queries._
    val since = new DateTime(2026, 11, 6, 12, 0)
    val all = Seq(
      lastSubmits(3, Seq(1, 2)), recentArrivals(3, since), pendingCounts(3), teamRooms(3, Seq(5, 6)),
      printJobs(Seq(3, 4), Some(5), Some("1234"), 10), teamCounts(3))
    for (q <- all.map(sqlOf)) {
      assert(q.contains(" limit "), q)
      assert(q.contains("contest = ") || q.contains("contest in ("), q)
      assert(!q.matches(""".*\b(p|s)\.(data|source)\b.*"""), q)
      assert(!q.contains("select *"), q)
    }
    assert(sqlOf(computers).contains(" limit "))
  }

  test("team, contest and limit values are spliced only as numbers") {
    import DbQuerySource.Queries._
    assert(sqlOf(lastSubmits(3, Seq(1, 2, 2))).contains("team_id in (1,2)"))
    assert(sqlOf(teamRooms(3, Seq(5, 6))).contains("array[5,6]"))
    val pj = sqlOf(printJobs(Seq(3, 4), None, None, 10))
    assert(pj.contains("p.contest in (3,4)"))
    assert(pj.contains("limit 10"))
    assert(sqlOf(printJobs(Seq(3), None, None, 100000)).contains(s"limit ${DbQuerySource.MaxPrintJobs}"))
  }

  test("teamCounts is one count over the contest's participants, the contest bound as a parameter") {
    val q = sqlOf(DbQuerySource.Queries.teamCounts(3))
    assert(q.contains("from participants p"), q)
    assert(q.contains("p.contest = ?"), q)
    assert(!q.contains("3"), q)
    assert(q.contains("not_rated"), q)
    assert(!q.contains("group by"), q)
  }

  test("pending means untested, so the partial index on untested submits applies") {
    assert(sqlOf(DbQuerySource.Queries.pendingCounts(3)).contains("not s.tested"))
  }

  // --- against a contest database (skipped unless ADMINS_TEST_URL is set) ---

  private val url = sys.env.get("ADMINS_TEST_URL")

  private lazy val dbConfig = DatabaseConfig.forConfig[JdbcProfile]("", ConfigFactory.parseString(
    s"""profile = "org.stingray.contester.dbmodel.MyPostgresProfile$$"
       |db { url = "${url.get}", user = "${sys.env.getOrElse("ADMINS_TEST_USER", "")}",
       |      password = "${sys.env.getOrElse("ADMINS_TEST_PASSWORD", "")}", driver = "org.postgresql.Driver",
       |      connectionPool = disabled }""".stripMargin))
  private lazy val db = dbConfig.db
  private lazy val dbSource = source(db = db)

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._

  private case class RawSubmit(contest: Int, team: Int, problem: String, id: Long, at: DateTime, tested: Boolean,
                               computer: Option[String])

  // Every submit, without its source.
  private lazy val rawSubmits: Seq[RawSubmit] = await(db.run(
    sql"""select contest, team_id, problem, id, submit_time_absolute, tested, host(computer_id) from submits"""
      .as[(Int, Int, String, Long, java.sql.Timestamp, Boolean, Option[String])]))
    .map(x => RawSubmit(x._1, x._2, x._3, x._4, new DateTime(x._5.getTime), x._6, x._7))

  private case class RawJob(id: Long, contest: Int, team: Int, computer: String, arrived: DateTime, status: String,
                            error: String, pages: Int)

  private lazy val rawJobs: Seq[RawJob] = await(db.run(
    sql"""select j.id, j.contest, j.team, host(j.computer_id), j.arrived, s.status, s.error, s.pages
          from print_jobs j join print_job_state s on s.job_id = j.id"""
      .as[(Long, Int, Int, String, java.sql.Timestamp, String, String, Int)]))
    .map(x => RawJob(x._1, x._2, x._3, x._4, new DateTime(x._5.getTime), x._6, x._7, x._8))

  // computer address -> (computer name, area name, printer)
  private lazy val located: Map[String, (String, String, String)] = await(db.run(
    sql"""select host(l.id), l.name, a.name, a.printer from computer_locations l join areas a on a.id = l.location"""
      .as[(String, String, String, String)])).map(x => x._1 -> (x._2, x._3, x._4)).toMap

  private lazy val busiest: Int = {
    assume(rawSubmits.nonEmpty, "the database has no submits")
    rawSubmits.groupBy(_.contest).maxBy(_._2.size)._1
  }

  test("lastSubmits returns each team's latest submit time") {
    assume(url.isDefined)
    val inContest = rawSubmits.filter(_.contest == busiest)
    val teams = inContest.map(_.team).distinct
    val expected = inContest.groupBy(_.team).map { case (t, xs) => t -> xs.map(_.at.getMillis).max }
    val got = await(dbSource.lastSubmits(busiest, teams :+ 999999))
    assert(got.map { case (t, d) => t -> d.getMillis } == expected)
  }

  test("recentArrivals counts submits per problem since the given time, whatever their outcome") {
    assume(url.isDefined)
    val inContest = rawSubmits.filter(_.contest == busiest)
    for (since <- Seq(inContest.map(_.at).minBy(_.getMillis), inContest.map(_.at).maxBy(_.getMillis).minusHours(1),
                      inContest.map(_.at).maxBy(_.getMillis).plusSeconds(1))) {
      val expected = inContest.filter(!_.at.isBefore(since)).groupBy(_.problem).map { case (p, xs) => p -> xs.size }
      assert(await(dbSource.recentArrivals(busiest, since)) == expected, s"since $since")
    }
  }

  test("pendingCounts counts untested submits per problem") {
    assume(url.isDefined)
    for (contest <- rawSubmits.map(_.contest).distinct) {
      val expected = rawSubmits.filter(x => x.contest == contest && !x.tested).groupBy(_.problem)
        .map { case (p, xs) => p -> xs.size }
      assert(await(dbSource.pendingCounts(contest)) == expected, s"contest $contest")
    }
  }

  test("teamRooms: the area of the latest located submit, else of the latest located print job") {
    assume(url.isDefined)
    def areaOf(computer: Option[String]) = computer.flatMap(located.get).map(_._2)
    var checked = 0
    for (contest <- (rawSubmits.map(_.contest) ++ rawJobs.map(_.contest)).distinct) {
      val teams = (rawSubmits.filter(_.contest == contest).map(_.team) ++ rawJobs.filter(_.contest == contest).map(_.team)).distinct
      val expected = teams.flatMap { t =>
        val bySubmit = rawSubmits.filter(x => x.contest == contest && x.team == t && areaOf(x.computer).isDefined)
          .sortBy(x => (-x.at.getMillis, -x.id)).headOption.flatMap(x => areaOf(x.computer))
        val byJob = rawJobs.filter(x => x.contest == contest && x.team == t && areaOf(Some(x.computer)).isDefined)
          .sortBy(x => (-x.arrived.getMillis, -x.id)).headOption.flatMap(x => areaOf(Some(x.computer)))
        bySubmit.orElse(byJob).map(t -> _)
      }.toMap
      assert(await(dbSource.teamRooms(contest, teams)) == expected, s"contest $contest")
      checked += expected.size
    }
    info(s"$checked teams located")
  }

  test("computers lists every computer with its area and the area's printer") {
    assume(url.isDefined)
    val got = await(dbSource.computers())
    assert(got.map(c => (c.name, c.area, c.printer)).sorted == located.values.toSeq.sorted)
  }

  test("printJobs honours contests, team, area and limit, newest first") {
    assume(url.isDefined)
    assume(rawJobs.nonEmpty, "the database has no print jobs")
    def areaOf(j: RawJob) = located.get(j.computer).map(_._2).getOrElse("")
    def ids(rows: Seq[PrintJobRow]) = rows.map(_.id.toLong)
    def newest(js: Seq[RawJob], n: Int) = js.sortBy(x => (-x.arrived.getMillis, -x.id)).take(n).map(_.id)

    val contest = rawJobs.groupBy(_.contest).maxBy(_._2.size)._1
    val inContest = rawJobs.filter(_.contest == contest)
    assert(ids(await(dbSource.printJobs(Seq(contest), None, None, 10))) == newest(inContest, 10))
    assert(ids(await(dbSource.printJobs(Seq(contest), None, None, 3))) == newest(inContest, 3))

    val team = inContest.head.team
    assert(ids(await(dbSource.printJobs(Seq(contest), Some(team), None, 10))) == newest(inContest.filter(_.team == team), 10))

    val area = areaOf(inContest.head)
    assume(area.nonEmpty)
    assert(ids(await(dbSource.printJobs(Seq(contest), None, Some(area), 10))) ==
      newest(inContest.filter(areaOf(_) == area), 10))
    assert(ids(await(dbSource.printJobs(Seq(contest), None, Some(area.toUpperCase), 10))) ==
      newest(inContest.filter(areaOf(_) == area), 10))

    assert(await(dbSource.printJobs(Seq(999999), None, None, 10)).isEmpty)

    val row = await(dbSource.printJobs(Seq(contest), None, None, 1)).head
    val raw = inContest.find(_.id == row.id).get
    assert(row == PrintJobRow(raw.id.toInt, raw.contest, raw.team, located.get(raw.computer).map(_._1).getOrElse(raw.computer),
      areaOf(raw), raw.status == "printed", raw.error, raw.pages, row.arrived))
    assert(row.arrived.getMillis == raw.arrived.getMillis)
  }

  test("teamCounts splits each contest's participants by the not-rated flag") {
    assume(url.isDefined)
    val raw = await(db.run(sql"""select contest, not_rated from participants""".as[(Int, Boolean)]))
    assume(raw.nonEmpty, "the database has no participants")
    for ((contest, rows) <- raw.groupBy(_._1)) {
      val out = rows.count(_._2)
      assert(await(dbSource.teamCounts(contest)) == TeamCounts(rows.size - out, out), s"contest $contest")
    }
    info(s"${raw.groupBy(_._1).size} contests checked, ${raw.count(_._2)} not-rated participants")
    assert(await(dbSource.teamCounts(999999)) == TeamCounts(0, 0))
  }

  test("contests lists every contest") {
    assume(url.isDefined)
    assert(await(dbSource.contests()).map(_.id).sorted ==
      await(db.run(org.stingray.contester.dbmodel.SlickModel.allContests.result)).map(_.id).sorted)
  }
}
