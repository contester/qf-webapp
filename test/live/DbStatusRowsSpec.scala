package live

import java.util.concurrent.atomic.AtomicInteger

import actors.LiveMonitorActor.{DbStatusRows, StatusRow}
import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.TestKit
import com.typesafe.config.ConfigFactory
import models._
import org.joda.time.DateTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.{LocalTeam, Memory, TimeMs}
import org.stingray.qf.actors.TeamStateActor
import org.stingray.qf.models.TeamClient
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * The rows the live status page is pushed. The retry and the overrides run on a fake reader; the rest reads a real
  * database (skipped unless ADMINS_TEST_URL is set, like AdminSubmitsDbSpec; any data will do):
  *   ADMINS_TEST_URL=jdbc:postgresql://127.0.0.1:54329/contest ADMINS_TEST_USER=postgres ADMINS_TEST_PASSWORD=x
  */
class DbStatusRowsSpec extends TestKit(ActorSystem("DbStatusRowsSpec", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private implicit val ec: ExecutionContext = ExecutionContext.global
  private def await[T](f: Future[T]) = Await.result(f, 120.seconds)

  // Answers the team list of any contest with the given teams.
  private def teamClient(teams: LocalTeam*): TeamClient = new TeamClient(system.actorOf(Props(new Actor {
    def receive = { case TeamStateActor.GetTeams(_) => sender() ! teams.map(t => t.id -> t).toMap }
  })))

  private def team(id: Int, school: String = "Lyceum & Co") =
    LocalTeam(id, 1, school, "L", 2, "<Alpha>", notRated = false, noPrint = false, disabled = false)

  private val noTeams = teamClient()

  private def submitRow(id: Int, finished: Boolean, success: Boolean = true, afterFreeze: Boolean = false,
                        teamId: Int = 5): AdminSubmitRow = {
    val s = Submit(SubmitId(id, Arrived(new DateTime(2026, 11, 1, 10, 0), 600, afterFreeze), teamId, 3, RatedProblem("A", 30), "cpp"),
      finished, compiled = finished, passed = if (success) 3 else 1, taken = if (finished) 3 else 0, None)
    AdminSubmitRow(s, None, if (!finished) SubmitWaiting else if (success) SubmitAccepted else SubmitACMPartialResult("Wrong answer", Some(2)),
      SubmitStats(TimeMs(31), Memory(2048)), runs = 1)
  }

  private def rows(read: Int => Future[Option[AdminSubmitRow]], tc: TeamClient = noTeams) =
    new DbStatusRows(read, tc, system.scheduler, retryAfter = 20.millis, retries = 3)

  test("a row is rendered for those who can rejudge and for those who cannot") {
    val r = await(rows(id => Future.successful(Some(submitRow(id, finished = true)))).render(42, None)).get
    assert(r.contest == 3 && r.id == 42 && r.team == 5 && !r.afterFreeze)
    assert(r.rw.startsWith("<tr id=\"result-row-42\"") && r.ro.startsWith("<tr id=\"result-row-42\""))
    assert(r.rw.contains("rejudge-42") && !r.ro.contains("rejudge-42"))
    assert(r.rw.contains("ACCEPTED") && r.ro.contains("ACCEPTED"))
    assert(r.rw.contains("class=\"team-link\"") && r.rw.contains("Team 5"))
  }

  test("the team is named, escaped, from the team list") {
    val r = await(rows(id => Future.successful(Some(submitRow(id, finished = true))), teamClient(team(5))).render(42, None)).get
    assert(r.rw.contains("Lyceum &amp; Co") && !r.rw.contains("Lyceum & Co"), r.rw)
  }

  test("a freeze flag of the submit is carried") {
    val r = await(rows(id => Future.successful(Some(submitRow(id, finished = true, afterFreeze = true)))).render(42, None)).get
    assert(r.afterFreeze)
  }

  test("a submit that is not there has no row") {
    assert(await(rows(_ => Future.successful(None)).render(42, None)).isEmpty)
  }

  test("a fresh submit and a rejudge are shown as being tested whatever the database says") {
    val read = (id: Int) => Future.successful(Some(submitRow(id, finished = true, success = false)))
    val fresh = await(rows(read).render(42, Some(SubmitWaiting))).get
    assert(fresh.rw.contains("Running...") && !fresh.rw.contains("Wrong answer"), fresh.rw)
    val again = await(rows(read).render(42, Some(BeingRetestedResult))).get
    assert(again.rw.contains("Testing in progress...") && !again.rw.contains("Wrong answer"))
  }

  test("a finish heard before its result is visible is read again until it is") {
    val reads = new AtomicInteger
    val r = await(rows(id => Future.successful(Some(submitRow(id, finished = reads.incrementAndGet() >= 3)))).render(42, None)).get
    assert(reads.get == 3)
    assert(r.rw.contains("ACCEPTED") && !r.rw.contains("Running..."))
  }

  test("a result that never shows up is given up on after the retries, and the row is still sent") {
    val reads = new AtomicInteger
    val r = await(rows(id => { reads.incrementAndGet(); Future.successful(Some(submitRow(id, finished = false))) }).render(42, None)).get
    assert(reads.get == 4, "one read and three retries")
    assert(r.rw.contains("Running..."))
  }

  test("an arrival or a rejudge is read once, not retried") {
    val reads = new AtomicInteger
    await(rows(id => { reads.incrementAndGet(); Future.successful(Some(submitRow(id, finished = false))) }).render(42, Some(SubmitWaiting)))
    assert(reads.get == 1)
  }

  test("a failing read is a failed render, not a hang") {
    val f = rows(_ => Future.failed(new RuntimeException("db down"))).render(42, None)
    assert(scala.util.Try(await(f)).isFailure)
  }

  // --- against a database -------------------------------------------------------------------------------------

  private val url = sys.env.get("ADMINS_TEST_URL")

  private lazy val db = DatabaseConfig.forConfig[JdbcProfile]("", ConfigFactory.parseString(
    s"""profile = "org.stingray.contester.dbmodel.MyPostgresProfile$$"
       |db { url = "${url.get}", user = "${sys.env.getOrElse("ADMINS_TEST_USER", "")}",
       |      password = "${sys.env.getOrElse("ADMINS_TEST_PASSWORD", "")}", driver = "org.postgresql.Driver",
       |      connectionPool = disabled }""".stripMargin)).db

  private lazy val contests: Seq[Int] = {
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    await(db.run(org.stingray.contester.dbmodel.SlickModel.allContests.result)).map(_.id)
  }

  private lazy val real = DbStatusRows.fromDb(db, noTeams, system.scheduler)

  test("db: every submit renders, in its own contest, with the freeze flag the page uses") {
    assume(url.isDefined)
    var n = 0
    contests.foreach { c =>
      val shown = await(AdminSubmits.page(db, c, None, None, None, canSeeAll = true, size = 100000)).rows
      val visible = await(AdminSubmits.page(db, c, None, None, None, canSeeAll = false, size = 100000)).rows.map(_.submit.submitId.id).toSet
      shown.foreach { s =>
        val id = s.submit.submitId.id
        val r = await(real.render(id, None)).getOrElse(fail(s"no row for submit $id"))
        assert(r.contest == c && r.id == id && r.team == s.submit.submitId.teamId, s"submit $id")
        assert(r.afterFreeze == !visible(id), s"freeze flag of $id")
        assert(r.rw.startsWith("<tr id=\"result-row-" + id + "\"") && r.rw.endsWith("</tr>"), s"markup of $id")
        assert(r.rw.contains(s.result.message) && r.ro.contains(s.result.message), s"verdict of $id")
        assert(r.rw.contains(s"rejudge-$id") && !r.ro.contains(s"rejudge-$id"), s"rejudge button of $id")
        n += 1
      }
    }
    assert(n > 0)
    info(s"$n submits in ${contests.size} contests")
  }

  test("db: a submit that is not tested yet is shown as running, after the retries") {
    assume(url.isDefined)
    val untested = contests.flatMap(c => await(AdminSubmits.page(db, c, None, None, None, canSeeAll = true, size = 100000)).rows)
      .filterNot(_.submit.finished)
    assume(untested.nonEmpty, "no untested submit in the database")
    val fast = new DbStatusRows(id => AdminSubmits.row(db, id), noTeams, system.scheduler, retryAfter = 10.millis, retries = 2)
    untested.foreach { s =>
      val r = await(fast.render(s.submit.submitId.id, None)).get
      assert(r.rw.contains("Running..."), s.submit.submitId.id.toString)
    }
  }

  test("db: how long a row takes to read (the biggest contest, a team with many attempts on one problem)") {
    assume(url.isDefined)
    val all = contests.flatMap(c => await(AdminSubmits.page(db, c, None, None, None, canSeeAll = true, size = 100000)).rows)
    val worst = all.groupBy(s => (s.submit.submitId.contestId, s.submit.submitId.teamId, s.submit.submitId.problem.id)).maxBy(_._2.size)._2
    await(real.render(worst.last.submit.submitId.id, None)) // warm up
    val t0 = System.nanoTime()
    val reps = 20
    (1 to reps).foreach(_ => await(real.render(worst.last.submit.submitId.id, None)))
    val ms = (System.nanoTime() - t0) / 1e6 / reps
    info(f"$ms%.1f ms per row, a pair with ${worst.size} attempts, ${all.size} submits in all")
    assert(ms < 500)
  }
}
