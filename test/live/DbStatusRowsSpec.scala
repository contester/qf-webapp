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
import org.stingray.contester.dbmodel.{Memory, Team, TimeMs}
import org.stingray.qf.actors.TeamStateActor
import org.stingray.qf.models.TeamClient

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * The rows the live status page is pushed. The retry and the overrides run on a fake reader; the rest reads a
  * fixture round in the teacher-web schema (skipped unless QF_TEST_URL is set, see SubmitInsertDbSpec).
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
  private def teamClient(teams: Team*): TeamClient = new TeamClient(system.actorOf(Props(new Actor {
    def receive = { case TeamStateActor.GetTeams(_) => sender() ! teams.map(t => t.id -> t).toMap }
  })))

  private def team(id: Int, school: String = "Lyceum & Co") =
    Team(id, 2, "<Alpha>", school, "City", None)

  private val noTeams = teamClient()

  private def submitRow(id: Int, finished: Boolean, success: Boolean = true,
                        teamId: Int = 5): AdminSubmitRow = {
    val s = Submit(SubmitId(id, Arrived(new DateTime(2026, 11, 1, 10, 0), 600), teamId, 3, RatedProblem("A", 30), "cpp"),
      finished, compiled = finished, passed = if (success) 3 else 1, taken = if (finished) 3 else 0, None)
    AdminSubmitRow(s, if (!finished) SubmitWaiting else if (success) SubmitPointsResult(3, 3) else SubmitPointsResult(1, 3),
      SubmitStats(TimeMs(31), Memory(2048)), runs = 1)
  }

  private def rows(read: Int => Future[Option[AdminSubmitRow]], tc: TeamClient = noTeams) =
    new DbStatusRows(read, tc, system.scheduler, retryAfter = 20.millis, retries = 3)

  test("a row is rendered for those who can rejudge and for those who cannot") {
    val r = await(rows(id => Future.successful(Some(submitRow(id, finished = true)))).render(42, None)).get
    assert(r.contest == 3 && r.id == 42 && r.team == 5)
    assert(r.rw.startsWith("<tr id=\"result-row-42\"") && r.ro.startsWith("<tr id=\"result-row-42\""))
    assert(r.rw.contains("rejudge-42") && !r.ro.contains("rejudge-42"))
    assert(r.rw.contains("3 из 3") && r.ro.contains("3 из 3"))
    assert(r.rw.contains("class=\"team-link\"") && r.rw.contains("Team 5"))
  }

  test("the team is named, escaped, from the team list") {
    val r = await(rows(id => Future.successful(Some(submitRow(id, finished = true))), teamClient(team(5))).render(42, None)).get
    assert(r.rw.contains("Lyceum &amp; Co") && !r.rw.contains("Lyceum & Co"), r.rw)
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
    assert(r.rw.contains("3 из 3") && !r.rw.contains("Running..."))
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

  test("Russian messages localize pushed rows, running states and fallback team names") {
    val src = scala.io.Source.fromFile("conf/messages.ru", "UTF-8")
    val ruMap = try src.getLines().map(_.trim).filter(l => l.contains("=") && !l.startsWith("#"))
      .map { l => val i = l.indexOf('='); l.substring(0, i).trim -> l.substring(i + 1).trim }.toMap
    finally src.close()
    val ru = play.api.i18n.MessagesImpl(play.api.i18n.Lang("ru"),
      new play.api.i18n.DefaultMessagesApi(Map("ru" -> ruMap), new play.api.i18n.DefaultLangs(Seq(play.api.i18n.Lang("ru")))))
    val r = new DbStatusRows(id => Future.successful(Some(submitRow(id, finished = true))), noTeams, system.scheduler,
      retryAfter = 20.millis, retries = 3, messages = ru)
    val done = await(r.render(42, None)).get
    assert(done.rw.contains("Перепроверить") && done.rw.contains("Команда 5") && done.rw.contains("3 из 3"))
    assert(!done.rw.contains(">Rejudge<") && !done.rw.contains("Team 5"))
    val wait = await(r.render(42, Some(SubmitWaiting))).get
    assert(wait.rw.contains("Выполняется..."))
    val retest = await(r.render(42, Some(BeingRetestedResult))).get
    assert(retest.rw.contains("Идёт тестирование..."))
  }

  // --- against a database: a fixture round of its own (skipped unless QF_TEST_URL is set) -----------------------

  private val fx = new models.ContestFixture("Status rows spec")

  private def realRows = DbStatusRows.fromDb(fx.db, noTeams, system.scheduler)

  test("db: every submit renders, in its own contest") {
    assume(fx.url.isDefined)
    fx.withFixture {
      val done = fx.insertSubmit()
      fx.finishSubmit(done, passed = 2)
      fx.insertSubmit()
      val shown = await(AdminSubmits.page(fx.db, fx.round, None, None, None, size = 100000)).rows
      assert(shown.size == 2)
      val real = realRows
      shown.foreach { s =>
        val id = s.submit.submitId.id
        val r = await(real.render(id, None)).getOrElse(fail(s"no row for submit $id"))
        assert(r.contest == fx.round && r.id == id && r.team == s.submit.submitId.teamId, s"submit $id")
        assert(r.rw.startsWith("<tr id=\"result-row-" + id + "\"") && r.rw.endsWith("</tr>"), s"markup of $id")
        assert(r.rw.contains(s.result.message) && r.ro.contains(s.result.message), s"verdict of $id")
        assert(r.rw.contains(s"rejudge-$id") && !r.ro.contains(s"rejudge-$id"), s"rejudge button of $id")
      }
      assert(await(real.render(done.toInt, None)).get.rw.contains("6.67 из 10"))
    }
  }

  test("db: a submit that is not tested yet is shown as running, after the retries") {
    assume(fx.url.isDefined)
    fx.withFixture {
      val untested = fx.insertSubmit().toInt
      val fast = new DbStatusRows(id => AdminSubmits.row(fx.db, id), noTeams, system.scheduler, retryAfter = 10.millis, retries = 2)
      assert(await(fast.render(untested, None)).get.rw.contains("Running..."))
    }
  }
}
