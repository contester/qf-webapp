package live

import java.util.concurrent.atomic.AtomicInteger

import actors.LiveMonitorActor._
import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import models.{AdminSubmitRow, AdminSubmits}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.qf.actors.TeamStateActor
import org.stingray.qf.models.TeamClient
import play.api.libs.json.JsValue

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * The status page stream with the real rows: the live monitor actor fed with hook events for a submit of a fixture
  * round, the rows read and rendered from the database (skipped unless QF_TEST_URL is set, see SubmitInsertDbSpec).
  */
class LiveStatusDbSpec extends TestKit(ActorSystem("LiveStatusDbSpec", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll with LiveActorFixtures {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private implicit val ec: ExecutionContext = ExecutionContext.global
  private def await[T](f: Future[T]) = Await.result(f, 120.seconds)

  private val fx = new models.ContestFixture("Live status spec")

  private lazy val teamClient = new TeamClient(system.actorOf(Props(new Actor {
    def receive = { case TeamStateActor.GetTeams(_) => sender() ! Map.empty[Int, org.stingray.contester.dbmodel.Team] }
  })))

  /** Counts the reads, so that the number of viewers can be told apart from the number of events. */
  private class CountingRows(inner: StatusRows) extends StatusRows {
    val reads = new AtomicInteger
    override def render(id: Int, inTesting: Option[models.SubmitResult])(implicit ec: ExecutionContext): Future[Option[StatusRow]] = {
      reads.incrementAndGet()
      inner.render(id, inTesting)
    }
  }

  private def realRows = new CountingRows(DbStatusRows.fromDb(fx.db, teamClient, system.scheduler))

  // A finished submit of the fixture round (created inside withFixture).
  private def sample(): AdminSubmitRow = {
    val id = fx.insertSubmit()
    fx.finishSubmit(id, passed = 1)
    Await.result(AdminSubmits.row(fx.db, id.toInt), 60.seconds).get
  }

  private def hookRec(s: AdminSubmitRow) =
    rec(s.submit.submitId.id, s.submit.submitId.teamId, s.submit.submitId.problem.id, running)

  private def rowOf(p: TestProbe): JsValue = next(p, "statusRow")

  test("db: an arrival, a finish and a rejudge each push the submit's own row") {
    assume(fx.url.isDefined)
    fx.withFixture {
    val rawId = fx.insertSubmit()
    val pending = Await.result(AdminSubmits.row(fx.db, rawId.toInt), 60.seconds).get
    val cid = pending.submit.submitId.contestId
    val id = pending.submit.submitId.id
    val f = new Fixture(running, rows = Seq(contest(cid)), statusRows = realRows)
    val p = f.subscribe(JoinAdminStatus(cid))
    next(p, "ready")

    f.actor ! Arrived(cid, hookRec(pending))
    val arrived = rowOf(p)
    assert((arrived \ "id").as[Int] == id && (arrived \ "team").as[Int] == pending.submit.submitId.teamId)
    assert((arrived \ "rw").as[String].contains("Running..."), "an arrival shows as running")

    fx.finishSubmit(rawId, passed = 1)
    val sample = Await.result(AdminSubmits.row(fx.db, rawId.toInt), 60.seconds).get
    f.actor ! Finished(cid, id, compiled = true, sample.submit.points)
    val finished = rowOf(p)
    assert((finished \ "rw").as[String].contains(sample.result.message), s"expected '${sample.result.message}'")
    assert((finished \ "rw").as[String].contains(s"rejudge-$id") && !(finished \ "ro").as[String].contains(s"rejudge-$id"))

    // A late arrival for a finished submit does not revert to Running...
    f.actor ! Arrived(cid, hookRec(sample))
    val late = rowOf(p)
    assert((late \ "rw").as[String].contains(sample.result.message) && !(late \ "rw").as[String].contains("Running..."))

    // The hook for a rejudge has only the id: the contest comes from the database.
    f.actor ! RejudgeRequested(id)
    assert((rowOf(p) \ "rw").as[String].contains("Testing in progress..."))
    f.stop()
    }
  }

  test("db: a submit of a team or a problem the board does not know still gets its row") {
    assume(fx.url.isDefined)
    fx.withFixture {
    val sample = this.sample()
    val cid = sample.submit.submitId.contestId
    // The fixture's board has teams 1-3 and problems A-C; this one is neither.
    val f = new Fixture(running, rows = Seq(contest(cid)), statusRows = realRows)
    val p = f.subscribe(JoinAdminStatus(cid))
    next(p, "ready")
    f.actor ! Arrived(cid, rec(sample.submit.submitId.id, 99999, "ZZ", running))
    assert((rowOf(p) \ "id").as[Int] == sample.submit.submitId.id)
    f.stop()
    }
  }

  test("db: any number of viewers cost one read per event") {
    assume(fx.url.isDefined)
    fx.withFixture {
    val sample = this.sample()
    val cid = sample.submit.submitId.contestId
    val rows = realRows
    val f = new Fixture(running, rows = Seq(contest(cid)), statusRows = rows)
    val viewers = (1 to 5).map(_ => f.subscribe(JoinAdminStatus(cid)))
    viewers.foreach(next(_, "ready"))
    f.actor ! Arrived(cid, hookRec(sample))
    viewers.foreach(rowOf)
    // One at a time: a finish heard while the arrival's row is still being read replaces it (tested elsewhere).
    f.actor ! Finished(cid, sample.submit.submitId.id, compiled = true, Some(1.0))
    viewers.foreach(rowOf)
    assert(rows.reads.get == 2)
    f.stop()
    }
  }
}
