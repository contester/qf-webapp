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
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * The status page stream with the real rows: the live monitor actor fed with hook events for the submits of a
  * database, the rows read and rendered from it (skipped unless ADMINS_TEST_URL is set, like AdminSubmitsDbSpec;
  * needs a contest with submits):
  *   ADMINS_TEST_URL=jdbc:postgresql://127.0.0.1:54329/contest ADMINS_TEST_USER=postgres ADMINS_TEST_PASSWORD=x
  */
class LiveStatusDbSpec extends TestKit(ActorSystem("LiveStatusDbSpec", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll with LiveActorFixtures {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private implicit val ec: ExecutionContext = ExecutionContext.global
  private def await[T](f: Future[T]) = Await.result(f, 120.seconds)

  private val url = sys.env.get("ADMINS_TEST_URL")

  private lazy val db = DatabaseConfig.forConfig[JdbcProfile]("", ConfigFactory.parseString(
    s"""profile = "org.stingray.contester.dbmodel.MyPostgresProfile$$"
       |db { url = "${url.get}", user = "${sys.env.getOrElse("ADMINS_TEST_USER", "")}",
       |      password = "${sys.env.getOrElse("ADMINS_TEST_PASSWORD", "")}", driver = "org.postgresql.Driver",
       |      connectionPool = disabled }""".stripMargin)).db

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

  private def realRows = new CountingRows(DbStatusRows.fromDb(db, teamClient, system.scheduler))

  private def pageRows(contest: Int): Seq[AdminSubmitRow] =
    await(AdminSubmits.page(db, contest, None, None, None, size = 100000)).rows

  // A finished submit of the biggest contest.
  private lazy val sample: AdminSubmitRow = {
    val all = await(models.Rounds.currentRounds(db)).flatMap(c => pageRows(c.id))
    all.filter(_.submit.finished).maxBy(_.submit.submitId.id)
  }

  private def hookRec(s: AdminSubmitRow) =
    rec(s.submit.submitId.id, s.submit.submitId.teamId, s.submit.submitId.problem.id, running)

  private def rowOf(p: TestProbe): JsValue = next(p, "statusRow")

  test("db: an arrival, a finish and a rejudge each push the submit's own row") {
    assume(url.isDefined)
    val cid = sample.submit.submitId.contestId
    val id = sample.submit.submitId.id
    val f = new Fixture(running, rows = Seq(contest(cid)), statusRows = realRows)
    val p = f.subscribe(JoinAdminStatus(cid))
    next(p, "ready")

    f.actor ! Arrived(cid, hookRec(sample))
    val arrived = rowOf(p)
    assert((arrived \ "id").as[Int] == id && (arrived \ "team").as[Int] == sample.submit.submitId.teamId)
    assert((arrived \ "rw").as[String].contains("Running..."), "an arrival shows as running")

    f.actor ! Finished(cid, id, compiled = true, sample.submit.points)
    val finished = rowOf(p)
    assert((finished \ "rw").as[String].contains(sample.result.message), s"expected '${sample.result.message}'")
    assert((finished \ "rw").as[String].contains(s"rejudge-$id") && !(finished \ "ro").as[String].contains(s"rejudge-$id"))

    // The hook for a rejudge has only the id: the contest comes from the database.
    f.actor ! RejudgeRequested(id)
    assert((rowOf(p) \ "rw").as[String].contains("Testing in progress..."))
    f.stop()
  }

  test("db: a submit of a team or a problem the board does not know still gets its row") {
    assume(url.isDefined)
    val cid = sample.submit.submitId.contestId
    // The fixture's board has teams 1-3 and problems A-C; this one is neither.
    val f = new Fixture(running, rows = Seq(contest(cid)), statusRows = realRows)
    val p = f.subscribe(JoinAdminStatus(cid))
    next(p, "ready")
    f.actor ! Arrived(cid, rec(sample.submit.submitId.id, 99999, "ZZ", running))
    assert((rowOf(p) \ "id").as[Int] == sample.submit.submitId.id)
    f.stop()
  }

  test("db: any number of viewers cost one read per event") {
    assume(url.isDefined)
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
