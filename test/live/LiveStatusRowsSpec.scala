package live

import actors.LiveMonitorActor._
import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import play.api.libs.json.JsValue

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}

/** The status page stream of the live monitor: a row for every arrival, finish and rejudge, gated by the freeze. */
class LiveStatusRowsSpec extends TestKit(ActorSystem("LiveStatusRowsSpec", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll with LiveActorFixtures {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  /** Rows by submit id; a row can be held back with a promise to make a read slow. */
  private class FakeRows(after: Set[Int] = Set.empty) extends StatusRows {
    val gates = new ConcurrentHashMap[Int, Promise[Unit]]
    val asked = new ConcurrentHashMap[Int, Option[models.SubmitResult]]

    override def render(id: Int, inTesting: Option[models.SubmitResult])(implicit ec: ExecutionContext): Future[Option[StatusRow]] = {
      asked.put(id, inTesting)
      val gate = Option(gates.get(id)).map(_.future).getOrElse(Future.successful(()))
      val what = inTesting.map(_.message).getOrElse("done")
      gate.map(_ => Some(StatusRow(1, id, 1, after(id), s"rw-$id-$what", s"ro-$id-$what")))
    }
  }

  private def row(p: TestProbe): JsValue = next(p, "statusRow")

  test("an open status page gets a ready event, then a row for an arrival and for a finish") {
    val rows = new FakeRows
    val f = new Fixture(running, statusRows = rows)
    val p = f.subscribe(JoinAdminStatus(1, real = true))
    next(p, "ready")

    f.actor ! Arrived(1, rec(11, 1, "A", running))
    val arrived = row(p)
    assert((arrived \ "id").as[Int] == 11)
    assert((arrived \ "rw").as[String] == "rw-11-Running...")
    assert((arrived \ "ro").as[String] == "ro-11-Running...")

    f.actor ! Finished(1, 11, compiled = true, 3, 3)
    assert((row(p) \ "rw").as[String] == "rw-11-done")
    f.stop()
  }

  test("a rejudge of a submit reads its row as being tested, in whatever contest it is") {
    val rows = new FakeRows
    val f = new Fixture(running, subs = Map(1 -> Seq(rec(11, 1, "A", running, ok))), statusRows = rows)
    val p = f.subscribe(JoinAdminStatus(1, real = true))
    next(p, "ready")

    f.actor ! RejudgeRequested(11)
    assert((row(p) \ "rw").as[String] == "rw-11-Testing in progress...")
    f.stop()
  }

  test("a row read for an older event does not overwrite the newer one") {
    val rows = new FakeRows
    val slow = Promise[Unit]()
    rows.gates.put(11, slow)
    val f = new Fixture(running, statusRows = rows)
    val p = f.subscribe(JoinAdminStatus(1, real = true))
    next(p, "ready")

    f.actor ! Arrived(1, rec(11, 1, "A", running))
    f.barrier()
    // The finish is read after the arrival, and quickly.
    rows.gates.remove(11)
    f.actor ! Finished(1, 11, compiled = true, 3, 3)
    assert((row(p) \ "rw").as[String] == "rw-11-done")
    slow.success(())
    f.barrier()
    p.expectNoMessage(300.millis)
    f.stop()
  }

  test("a viewer that cannot see the real board gets no rows of submits after the freeze") {
    val rows = new FakeRows(after = Set(12))
    val f = new Fixture(running, statusRows = rows)
    val real = f.subscribe(JoinAdminStatus(1, real = true))
    val frozen = f.subscribe(JoinAdminStatus(1, real = false))
    next(real, "ready")
    next(frozen, "ready")

    f.actor ! Arrived(1, rec(12, 1, "A", running))
    f.actor ! Arrived(1, rec(13, 2, "A", running))
    assert((row(real) \ "id").as[Int] == 12)
    assert((row(real) \ "id").as[Int] == 13)
    assert((row(frozen) \ "id").as[Int] == 13)
    frozen.expectNoMessage(300.millis)
    f.stop()
  }

  test("nothing is read while no status page is open") {
    val rows = new FakeRows
    val f = new Fixture(running, statusRows = rows)
    f.actor ! Arrived(1, rec(11, 1, "A", running))
    f.actor ! Finished(1, 11, compiled = true, 3, 3)
    f.actor ! RejudgeRequested(11)
    f.barrier()
    assert(rows.asked.isEmpty)
    f.stop()
  }

  test("a status page does not get board events") {
    val f = new Fixture(running, statusRows = new FakeRows)
    val p = f.subscribe(JoinAdminStatus(1, real = true))
    next(p, "ready")
    f.actor ! Arrived(1, rec(11, 1, "A", running))
    row(p)
    p.expectNoMessage(300.millis)
    f.stop()
  }
}
