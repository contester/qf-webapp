package live

import actors.LiveMonitorActor._
import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.{LocalTeam, Team}

import scala.concurrent.duration._

/** A changed team (not rated, name, ...) reaches the boards at once, as whole boards for every stream. */
class LiveTeamChangeSpec extends TestKit(ActorSystem("LiveTeamChangeSpec", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |akka.use-slf4j = off
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll with LiveActorFixtures {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private def changed(id: Int, notRated: Boolean = false, school: String = "Changed"): Team =
    LocalTeam(id, 1, school, s"S$id", 0, "", notRated, noPrint = false, disabled = false)

  // Team 1 solved A fast, team 2 solved A slower, team 3 nothing.
  private val subs = Seq(rec(1, 1, "A", t0.plusMinutes(10), ok), rec(2, 2, "A", t0.plusMinutes(20), ok))

  private def ranks(b: LiveBoard) = b.rows.map(r => (r.team.id, r.rank))

  test("model: a team that becomes not rated gets rank 0 and the next rated team takes rank 1, in both boards") {
    val m = new LiveContest(contest(), problems, teams, subs, running)
    assert(ranks(m.board(Real)) == Seq((1, 1), (2, 2), (3, 3)))
    val d = m.onTeamChanged(changed(1, notRated = true), running)
    assert(ranks(m.board(Real)) == Seq((1, 0), (2, 1), (3, 2)))
    assert(ranks(m.board(Freezing)) == ranks(m.board(Real)))
    assert(d.freezing == Seq(WholeOut(m.board(Freezing))) && d.real == Seq(WholeOut(m.board(Real))))
    assert(m.board(Real).rowOf(1).get.team.schoolFullNameWithNum.contains("Changed"))
    // the cells are kept
    assert(m.board(Real).rowOf(1).get.cells.contains("A"))
    m.onTeamChanged(changed(1), running)
    assert(ranks(m.board(Real)) == Seq((1, 1), (2, 2), (3, 3)))
  }

  test("model: the result equals a fresh build from the changed team list") {
    val m = new LiveContest(contest(), problems, teams, subs, running)
    m.onTeamChanged(changed(2, notRated = true), running)
    val fresh = new LiveContest(contest(), problems, Seq(teams(0), changed(2, notRated = true), teams(2)), subs, running)
    assert(m.differences(fresh).isEmpty)
  }

  test("model: a team new to the contest is added") {
    val m = new LiveContest(contest(), problems, teams, subs, running)
    m.onTeamChanged(changed(9), running)
    assert(m.board(Real).rowOf(9).isDefined)
  }

  test("actor: every stream gets a whole board") {
    val f = new Fixture(running, subs = Map(1 -> subs))
    val pub = f.subscribe(JoinPublic(1))
    val adm = f.subscribe(JoinAdmin(1, real = true))
    val tm = f.subscribe(JoinTeam(1, 2))
    Seq(pub, adm, tm).foreach(p => next(p, "board"))

    f.actor ! TeamChanged(1, changed(1, notRated = true))
    for (p <- Seq(pub, adm, tm)) {
      val b = next(p, "board")
      val rows = (b \ "rows").as[Seq[play.api.libs.json.JsValue]]
      assert(rows.map(r => ((r \ "team").as[Int], (r \ "rank").as[String])) == Seq((1, "*"), (2, "1"), (3, "2")))
    }
    f.stop()
  }

  test("actor: a change that arrives while a load runs is replayed onto the loaded state") {
    val f = new Fixture(running, subs = Map(1 -> subs))
    val pub = f.subscribe(JoinPublic(1))
    next(pub, "board")
    // Hold a load back; it returns the team list as it was before the change.
    val release = scala.concurrent.Promise[Unit]()
    f.loadGate = release.future
    f.actor ! CrossCheck
    awaitCond(f.loads.get == 2, 5.seconds)
    f.actor ! TeamChanged(1, changed(1, notRated = true))
    next(pub, "board")
    release.success(())
    pub.expectNoMessage(500.millis)
    val b = next(f.subscribe(JoinPublic(1)), "board")
    assert(((b \ "rows").as[Seq[play.api.libs.json.JsValue]].head \ "rank").as[String] == "*")
    f.stop()
  }

  test("a change for an unknown contest is ignored") {
    val f = new Fixture(running, subs = Map(1 -> subs))
    f.barrier()
    f.actor ! TeamChanged(5, changed(1, notRated = true))
    f.barrier()
    f.stop()
    succeed
  }

  test("before the start a not-rated team keeps its * and the rest show rank 1") {
    val f = new Fixture(t0.minusMinutes(5))
    val pub = f.subscribe(JoinPublic(1))
    next(pub, "board")
    f.actor ! TeamChanged(1, changed(1, notRated = true))
    val rows = (next(pub, "board") \ "rows").as[Seq[play.api.libs.json.JsValue]]
    assert(rows.map(r => ((r \ "team").as[Int], (r \ "rank").as[String])).toSet == Set((1, "*"), (2, "1"), (3, "1")))
    f.stop()
  }
}
