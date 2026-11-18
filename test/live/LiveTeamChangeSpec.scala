package live

import actors.LiveMonitorActor._
import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.{Disqualified, NotRated, Team}

import scala.concurrent.duration._

/** A changed team (not rated, name, ...) reaches the boards at once, as whole boards for every stream. */
class LiveTeamChangeSpec extends TestKit(ActorSystem("LiveTeamChangeSpec", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |akka.use-slf4j = off
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll with LiveActorFixtures {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private def changed(id: Int, notRated: Boolean = false, school: String = "Changed", disqualified: Boolean = false): Team =
    Team(id, 0, "", school, "City", (if (disqualified) Some(Disqualified) else if (notRated || disqualified) Some(NotRated) else None))

  // Team 1 solved A fast, team 2 solved A slower, team 3 nothing.
  private val subs = Seq(rec(1, 1, "A", t0.plusMinutes(10), ok), rec(2, 2, "A", t0.plusMinutes(20), ok))

  private def ranks(b: LiveBoard) = b.rows.map(r => (r.team.id, r.rank))

  test("model: a team that becomes not rated gets rank 0 and the next rated team takes rank 1, in both boards") {
    val m = new LiveContest(contest(), problems, teams, subs, running)
    assert(ranks(m.board(Real)) == Seq((1, 1), (2, 2), (3, 3)))
    val d = m.onTeamChanged(changed(1, notRated = true), running)
    assert(ranks(m.board(Real)) == Seq((1, 0), (2, 1), (3, 2)))
    // the round is frozen from its start: no solves on the freezing board, but team 1 is unranked there too
    assert(ranks(m.board(Freezing)).find(_._1 == 1).contains((1, 0)))
    // one row change per board, not a whole board
    assert(d.freezing.size == 1 && d.freezing.forall(_.isInstanceOf[RowOut]) && d.real.size == 1 && d.real.forall(_.isInstanceOf[RowOut]))
    assert(d.public.size == 1 && d.public.forall(_.isInstanceOf[RowOut]) && d.ownCells.isEmpty)
    assert(m.differences(new LiveContest(contest(), problems, Seq(changed(1, notRated = true), teams(1), teams(2)), subs, running)).isEmpty)
    assert(m.board(Real).rowOf(1).get.team.schoolFullNameWithNum.contains("Changed"))
    // the cells are kept
    assert(m.board(Real).rowOf(1).get.cells.contains("A"))
    m.onTeamChanged(changed(1), running)
    assert(ranks(m.board(Real)) == Seq((1, 1), (2, 2), (3, 3)))
  }

  test("model: a disqualified team leaves the boards, and an undone disqualification ranks it again") {
    val m = new LiveContest(contest(), problems, teams, subs, running)
    m.onTeamChanged(changed(1, disqualified = true), running)
    assert(ranks(m.board(Real)) == Seq((2, 1), (3, 2)))
    // disqualified, then merely not rated: back on the board, unranked
    m.onTeamChanged(changed(1, notRated = true), running)
    assert(ranks(m.board(Real)) == Seq((1, 0), (2, 1), (3, 2)) && !m.board(Real).rowOf(1).get.team.disqualified)
    m.onTeamChanged(changed(1), running)
    assert(ranks(m.board(Real)) == Seq((1, 1), (2, 2), (3, 3)))
    assert(m.differences(new LiveContest(contest(), problems, Seq(changed(1), teams(1), teams(2)), subs, running)).isEmpty)
  }

  test("model: a disabled team is on no board, its submits score nothing, and enabling it brings its work back") {
    def off(id: Int) = Team(id, 0, "", "Off", "City", Some(Disqualified))
    val m = new LiveContest(contest(), problems, teams, subs, running)
    val d = m.onTeamChanged(off(1), running)
    assert(ranks(m.board(Real)) == Seq((2, 1), (3, 2)) && m.publicBoard.rowOf(1).isEmpty)
    assert(d.real == Seq(WholeOut(m.board(Real))))
    // a new submit of the disabled team changes nothing
    val late = m.onArrived(rec(5, 1, "B", t0.plusMinutes(30), ok), running)
    assert(late == Delta.empty || (late.real.isEmpty && late.freezing.isEmpty && late.public.isEmpty))
    assert(ranks(m.board(Real)) == Seq((2, 1), (3, 2)))
    // a team that starts disabled behaves the same, and a fresh build agrees
    assert(m.differences(new LiveContest(contest(), problems, Seq(off(1), teams(1), teams(2)), subs :+ rec(5, 1, "B", t0.plusMinutes(30), ok), running)).isEmpty)
    m.onTeamChanged(teams(0), running)
    assert(ranks(m.board(Real)) == Seq((1, 1), (2, 2), (3, 3)))
    assert(m.board(Real).rowOf(1).get.cells.contains("B"))
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
