package live

import actors.LiveMonitorActor._
import akka.actor.{ActorSystem, Status}
import akka.testkit.{TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import play.api.i18n.{DefaultMessagesApi, Lang, MessagesImpl}

import scala.concurrent.duration._

/** The board rendered into the page is the board the stream starts with: same viewer rules, no leaks. */
class LiveSnapshotSpec extends TestKit(ActorSystem("LiveSnapshotSpec", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |akka.use-slf4j = off
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll with LiveActorFixtures {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private implicit val messages: MessagesImpl = MessagesImpl(Lang("en"), new DefaultMessagesApi())

  // Team 1 solved before the freeze; team 2 solved B after the freeze (real only); team 3 has a failed try.
  private val subs = Seq(
    rec(1, 1, "A", t0.plusMinutes(30), ok),
    rec(2, 2, "B", t0.plusHours(4).plusMinutes(5), ok),
    rec(3, 3, "C", t0.plusHours(3), Done(compiled = true, 0, 3)))

  private def snapshot(f: Fixture, j: Join): LiveBoard = {
    val p = TestProbe()
    p.send(f.actor, Snapshot(j))
    p.expectMsgType[LiveBoard](5.seconds)
  }

  private def fixture() = new Fixture(frozen, subs = Map(1 -> subs))
  private val model = new LiveContest(contest(), problems, teams, subs, frozen)

  test("public and the freezing admin get the freezing board, the real admin the real one") {
    val f = fixture()
    assert(snapshot(f, JoinPublic(1)) == model.board(Freezing))
    assert(snapshot(f, JoinAdmin(1, real = false)) == model.board(Freezing))
    assert(snapshot(f, JoinAdmin(1, real = true)) == model.board(Real))
    assert(model.board(Freezing) != model.board(Real))
    f.stop()
  }

  test("the page of a viewer without the real board never carries a real-only cell") {
    val f = fixture()
    val real = views.html.livetable(model.board(Real), 0).body
    val frozenHtml = views.html.livetable(model.board(Freezing), 0).body
    assert(real != frozenHtml)
    assert(views.html.livetable(snapshot(f, JoinPublic(1)), 0).body == frozenHtml)
    assert(views.html.livetable(snapshot(f, JoinTeam(1, 3)), 3).body != real)
    f.stop()
  }

  test("a team sees its own real cells with the frozen rank, score and position") {
    val f = fixture()
    val frozenBoard = model.board(Freezing)
    val b = snapshot(f, JoinTeam(1, 2))
    assert(b.rows.map(r => (r.team.id, r.rank, r.score)) == frozenBoard.rows.map(r => (r.team.id, r.rank, r.score)))
    assert(b.solvedCounts == frozenBoard.solvedCounts)
    assert(b.rowOf(2).get.cells == model.board(Real).rowOf(2).get.cells)
    assert(b.rowOf(2).get.cells != frozenBoard.rowOf(2).get.cells)
    for (t <- Seq(1, 3)) assert(b.rowOf(t) == frozenBoard.rowOf(t))
    f.stop()
  }

  test("after the expose time everyone gets the real board") {
    val f = new Fixture(contest().exposeTime.plusMinutes(1), subs = Map(1 -> subs))
    val real = new LiveContest(contest(), problems, teams, subs, contest().exposeTime.plusMinutes(1)).board(Real)
    assert(snapshot(f, JoinPublic(1)) == real)
    assert(snapshot(f, JoinTeam(1, 2)) == real)
    f.stop()
  }

  test("an unknown contest and a contest that is not public are refused like the join") {
    val f = new Fixture(frozen, cfg = defaultCfg.copy(publicContests = Set(2)), subs = Map(1 -> subs))
    for (j <- Seq(JoinPublic(1), JoinPublic(7), JoinTeam(7, 1), JoinAdmin(7, real = true))) {
      val p = TestProbe()
      p.send(f.actor, Snapshot(j))
      p.expectMsgType[Status.Failure](5.seconds)
    }
    f.stop()
  }
}
