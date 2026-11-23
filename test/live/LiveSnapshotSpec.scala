package live

import actors.LiveMonitorActor._
import akka.actor.{ActorSystem, Status}
import akka.testkit.{TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import play.api.i18n.{DefaultMessagesApi, Lang, MessagesImpl}

import scala.concurrent.duration._

/** The board rendered into the admin and participant pages is the board the stream starts with. */
class LiveSnapshotSpec extends TestKit(ActorSystem("LiveSnapshotSpec", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |akka.use-slf4j = off
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll with LiveActorFixtures {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private implicit val messages: MessagesImpl = MessagesImpl(Lang("en"), new DefaultMessagesApi())

  private val subs = Seq(
    rec(1, 1, "A", t0.plusMinutes(30), ok),
    rec(2, 2, "B", t0.plusHours(4).plusMinutes(5), pts(6)),
    rec(3, 3, "C", t0.plusHours(3), live.Pending))

  private def snapshot(f: Fixture, j: Join): LiveBoard = {
    val p = TestProbe()
    p.send(f.actor, Snapshot(j))
    p.expectMsgType[LiveBoard](5.seconds)
  }

  test("the snapshot is the real board, equal to the stream's first board event, also while the round runs") {
    val f = new Fixture(late, subs = Map(1 -> subs))
    val b = snapshot(f, JoinAdmin(1))
    assert(b == boardOf(contest(), subs))
    assert(LiveJson.board(b) == next(f.subscribe(JoinAdmin(1)), "board"))
    val html = views.html.livetable(b).body
    assert(html.contains("full-score") && html.contains("?1") && html.contains(">6<"))
    f.stop()
  }

  test("the participant snapshot is teamsOnly before start and the real board once started, matching the stream") {
    val before = new Fixture(t0.minusMinutes(5), subs = Map(1 -> subs))
    val bBefore = snapshot(before, JoinTeam(1))
    assert(bBefore == LiveBoard.build(Nil, teams, Map.empty))
    assert(LiveJson.board(bBefore) == next(before.subscribe(JoinTeam(1)), "board"))
    before.stop()

    val after = new Fixture(late, subs = Map(1 -> subs))
    val bAfter = snapshot(after, JoinTeam(1))
    assert(bAfter == boardOf(contest(), subs))
    assert(LiveJson.board(bAfter) == next(after.subscribe(JoinTeam(1)), "board"))
    after.stop()
  }

  test("an unknown contest is refused like the join") {
    val f = new Fixture(late, subs = Map(1 -> subs))
    val p = TestProbe()
    p.send(f.actor, Snapshot(JoinAdmin(9)))
    p.expectMsgType[Status.Failure](5.seconds)
    p.send(f.actor, Snapshot(JoinTeam(9)))
    p.expectMsgType[Status.Failure](5.seconds)
    f.stop()
  }
}
