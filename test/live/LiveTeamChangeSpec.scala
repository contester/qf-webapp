package live

import actors.LiveMonitorActor._
import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.{Disqualified, NotRated}
import play.api.libs.json.JsValue

import scala.concurrent.Promise
import scala.concurrent.duration._

/**
  * Review Focus 5: credential_changed (unrated flips, is_active off) and participant edits while an admin monitor is
  * open update, remove or add the row on the open stream, with no reload of the contest and no page reload.
  */
class LiveTeamChangeSpec extends TestKit(ActorSystem("LiveTeamChangeSpec", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |akka.use-slf4j = off
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll with LiveActorFixtures {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  // Team 1 has 10 points, team 2 has 5, team 3 nothing.
  private val subs = Seq(rec(1, 1, "A", t0.plusMinutes(10), ok), rec(2, 2, "A", t0.plusMinutes(20), pts(5)))

  private def places(b: JsValue): Seq[(Int, Option[Int])] =
    (b \ "rows").as[Seq[JsValue]].map(r => ((r \ "team").as[Int], (r \ "place").asOpt[Int]))

  private def open(): (Fixture, TestProbe) = {
    val f = new Fixture(running, subs = Map(1 -> subs))
    val p = f.subscribe(JoinAdmin(1))
    assert(places(next(p, "board")) == Seq((1, Some(1)), (2, Some(2)), (3, Some(3))))
    (f, p)
  }

  private def change(f: Fixture, ts: Seq[org.stingray.contester.dbmodel.Team]): Unit = {
    f.teamLists = Map(1 -> ts)
    f.actor ! TeamsChanged(1)
  }

  test("an unrated flip to not_rated moves the row to the unranked tail; the others move up; no reload") {
    val (f, p) = open()
    change(f, teams.map(t => if (t.id == 1) t.copy(unrated = Some(NotRated)) else t))
    val row = next(p, "row")
    assert((row \ "team").as[Int] == 1 && (row \ "pos").as[Int] == 2 && (row \ "row" \ "place").asOpt[Int].isEmpty)
    assert((row \ "rankFrom").as[Int] == 0 && (row \ "ranks").as[Seq[JsValue]].map(_.asOpt[Int]) == Seq(Some(1), Some(2), None))
    assert(f.loads.get == 1 && f.teamReads.get == 1)
    p.expectNoMessage(200.millis)
    f.stop()
  }

  test("a disqualification removes the row (a board event without it); undoing it brings the row back") {
    val (f, p) = open()
    change(f, teams.map(t => if (t.id == 1) t.copy(unrated = Some(Disqualified)) else t))
    assert(places(next(p, "board")) == Seq((2, Some(1)), (3, Some(2))))
    change(f, teams)
    assert(places(next(p, "board")) == Seq((1, Some(1)), (2, Some(2)), (3, Some(3))))
    assert(f.loads.get == 1)
    f.stop()
  }

  test("a deactivated credential removes the row; reactivating it brings back the team's points") {
    val (f, p) = open()
    change(f, teams.filterNot(_.id == 2))
    assert(places(next(p, "board")) == Seq((1, Some(1)), (3, Some(2))))
    change(f, teams)
    val back = next(p, "board")
    assert(places(back) == Seq((1, Some(1)), (2, Some(2)), (3, Some(3))))
    assert(best(cellJs(back, 2, "A")).contains("5"))
    assert(f.loads.get == 1)
    f.stop()
  }

  test("a participant rename or school change updates the row in place") {
    val (f, p) = open()
    change(f, teams.map(t => if (t.id == 2) t.copy(name = "Renamed", school = "New School") else t))
    val row = next(p, "row")
    assert((row \ "team").as[Int] == 2 && (row \ "pos").as[Int] == 1)
    assert((row \ "row" \ "name").as[String] == "Renamed" && (row \ "row" \ "school").as[String] == "New School, City")
    assert(f.loads.get == 1)
    f.stop()
  }

  test("a new participant is added; an unchanged list sends nothing") {
    val (f, p) = open()
    change(f, teams)
    p.expectNoMessage(300.millis)
    change(f, teams :+ team(4))
    assert(places(next(p, "board")).map(_._1) == Seq(1, 2, 3, 4))
    f.stop()
  }

  test("a burst of TeamsChanged during a read coalesces into one more read; the newest list wins") {
    val (f, p) = open()
    val gate = Promise[Unit]()
    f.teamsGate = gate.future
    f.teamLists = Map(1 -> teams.map(t => if (t.id == 3) t.copy(name = "Old") else t))
    f.actor ! TeamsChanged(1)
    f.barrier()
    f.teamLists = Map(1 -> teams.map(t => if (t.id == 3) t.copy(name = "Newest") else t))
    (1 to 10).foreach(_ => f.actor ! TeamsChanged(1))
    f.barrier()
    assert(f.teamReads.get == 1)
    gate.success(())
    awaitCond(f.teamReads.get == 2, 5.seconds)
    f.barrier()
    Thread.sleep(200)
    assert(f.teamReads.get == 2)
    val board = next(f.subscribe(JoinAdmin(1)), "board")
    assert((board \ "rows").as[Seq[JsValue]].exists(r => (r \ "name").as[String] == "Newest"))
    f.stop()
  }

  test("a failed team read falls back to reloading the contest, so the change is not lost") {
    val (f, p) = open()
    f.teamsFail = true
    f.data = Map(1 -> ContestData(problems, teams.filterNot(_.id == 3), subs))
    f.actor ! TeamsChanged(1)
    assert(places(next(p, "board")).map(_._1) == Seq(1, 2))
    assert(f.loads.get == 2)
    f.stop()
  }

  test("a change for a round the monitor does not hold is ignored") {
    val (f, p) = open()
    f.actor ! TeamsChanged(42)
    f.barrier()
    assert(f.teamReads.get == 0)
    p.expectNoMessage(200.millis)
    f.stop()
  }
}
