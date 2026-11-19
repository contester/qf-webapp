package live

import java.util.concurrent.atomic.AtomicInteger

import actors.LiveMonitorActor
import actors.LiveMonitorActor._
import akka.NotUsed
import akka.actor.ActorRef
import akka.stream.scaladsl.Source
import akka.testkit.{TestKit, TestProbe}
import models.ProblemColumn
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{Contest, Team}
import palevo.PalevoSink
import play.api.libs.EventSource.Event
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

/** Shared fixtures for the specs that drive a LiveMonitorActor with a fake loader and clock. */
trait LiveActorFixtures { this: TestKit =>
  val t0 = new DateTime(2026, 11, 1, 10, 0)
  val running = t0.plusHours(1)
  val late = t0.plusHours(4).plusMinutes(10)

  def contest(id: Int = 1): Contest =
    Contest(id, s"C$id", t0, t0.plusHours(5), None, 1)

  def team(id: Int): Team =
    Team(id, id, s"Team$id", f"School$id%02d", "City", None)

  // Three problems of 10 points each.
  val problems: Seq[ProblemColumn] = Seq("A", "B", "C").map(p => ProblemColumn(p, s"Problem $p", 10))
  val problemIds: Seq[String] = problems.map(_.id)
  val teams = Seq(team(1), team(2), team(3))
  /** A finished testing with all 10 points. */
  val ok = Done(compiled = true, Some(10.0))
  def pts(p: Double): SubmitState = Done(compiled = true, Some(p))

  def rec(id: Int, t: Int, p: String, at: DateTime, state: SubmitState = live.Pending): SubmitRec =
    SubmitRec(id, t, p, at, ((at.getMillis - t0.getMillis) / 1000).toInt, state)

  val defaultTimings = LiveTimings(reloadGap = Duration.Zero, ping = 1.hour)

  class FakeClock(@volatile var at: DateTime) extends LiveClock {
    @volatile var ms: Long = 0L
    override def now(): DateTime = at
    override def millis(): Long = ms
  }

  class Fixture(now: DateTime,
                rows: Seq[Contest] = Seq(contest()),
                subs: Map[Int, Seq[SubmitRec]] = Map.empty,
                palevoSink: PalevoSink = palevo.NoopPalevoSink,
                gate: Future[Unit] = Future.successful(()),
                timings: LiveTimings = defaultTimings,
                overrideData: Map[Int, ContestData] = Map.empty,
                // True: the actor reads the system clock (timer tests); `clock` is then unused.
                realClock: Boolean = false,
                statusRows: StatusRows = NoStatusRows) {
    val clock = new FakeClock(now)
    @volatile var contestRows: Seq[Contest] = rows
    // The start-up listing reads the rows when it starts but only answers when this is completed.
    @volatile var pollGate: Future[Unit] = Future.successful(())
    @volatile var data: Map[Int, ContestData] =
      rows.map(c => c.id -> ContestData(problems, teams, subs.getOrElse(c.id, Nil))).toMap ++ overrideData
    // Loads wait for this; a test replaces it to hold a later load back.
    @volatile var loadGate: Future[Unit] = gate
    val loads = new AtomicInteger
    // Number of upcoming loads that fail.
    val failLoads = new AtomicInteger
    // Submit id -> owning contest for the contestOf lookup; a missing id is "no such submit". lookupFails: the lookup throws.
    @volatile var owners: Map[Int, Int] = Map.empty
    @volatile var lookupFails = false
    val lookups = new AtomicInteger
    // The team lists read on TeamsChanged: by default the loaded data's; teamsGate holds the reads back.
    @volatile var teamLists: Map[Int, Seq[Team]] = Map.empty
    @volatile var teamsGate: Future[Unit] = Future.successful(())
    @volatile var teamsFail = false
    val teamReads = new AtomicInteger

    private val loader = new ContestLoader {
      override def load(c: Contest)(implicit ec: ExecutionContext): Future[ContestData] = {
        loads.incrementAndGet()
        if (failLoads.getAndUpdate(n => (n - 1).max(0)) > 0) Future.failed(new RuntimeException("injected load failure"))
        else loadGate.map(_ => data(c.id))
      }

      override def contestOf(id: Int)(implicit ec: ExecutionContext): Future[Option[Int]] = {
        lookups.incrementAndGet()
        if (lookupFails) Future.failed(new RuntimeException("injected lookup failure"))
        else Future.successful(owners.get(id))
      }

      override def teams(c: Int)(implicit ec: ExecutionContext): Future[Seq[Team]] = {
        teamReads.incrementAndGet()
        if (teamsFail) Future.failed(new RuntimeException("injected team read failure"))
        else teamsGate.map(_ => teamLists.getOrElse(c, data(c).teams))
      }
    }

    val actor: ActorRef = system.actorOf(LiveMonitorActor.propsWith(
      () => { val read = contestRows; pollGate.map(_ => read)(ExecutionContext.global) },
      loader, palevoSink, if (realClock) SystemLiveClock else clock, timings, statusRows = statusRows))

    def join(m: Any): Source[Event, NotUsed] = {
      val p = TestProbe()
      p.send(actor, m)
      p.expectMsgType[Source[Event, NotUsed]](5.seconds)
    }

    def subscribe(m: Any): TestProbe = {
      val p = TestProbe()
      join(m).runForeach(e => p.ref ! e)
      p
    }

    // A reply to a join proves every earlier message has been handled.
    def barrier(): Unit = join(JoinAdmin(rows.head.id))

    def stop(): Unit = system.stop(actor)
  }

  def next(p: TestProbe, name: String): JsValue = {
    val e = p.expectMsgType[Event](5.seconds)
    assert(e.name.contains(name), e)
    Json.parse(e.data)
  }

  def boardOf(c: Contest, subs: Seq[SubmitRec]): LiveBoard =
    new LiveContest(c, problems, teams, subs).board

  /** A cell of a `board` event's row of the team (an absent cell is {}). */
  def cellJs(board: JsValue, team: Int, p: String): JsValue =
    ((board \ "rows").as[Seq[JsValue]].find(r => (r \ "team").as[Int] == team).get \ "cells" \ p).asOpt[JsValue]
      .getOrElse(Json.obj())

  /** A cell of a `row` event. */
  def rowCellJs(row: JsValue, p: String): JsValue = (row \ "row" \ "cells" \ p).asOpt[JsValue].getOrElse(Json.obj())

  /** The best points of a cell as shown, None without. */
  def best(cell: JsValue): Option[String] = (cell \ "b").asOpt[String]

  /** How many submits of the cell are in flight. */
  def pending(cell: JsValue): Int = (cell \ "p").asOpt[Int].getOrElse(0)
}
