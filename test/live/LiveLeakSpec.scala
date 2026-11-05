package live

import actors.LiveMonitorActor._
import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.joda.time.DateTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import play.api.libs.EventSource.Event
import play.api.libs.json.{JsLookupResult, JsObject, JsValue, Json}

import scala.concurrent.duration._
import scala.util.Random

/**
  * Mid-freeze leak checks. A model LiveContest is fed the same random events as the actor; what each stream
  * must carry is derived from the model's deltas (public = freezing board only), never from the actor's output.
  */
class LiveLeakSpec extends TestKit(ActorSystem("LiveLeakSpec", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |akka.use-slf4j = off
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll with LiveActorFixtures {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private val ids = Seq(1, 2, 3)
  private val seeds = Seq(1L, 7L, 42L)
  private val steps = 45

  private def modelEvent(out: BoardOut): Event = out match {
    case WholeOut(b) => LiveJson.event("board", LiveJson.board(b))
    case RowOut(c) => LiveJson.event("row", LiveJson.row(c))
  }

  private def drain(p: TestProbe): Vector[Event] =
    p.receiveWhile(10.seconds, 400.millis) { case e: Event => e }.toVector

  private def js(e: Event): JsValue = Json.parse(e.data)

  /** What every stream carried, plus what the model says it must have carried. */
  private case class Run(expectedPublic: Vector[Event], expectedReal: Vector[Event],
                         realBoards: Vector[LiveBoard], freezingBoards: Vector[LiveBoard],
                         pub: Vector[Event], adminFrozen: Vector[Event], adminReal: Vector[Event],
                         team: Map[Int, Vector[Event]], hiddenSteps: Int)

  private val initial = Seq(
    rec(1, 1, "A", t0.plusMinutes(30), ok),
    rec(2, 2, "B", t0.plusHours(4).plusMinutes(5), ok),
    rec(3, 3, "C", t0.plusHours(3), Done(compiled = true, 1, 3)))

  private def boardEv(b: LiveBoard): Event = LiveJson.event("board", LiveJson.board(b))

  private def scenario(seed: Long): Run = {
    val rnd = new Random(seed)
    val model = new LiveContest(contest(), problems, teams, initial, frozen)
    val f = new Fixture(frozen, subs = Map(1 -> initial))
    val pub = f.subscribe(JoinPublic(1))
    val af = f.subscribe(JoinAdmin(1, real = false))
    val ar = f.subscribe(JoinAdmin(1, real = true))
    val tm = ids.map(t => t -> f.subscribe(JoinTeam(1, t))).toMap
    f.barrier()

    var known = initial.map(r => r.id -> r).toMap
    var nextId = 100
    var at = t0.plusHours(3)
    var expPub = Vector(boardEv(model.board(Freezing)))
    var expReal = Vector(boardEv(model.board(Real)))
    var reals = Vector(model.board(Real))
    var frozens = Vector(model.board(Freezing))
    var hidden = 0

    def record(d: Option[Delta]): Unit = d.foreach { d =>
      expPub ++= d.freezing.map(modelEvent)
      expReal ++= d.real.map(modelEvent)
      reals :+= model.board(Real)
      frozens :+= model.board(Freezing)
      if (model.board(Real) != model.board(Freezing)) hidden += 1
    }

    (1 to steps).foreach { _ =>
      val roll = rnd.nextInt(10)
      def anyId = known.keys.toSeq.sorted.apply(rnd.nextInt(known.size))
      if (roll < 4) {
        at = Seq(at.plusSeconds(60 + rnd.nextInt(600)), frozen).minBy(_.getMillis)
        val r = rec(nextId, 1 + rnd.nextInt(3), problems(rnd.nextInt(3)), at)
        nextId += 1
        known += r.id -> r
        f.actor ! Arrived(1, r)
        record(Some(model.onArrived(r, frozen)))
      } else if (roll < 8) {
        val id = anyId
        val passed = if (rnd.nextBoolean()) 3 else rnd.nextInt(3)
        f.actor ! Finished(1, id, compiled = true, passed, 3)
        record(model.onFinished(id, compiled = true, passed, 3, frozen))
      } else {
        val id = anyId
        f.actor ! RejudgeRequested(id)
        record(model.onRejudge(id, frozen))
      }
    }
    f.barrier()
    val run = Run(expPub, expReal, reals, frozens, drain(pub), drain(af), drain(ar),
      tm.map { case (t, p) => t -> drain(p) }, hidden)
    f.stop()
    run
  }

  private lazy val runs: Map[Long, Run] = seeds.map(s => s -> scenario(s)).toMap

  /** (team, problem, cell) triples that an event shows. */
  private def cellsIn(e: Event): Seq[(Int, String, JsValue)] = {
    val j = js(e)
    def of(team: Int, cells: JsLookupResult) = cells.as[JsObject].fields.map { case (p, v) => (team, p, v) }
    e.name match {
      case Some("board") => (j \ "rows").as[Seq[JsValue]].flatMap(r => of((r \ "team").as[Int], r \ "cells"))
      case Some("row") => of((j \ "team").as[Int], j \ "row" \ "cells")
      case Some("cells") => of((j \ "team").as[Int], j \ "cells")
      case _ => Nil
    }
  }

  private def shownBy(bs: Seq[LiveBoard]): Set[(Int, String, JsValue)] =
    bs.flatMap(b => cellsIn(boardEv(b))).toSet

  /** The event with team t's cells taken out (board and row events); everything else untouched. */
  private def withoutCells(e: Event, t: Int): Event = {
    val j = js(e)
    def strip(o: JsObject) = if ((o \ "team").as[Int] == t) o - "cells" else o
    val out = e.name match {
      case Some("board") => j.as[JsObject] ++ Json.obj("rows" -> (j \ "rows").as[Seq[JsObject]].map(strip))
      case Some("row") if (j \ "team").as[Int] == t =>
        j.as[JsObject] ++ Json.obj("row" -> strip((j \ "row").as[JsObject]))
      case _ => j.as[JsObject]
    }
    e.copy(data = Json.stringify(out))
  }

  test("public stream equals the freezing-board events byte for byte and never shows a real-only cell") {
    seeds.foreach { s =>
      val r = runs(s)
      assert(r.hiddenSteps > 5, s"seed $s: the scenario must actually hide results")
      assert(r.pub.map(_.data) == r.expectedPublic.map(_.data), s"seed $s")
      assert(r.pub.map(_.name) == r.expectedPublic.map(_.name), s"seed $s")
      val realOnly = shownBy(r.realBoards) -- shownBy(r.freezingBoards)
      assert(realOnly.nonEmpty, s"seed $s: some cell must exist only on the real board")
      val seen = r.pub.flatMap(cellsIn).toSet
      assert(seen.intersect(realOnly).isEmpty, s"seed $s: leaked ${seen.intersect(realOnly)}")
    }
  }

  test("a team stream differs from the public one only in its own cells and in extra cells events") {
    seeds.foreach { s =>
      val r = runs(s)
      ids.foreach { t =>
        val evs = r.team(t)
        val main = evs.filterNot(_.name.contains("cells"))
        val extra = evs.filter(_.name.contains("cells"))
        val ctx = s"seed $s team $t"
        assert(main.map(_.name) == r.pub.map(_.name), ctx)
        assert(main.map(withoutCells(_, t)) == r.pub.map(withoutCells(_, t)), ctx)
        extra.foreach(e => assert((js(e) \ "team").as[Int] == t, ctx))

        // Own cells only ever take values the real board gave team t, and end on its final real cells.
        val realValues = shownBy(r.realBoards).filter(_._1 == t)
        val own = evs.flatMap(cellsIn).filter(_._1 == t)
        assert(own.toSet.subsetOf(realValues), ctx)
        val last = own.groupBy(_._2).map { case (p, xs) => p -> xs.last._3 }
        cellsIn(boardEv(r.realBoards.last)).filter(_._1 == t).foreach { case (_, p, v) =>
          assert(last.getOrElse(p, Json.obj("t" -> "empty", "s" -> "")) == v, s"$ctx problem $p")
        }
      }
      assert(ids.exists(t => r.team(t).count(_.name.contains("cells")) > 0), s"seed $s: no cells events at all")
      assert(ids.exists(t => r.team(t).flatMap(cellsIn).toSet != r.pub.flatMap(cellsIn).filter(_._1 == t).toSet
        && r.team(t).flatMap(cellsIn).exists(c => c._1 == t && !r.pub.flatMap(cellsIn).contains(c))),
        s"seed $s: the overlay must show a team something the public does not see")
    }
  }

  test("a team stream never carries another team's real-only cells") {
    seeds.foreach { s =>
      val r = runs(s)
      val realOnly = shownBy(r.realBoards) -- shownBy(r.freezingBoards)
      ids.foreach { t =>
        val foreign = r.team(t).flatMap(cellsIn).filter(_._1 != t).toSet
        assert(foreign.intersect(realOnly).isEmpty, s"seed $s: team $t sees $foreign")
        r.team(t).filter(_.name.contains("cells")).foreach(e => assert((js(e) \ "team").as[Int] == t))
      }
    }
  }

  test("the freezing admin stream equals the public one; the real admin stream equals the real board") {
    seeds.foreach { s =>
      val r = runs(s)
      assert(r.adminFrozen == r.pub, s"seed $s")
      assert(r.adminReal.map(_.data) == r.expectedReal.map(_.data), s"seed $s")
      assert(r.adminReal.map(_.name) == r.expectedReal.map(_.name), s"seed $s")
      assert(r.adminReal != r.pub, s"seed $s: real and public streams must differ somewhere")
    }
  }

  test("after the expose time the team and public streams equal the real board") {
    val expose = contest().exposeTime
    val model = new LiveContest(contest(), problems, teams, initial, frozen)
    val f = new Fixture(frozen, subs = Map(1 -> initial))
    val pub = f.subscribe(JoinPublic(1))
    val t1 = f.subscribe(JoinTeam(1, 1))
    val t2 = f.subscribe(JoinTeam(1, 2))
    Seq(pub, t1, t2).foreach(next(_, "board"))
    val r4 = rec(104, 1, "B", frozen)
    val r5 = rec(105, 2, "A", frozen.plusMinutes(1))
    f.actor ! Arrived(1, r4)
    model.onArrived(r4, frozen)
    f.actor ! Arrived(1, r5)
    model.onArrived(r5, frozen)
    f.actor ! Finished(1, 104, compiled = true, 3, 3)
    model.onFinished(104, compiled = true, 3, 3, frozen)
    f.actor ! Finished(1, 105, compiled = true, 3, 3)
    model.onFinished(105, compiled = true, 3, 3, frozen)
    f.barrier()
    assert(model.board(Real) != model.board(Freezing))
    Seq(pub, t1, t2).foreach(drain)

    f.clock.at = expose.plusSeconds(1)
    f.actor ! Tick
    model.onTick(expose.plusSeconds(1))
    val real = LiveJson.board(model.board(Real))
    assert(real == LiveJson.board(model.board(Freezing)))
    Seq(pub, t1, t2).foreach(p => assert(next(p, "board") == real))
    Seq(pub, t1, t2).foreach(_.expectNoMessage(200.millis))
    Seq(JoinPublic(1), JoinTeam(1, 1), JoinTeam(1, 2)).foreach(m => assert(next(f.subscribe(m), "board") == real))
    f.stop()
  }

  test("the static file is the freezing board mid-freeze and never shows a real-only cell") {
    val render = (_: org.stingray.contester.dbmodel.Contest, b: LiveBoard) => LiveJson.board(b).toString
    seeds.foreach { s =>
      val rnd = new Random(s)
      val dir = java.nio.file.Files.createTempDirectory("live-leak-static").toFile
      val model = new LiveContest(contest(), problems, teams, initial, frozen)
      val f = new Fixture(frozen, subs = Map(1 -> initial), staticDir = Some(dir), render = render)
      f.barrier()
      val file = new java.io.File(dir, "1.html")
      var reals = Vector(model.board(Real))
      var frozens = Vector(model.board(Freezing))
      var seen = Vector.empty[String]
      var known = initial.map(_.id)
      var nextId = 100
      var at = t0.plusHours(3)
      (1 to steps).foreach { _ =>
        val roll = rnd.nextInt(10)
        val d =
          if (roll < 4) {
            at = Seq(at.plusSeconds(60 + rnd.nextInt(600)), frozen).minBy(_.getMillis)
            val r = rec(nextId, 1 + rnd.nextInt(3), problems(rnd.nextInt(3)), at)
            nextId += 1
            known :+= r.id
            f.actor ! Arrived(1, r)
            Some(model.onArrived(r, frozen))
          } else if (roll < 8) {
            val id = known(rnd.nextInt(known.size))
            val passed = if (rnd.nextBoolean()) 3 else rnd.nextInt(3)
            f.actor ! Finished(1, id, compiled = true, passed, 3)
            model.onFinished(id, compiled = true, passed, 3, frozen)
          } else {
            val id = known(rnd.nextInt(known.size))
            f.actor ! RejudgeRequested(id)
            model.onRejudge(id, frozen)
          }
        reals :+= model.board(Real)
        frozens :+= model.board(Freezing)
        f.clock.ms += 31000
        f.actor ! StaticCheck
        f.barrier()
        if (d.exists(_.freezing.nonEmpty) || file.exists()) {
          assert(file.exists(), s"seed $s")
          val text = new String(java.nio.file.Files.readAllBytes(file.toPath), "UTF-8")
          assert(text == render(contest(), model.board(Freezing)), s"seed $s")
          seen :+= text
        }
      }
      val realOnly = shownBy(reals) -- shownBy(frozens)
      assert(realOnly.nonEmpty, s"seed $s: some cell must exist only on the real board")
      assert(seen.nonEmpty && seen.distinct.size > 3, s"seed $s: the file must have been rewritten")
      seen.foreach(t => assert(cellsIn(Event(t, None, Some("board"))).toSet.intersect(realOnly).isEmpty, s"seed $s"))
      f.stop()
      dir.listFiles().foreach(_.delete())
      dir.delete()
    }
  }

  test("kill switch: the config key leaves no live actor, no-op hooks, and starts the old monitor") {
    import play.api.Configuration
    def cfg(s: String) = LiveMonitorConfig(Configuration(ConfigFactory.parseString(s)))
    var started = 0
    val off = cfg("monitor.disable_new_monitor = true")
    val built = LiveWiring.build(off, () => { started += 1; akka.actor.ActorRef.noSender })
    assert(built.hooks eq NoopLiveHooks)
    assert(built.monitor.isEmpty && started == 0)
    assert(LiveWiring.startOldMonitor(off))

    val on = cfg("monitor.disable_new_monitor = false")
    val builtOn = LiveWiring.build(on, () => { started += 1; akka.actor.ActorRef.noSender })
    assert(builtOn.monitor.isDefined && started == 1)
    assert(!LiveWiring.startOldMonitor(on))
    assert(!LiveWiring.startOldMonitor(cfg("")))
  }
}
