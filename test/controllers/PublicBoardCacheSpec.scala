package controllers

import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.testkit.TestKit
import com.typesafe.config.ConfigFactory
import live.SubmitRec
import models._
import org.joda.time.DateTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.{Contest, Team}
import play.api.http.{DefaultFileMimeTypes, FileMimeTypesConfiguration}
import play.api.i18n.{DefaultLangs, DefaultMessagesApi, Lang}
import play.api.libs.typedmap.TypedMap
import play.api.mvc._
import play.api.mvc.request.{RemoteConnection, RequestFactory, RequestTarget}

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

/**
  * The public board page of a round that is over is rendered once and kept until a notification can have changed it;
  * a running round's board is never rendered, cached or served; nothing expires by time.
  */
class PublicBoardCacheSpec extends TestKit(ActorSystem("PublicBoardCacheSpec", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |akka.use-slf4j = off
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private implicit val mat: Materializer = Materializer(system)
  private implicit val ec: ExecutionContext = system.dispatcher

  private val cc: ControllerComponents = {
    val parsers = PlayBodyParsers()
    DefaultControllerComponents(DefaultActionBuilder(parsers.defaultBodyParser), parsers, new DefaultMessagesApi(),
      new DefaultLangs(Seq(Lang("en"))), new DefaultFileMimeTypes(FileMimeTypesConfiguration()), ec)
  }

  private val start = new DateTime(2030, 1, 1, 10, 0)
  private val end = start.plusHours(3)
  private val runningAt = start.plusHours(1)
  private val afterEnd = end.plusMinutes(1)

  private class Rig {
    @volatile var rounds: Map[Int, Contest] = Map(5 -> Contest(5, "R5", start, end, None, 1), 6 -> Contest(6, "R6", start, end, None, 1))
    @volatile var points: Double = 20
    val renders = new AtomicInteger
    @volatile var gate: Future[Unit] = Future.unit
    val cache = new PublicBoardCache
    val source = BoardSource(id => Future.successful(rounds.get(id)), id => {
      renders.incrementAndGet()
      val p = points
      gate.map(_ => MonitorPoints(Seq(ProblemColumn("A", "Alpha", 20)), Seq(
        RankedRow(Some(1), Team(11, 7, s"Team of round $id", "Lyceum", "Town", None), Seq(PointsCell(Some(p), 1, full = p == 20)), p))))
    })
    val controller = new PublicMonitor(cc, source, cache)

    def pageF(id: Int, now: DateTime): Future[(Int, String)] = {
      val path = s"/public/monitor/$id"
      val req = RequestFactory.plain.createRequest(RemoteConnection("127.0.0.1", secure = false, None), "GET",
        RequestTarget(path, path, Map.empty), "HTTP/1.1", Headers(), TypedMap.empty, AnyContentAsEmpty)
      controller.pageAt(id, () => now)(req).flatMap(r => r.body.consumeData.map(b => (r.header.status, b.utf8String)))
    }

    def page(id: Int, now: DateTime): (Int, String) = Await.result(pageF(id, now), 5.seconds)
  }

  test("a cache hit does not render again") {
    val rig = new Rig
    val (s1, b1) = rig.page(5, afterEnd)
    val (s2, b2) = rig.page(5, afterEnd.plusDays(30)) // no time-based expiry, however late
    assert(s1 == 200 && s2 == 200 && b1 == b2 && b1.contains(">20<"))
    assert(rig.renders.get == 1)
    assert(rig.cache.cachedRounds == Set(5))
  }

  private val rec = SubmitRec(1, 11, "A", start, 0, live.Pending)

  private val notifications: Seq[(String, PublicBoardCache => Unit)] = Seq(
    "testing_finished (FinishedTesting)" -> (_.hooks.finished(5, 1, compiled = true, Some(7.0))),
    "rejudge reset" -> (_.hooks.rejudged(1)),
    "TeamsChanged (credential or participant)" -> (_.hooks.teamsChanged(5)),
    "ContestChanged (round_changed)" -> (_.hooks.contestChanged(Contest(5, "R5", start, end, None, 1))),
    "ContestRemoved" -> (_.hooks.contestRemoved(5)),
    "submit arrival" -> (_.hooks.submitArrived(5, rec)))

  for ((name, notify) <- notifications) test(s"invalidation by $name forces a re-render with the new data") {
    val rig = new Rig
    rig.page(5, afterEnd)
    rig.page(6, afterEnd)
    assert(rig.renders.get == 2)
    rig.points = 7
    notify(rig.cache)
    val (status, body) = rig.page(5, afterEnd)
    assert(status == 200 && body.contains(">7<"), body)
    assert(rig.renders.get == 3)
  }

  test("a notification for another round keeps this round's page") {
    val rig = new Rig
    rig.page(5, afterEnd)
    rig.cache.hooks.finished(6, 1, compiled = true, Some(1.0))
    rig.cache.hooks.teamsChanged(6)
    rig.page(5, afterEnd)
    assert(rig.renders.get == 1)
  }

  test("a running round's board is never rendered, cached or served") {
    val rig = new Rig
    val (status, body) = rig.page(5, runningAt)
    assert(status == 403 && body.contains("monitor.notPublished") && rig.renders.get == 0)
    assert(rig.cache.cachedRounds.isEmpty)
    // Cached after the end; the round is then reopened (ends later): the cached page is not served.
    rig.page(5, afterEnd)
    assert(rig.cache.cachedRounds == Set(5))
    rig.rounds = rig.rounds.updated(5, Contest(5, "R5", start, end.plusHours(2), None, 1))
    val (again, _) = rig.page(5, afterEnd)
    assert(again == 403 && rig.renders.get == 1)
  }

  test("a render that was running when its round was dropped is served but not stored") {
    val rig = new Rig
    val gate = Promise[Unit]()
    rig.gate = gate.future
    val pending = Future(rig.page(5, afterEnd))
    awaitCond(rig.renders.get == 1, 5.seconds)
    rig.cache.hooks.finished(5, 1, compiled = true, Some(7.0))
    gate.success(())
    assert(Await.result(pending, 5.seconds)._1 == 200)
    assert(rig.cache.cachedRounds.isEmpty)
    rig.gate = Future.unit
    rig.page(5, afterEnd)
    assert(rig.renders.get == 2)
  }

  // ---- single flight ----

  test("50 concurrent misses share exactly one render") {
    val rig = new Rig
    val gate = Promise[Unit]()
    rig.gate = gate.future
    val all = (1 to 50).map(_ => rig.pageF(5, afterEnd))
    awaitCond(rig.renders.get == 1, 5.seconds)
    Thread.sleep(200)
    assert(rig.renders.get == 1)
    gate.success(())
    val got = Await.result(Future.sequence(all), 10.seconds)
    assert(got.forall(_._1 == 200) && got.map(_._2).distinct.size == 1)
    assert(rig.renders.get == 1 && rig.cache.cachedRounds == Set(5))
  }

  test("a request after an invalidation during an in-flight render starts a new render; the stale one is not stored") {
    val rig = new Rig
    val gate = Promise[Unit]()
    rig.gate = gate.future
    val stale = rig.pageF(5, afterEnd)
    awaitCond(rig.renders.get == 1, 5.seconds)
    rig.points = 7
    rig.cache.hooks.finished(5, 1, compiled = true, Some(7.0))
    rig.gate = Future.unit
    val (s2, fresh) = rig.page(5, afterEnd)
    assert(s2 == 200 && fresh.contains(">7<") && rig.renders.get == 2)
    gate.success(())
    val (s1, old) = Await.result(stale, 5.seconds)
    assert(s1 == 200 && old.contains(">20<"), "a caller that was already waiting gets the older render")
    // The newer page is the one kept: no third render, and it shows 7.
    val (_, again) = rig.page(5, afterEnd)
    assert(again.contains(">7<") && rig.renders.get == 2)
  }

  test("a failed render is not cached: the next request renders again") {
    val cache = new PublicBoardCache
    val calls = new AtomicInteger
    val failed = cache.getOrRender(5, "en") { calls.incrementAndGet(); Future.failed(new RuntimeException("db down")) }
    assert(Await.ready(failed, 5.seconds).value.exists(_.isFailure))
    awaitCond(cache.entryRounds.isEmpty, 5.seconds)
    assert(Await.result(cache.getOrRender(5, "en") { calls.incrementAndGet(); Future.successful("page") }, 5.seconds) == "page")
    assert(calls.get == 2 && cache.cachedRounds == Set(5))
    // A render that throws instead of returning a failed future is a failure too.
    val thrown = cache.getOrRender(6, "en") { calls.incrementAndGet(); throw new IllegalStateException("boom") }
    assert(Await.ready(thrown, 5.seconds).value.exists(_.isFailure))
    awaitCond(!cache.entryRounds.contains(6), 5.seconds)
  }

  test("invalidate-all (a rejudge) drops the in-flight entries of every round") {
    val cache = new PublicBoardCache
    val calls = new AtomicInteger
    val p5 = Promise[String]()
    val p6 = Promise[String]()
    cache.getOrRender(5, "en") { calls.incrementAndGet(); p5.future }
    cache.getOrRender(6, "ru") { calls.incrementAndGet(); p6.future }
    assert(cache.entryRounds == Set(5, 6))
    cache.hooks.rejudgedAll(Seq(1, 2, 3))
    assert(cache.entryRounds.isEmpty)
    p5.success("old 5")
    p6.success("old 6")
    assert(Await.result(cache.getOrRender(5, "en") { calls.incrementAndGet(); Future.successful("new 5") }, 5.seconds) == "new 5")
    assert(calls.get == 3)
    awaitCond(cache.cachedRounds == Set(5), 5.seconds)
    cache.hooks.rejudged(9)
    assert(cache.entryRounds.isEmpty)
  }

  test("a problem rejudge drops the cache once per request, not once per submit") {
    var drops = 0
    val counting = new live.LiveHooks {
      override def submitArrived(contest: Int, rec: SubmitRec): Unit = ()
      override def finished(contest: Int, id: Int, compiled: Boolean, points: Option[Double]): Unit = ()
      override def rejudged(id: Int): Unit = drops += 1
      override def rejudgedAll(ids: Seq[Int]): Unit = drops += 1
      override def teamsChanged(roundId: Int): Unit = ()
      override def contestChanged(contest: Contest): Unit = ()
      override def contestRemoved(roundId: Int): Unit = ()
    }
    live.LiveHooks.all(counting).rejudgedAll(Seq(1, 2, 3))
    live.LiveWiring.deferred(counting).rejudgedAll(Seq(4, 5))
    assert(drops == 2)
    val admin = new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get("app/controllers/AdminApplication.scala")), "UTF-8")
    assert(admin.contains("liveHooks.rejudgedAll(ids.map(_.toInt))") && !admin.contains("ids.foreach(id => Try(liveHooks.rejudged"))
  }

  test("the cache is fed only by the notification hooks and has no timer") {
    val src = new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get("app/models/PublicBoardCache.scala")), "UTF-8")
    Seq("schedule", "Timer", "tick", "System.currentTimeMillis", "DateTime.now").foreach { w =>
      assert(!src.contains(w), w)
    }
    val loader = new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get("app/MyApplicationLoader.scala")), "UTF-8")
    assert(loader.contains("live.LiveHooks.all(new live.ActorLiveHooks(liveMonitor), publicBoardCache.hooks)"))
    assert(loader.contains("statusActorModel.contestChanged, statusActorModel.contestRemoved, statusActorModel.teamsChanged"))
  }
}
