package live

import java.util.concurrent.atomic.AtomicLong

import actors.LiveMonitorActor._
import akka.NotUsed
import akka.actor.ActorSystem
import akka.pattern.ask
import akka.stream.scaladsl.{Sink, Source}
import akka.testkit.TestKit
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.{LocalTeam, Team}
import play.api.libs.EventSource.Event

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * How much the live actor pays for many open public streams (skipped unless LIVE_BENCH is set):
  *   LIVE_BENCH=1000,10000,50000 sbt -J-Xmx8g "testOnly live.LiveFanoutBenchSpec"
  * No HTTP is involved: each stream is the Source the actor hands out, drained by a counting sink, so this measures
  * the actor and the per-stream cost, not the network or the proxy.
  */
class LiveFanoutBenchSpec extends TestKit(ActorSystem("LiveFanoutBench", ConfigFactory.parseString(
  """
    |akka.loglevel = WARNING
    |akka.stdout-loglevel = OFF
    |akka.use-slf4j = off
    |""".stripMargin))) with AnyFunSuiteLike with BeforeAndAfterAll with LiveActorFixtures {

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private implicit val ec: ExecutionContext = system.dispatcher
  private implicit val timeout: Timeout = 60.seconds
  private val sizes = sys.env.get("LIVE_BENCH").toSeq.flatMap(_.split(",")).map(_.trim.toInt)

  private val benchProblems = ('A' to 'J').map(_.toString)
  private val benchTeams: Seq[Team] = (1 to 160).map(i =>
    LocalTeam(i, 1, f"School $i%03d with a longish name", s"S$i", i % 7, "", notRated = false, noPrint = false, disabled = false))
  private val benchSubs = for {
    t <- 1 to 160
    (p, i) <- benchProblems.zipWithIndex
    if (t + i) % 3 != 0
  } yield rec(t * 100 + i, t, p, t0.plusMinutes(10 + (t * 7 + i * 13) % 200), Done(compiled = true, 3, if ((t + i) % 2 == 0) 3 else 2))

  private def usedMb(): Double = {
    (1 to 3).foreach { _ => System.gc(); Thread.sleep(200) }
    val r = Runtime.getRuntime
    (r.totalMemory - r.freeMemory) / 1048576.0
  }

  private def waitFor(c: AtomicLong, target: Long, what: String): Long = {
    val t = System.nanoTime()
    while (c.get < target) {
      if (System.nanoTime() - t > 180e9) fail(s"$what: only ${c.get} of $target after 180 s")
      Thread.sleep(1)
    }
    (System.nanoTime() - t) / 1000000
  }

  for (n <- sizes) test(s"fan-out to $n public streams") {
    val f = new Fixture(running, subs = Map.empty,
      overrideData = Map(1 -> ContestData(benchProblems, benchTeams, benchSubs)))
    f.barrier()

    val boards, rows, others, boardBytes = new AtomicLong
    val sink = Sink.foreach[Event] { e =>
      e.name match {
        case Some("board") => boards.incrementAndGet(); boardBytes.addAndGet(e.data.length)
        case Some("row") => rows.incrementAndGet()
        case _ => others.incrementAndGet()
      }
    }

    val before = usedMb()
    val t0n = System.nanoTime()
    benchmarkJoin(f, n, sink)
    val joinMs = waitFor(boards, n, "initial boards")
    val joinedMs = (System.nanoTime() - t0n) / 1000000
    val after = usedMb()
    val boardKb = boardBytes.get / n / 1024.0

    // a single row change, then the whole board (a team edit rebuilds both), several times each
    def timed(trigger: () => Unit, counter: AtomicLong): (Long, Long) = {
      val base = counter.get
      val start = System.nanoTime()
      trigger()
      val blocked = Await.result(f.actor.ask(JoinAdmin(1, real = true)), 60.seconds)
      val blockedMs = (System.nanoTime() - start) / 1000000
      val allMs = waitFor(counter, base + n, "delivery")
      (blockedMs, (System.nanoTime() - start) / 1000000 max allMs)
    }

    val rowRuns = (1 to 5).map { i =>
      timed(() => f.actor ! Arrived(1, rec(900000 + i, 3 * i, "A", running.plusMinutes(i))), rows)
    }
    val boardRuns = (1 to 3).map { i =>
      timed(() => f.actor ! TeamChanged(1, benchTeams(i).asInstanceOf[Team]), boards)
    }

    println(f"BENCH n=$n%6d  join all: $joinedMs%6d ms (${joinedMs * 1000.0 / n}%.0f us each)  heap: ${(after - before) * 1024 / n}%.1f KB/stream (${after - before}%.0f MB)  " +
      f"board event: $boardKb%.1f KB")
    println(f"BENCH n=$n%6d  row change   : actor busy ${rowRuns.map(_._1).mkString("/")} ms, last delivery ${rowRuns.map(_._2).mkString("/")} ms")
    println(f"BENCH n=$n%6d  whole board  : actor busy ${boardRuns.map(_._1).mkString("/")} ms, last delivery ${boardRuns.map(_._2).mkString("/")} ms " +
      f"(${boardKb * n / 1024.0}%.0f MB queued)")
    assert(others.get == 0, "unexpected events (pings?)")
    f.stop()
    Thread.sleep(2000)
  }

  private def benchmarkJoin(f: Fixture, n: Int, sink: Sink[Event, _]): Unit = {
    import akka.stream.Materializer
    val mat = Materializer.matFromSystem(system)
    (0 until n).grouped(500).foreach { chunk =>
      val srcs = Future.sequence(chunk.map(_ => f.actor.ask(JoinPublic(1)).mapTo[Source[Event, NotUsed]]))
      Await.result(srcs, 120.seconds).foreach(_.runWith(sink)(mat))
    }
  }
}
