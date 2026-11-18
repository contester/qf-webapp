package actors

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import org.joda.time.DateTime
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.{Contest, Problem, Team}
import org.stingray.qf.actors.{ProblemStateActor, TeamStateActor}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}

/** The state actors reload on change messages only, never overlapping: bursts coalesce and an older read never wins. */
class StateActorCoalescingSpec extends TestKit(ActorSystem("StateActorCoalescingSpec")) with AnyFunSuiteLike with BeforeAndAfterAll {
  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  /** Each load takes the next promise, which the test completes. */
  class Loads[T] {
    val promises = new ConcurrentLinkedQueue[Promise[T]]()
    val started = new AtomicInteger
    def next(): Future[T] = {
      started.incrementAndGet()
      val p = Promise[T]()
      promises.add(p)
      p.future
    }
    def complete(n: Int, v: T): Unit = {
      awaitCond(started.get >= n, 5.seconds)
      promises.toArray(Array.empty[Promise[T]])(n - 1).success(v)
    }
  }

  private val round = Contest(1, "R", DateTime.now, DateTime.now.plusHours(1), None, 1)
  private def problems(ids: String*): ProblemStateActor.ProblemState = Map(1 -> ids.map(id => Problem(1, id, id, 1)))

  private def problemIds(a: akka.actor.ActorRef): Seq[String] = {
    val p = TestProbe()
    p.send(a, ProblemStateActor.GetProblems(1))
    p.expectMsgType[Seq[Problem]](5.seconds).map(_.id)
  }

  test("ProblemStateActor: a burst of changes during a load gives exactly one more load, and the newest data wins") {
    val loads = new Loads[ProblemStateActor.ProblemState]
    val a = system.actorOf(ProblemStateActor.propsWith(_ => loads.next()))
    loads.complete(1, problems("A"))
    assert(problemIds(a) == Seq("A"))
    (1 to 10).foreach(_ => a ! StatusActor.ContestChanged(round))
    awaitCond(loads.started.get == 2, 5.seconds)
    (1 to 10).foreach(_ => a ! StatusActor.ContestChanged(round))
    assert(problemIds(a) == Seq("A"))
    assert(loads.started.get == 2, "no second load may overlap the running one")
    // The running (older) load finishes: applied, then exactly one more load for the changes that came during it.
    loads.complete(2, problems("A", "B"))
    awaitCond(loads.started.get == 3, 5.seconds)
    loads.complete(3, problems("A", "B", "C"))
    awaitCond(problemIds(a) == Seq("A", "B", "C"), 5.seconds, 20.millis)
    TestProbe().expectNoMessage(300.millis)
    assert(loads.started.get == 3)
    system.stop(a)
  }

  test("ProblemStateActor: a failed start-up load is retried on its own (retry-on-failure), then serves") {
    val calls = new AtomicInteger
    val a = system.actorOf(ProblemStateActor.propsWith { _ =>
      if (calls.incrementAndGet() == 1) Future.failed(new RuntimeException("db down")) else Future.successful(problems("Z"))
    })
    assert(problemIds(a) == Seq("Z")) // stashed until the retry, about 1 s later
    assert(calls.get == 2)
    system.stop(a)
  }

  private def team(id: Int) = Team(id, 0, "", s"S$id", "C", None)

  class TeamLoads extends TeamStateActor.Loader {
    val rounds = new Loads[Option[Seq[Team]]]
    override def all()(implicit ec: ExecutionContext): Future[TeamStateActor.TeamState] = Future.successful(Map(1 -> Map(1 -> team(1))))
    override def round(id: Int)(implicit ec: ExecutionContext): Future[Option[Seq[Team]]] = rounds.next()
  }

  private def teamIds(a: akka.actor.ActorRef): Set[Int] = {
    val p = TestProbe()
    p.send(a, TeamStateActor.GetTeams(1))
    p.expectMsgType[Map[Int, Team]](5.seconds).keySet
  }

  test("TeamStateActor: per-round reloads never overlap; a burst gives one more read and a stale read cannot win") {
    val loader = new TeamLoads
    val a = system.actorOf(TeamStateActor.propsWith(loader))
    assert(teamIds(a) == Set(1))
    (1 to 10).foreach(_ => a ! StatusActor.TeamsChanged(1))
    awaitCond(loader.rounds.started.get == 1, 5.seconds)
    assert(teamIds(a) == Set(1))
    assert(loader.rounds.started.get == 1)
    loader.rounds.complete(1, Some(Seq(team(1), team(2)))) // older read
    awaitCond(loader.rounds.started.get == 2, 5.seconds)
    loader.rounds.complete(2, Some(Seq(team(3)))) // newer read
    awaitCond(teamIds(a) == Set(3), 5.seconds, 20.millis)
    TestProbe().expectNoMessage(300.millis)
    assert(loader.rounds.started.get == 2)
    system.stop(a)
  }

  test("TeamStateActor: a removal during a read drops the round and reads once more, so a stale read cannot bring it back") {
    val loader = new TeamLoads
    val a = system.actorOf(TeamStateActor.propsWith(loader))
    assert(teamIds(a) == Set(1))
    a ! StatusActor.TeamsChanged(1)
    awaitCond(loader.rounds.started.get == 1, 5.seconds)
    a ! StatusActor.ContestRemoved(1)
    assert(teamIds(a).isEmpty)
    loader.rounds.complete(1, Some(Seq(team(1)))) // read before the removal
    awaitCond(loader.rounds.started.get == 2, 5.seconds)
    loader.rounds.complete(2, None)
    TestProbe().expectNoMessage(200.millis)
    assert(teamIds(a).isEmpty)
    system.stop(a)
  }
}
