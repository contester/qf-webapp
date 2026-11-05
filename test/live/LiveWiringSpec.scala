package live

import akka.actor.ActorRef
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{Contest, LocalTeam, Team}
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.duration._

class LiveWiringSpec extends AnyFunSuite {
  private def cfg(enabled: Boolean) = LiveMonitorConfig(enabled, 30.seconds, 60.seconds, (1 to 10).toSet, 1000)

  test("disabled: no-op hooks, the actor factory is never called, the old monitor starts") {
    var started = 0
    val built = LiveWiring.build(cfg(enabled = false), () => { started += 1; ActorRef.noSender })
    assert(built.hooks eq NoopLiveHooks)
    assert(built.monitor.isEmpty)
    assert(started == 0)
    assert(LiveWiring.startOldMonitor(cfg(enabled = false)))
  }

  test("enabled: actor hooks on the started actor, the old monitor does not start") {
    var started = 0
    val built = LiveWiring.build(cfg(enabled = true), () => { started += 1; ActorRef.noSender })
    assert(built.hooks.isInstanceOf[ActorLiveHooks])
    assert(built.monitor.contains(ActorRef.noSender))
    assert(started == 1)
    assert(!LiveWiring.startOldMonitor(cfg(enabled = true)))
  }

  test("ActorLiveHooks swallows an exception thrown by the send") {
    // A null ref makes `ref ! msg` throw a NullPointerException.
    val hooks = new ActorLiveHooks(null)
    hooks.submitArrived(1, SubmitRec(1, 2, "A", new DateTime(0), 0, Pending))
    hooks.finished(1, 1, compiled = true, 3, 100)
    hooks.rejudged(1)
    hooks.teamChanged(1, LocalTeam(2, 1, "School", "S", 0, "", notRated = true, noPrint = false, disabled = false))
  }

  test("deferred hooks resolve their target only on first use and forward every call") {
    var resolved = 0
    val calls = scala.collection.mutable.Buffer.empty[String]
    val target = new LiveHooks {
      override def submitArrived(contest: Int, rec: SubmitRec): Unit = calls += s"arrived $contest ${rec.id}"
      override def finished(contest: Int, id: Int, compiled: Boolean, passed: Int, taken: Int): Unit =
        calls += s"finished $contest $id $compiled $passed $taken"
      override def rejudged(id: Int): Unit = calls += s"rejudged $id"
      override def teamChanged(contest: Int, team: Team): Unit = calls += s"team $contest ${team.id}"
      override def contestChanged(contest: Contest): Unit = calls += s"contest ${contest.id}"
    }
    val hooks = LiveWiring.deferred { resolved += 1; target }
    assert(resolved == 0)
    hooks.submitArrived(1, SubmitRec(5, 2, "A", new DateTime(0), 0, Pending))
    hooks.finished(1, 5, compiled = true, 3, 100)
    hooks.rejudged(5)
    hooks.teamChanged(1, LocalTeam(7, 1, "School", "S", 0, "", notRated = false, noPrint = false, disabled = false))
    hooks.contestChanged(Contest(3, "c", new DateTime(0), new DateTime(0), new DateTime(0), new DateTime(0), None, "ru", schoolMode = false))
    assert(resolved == 1)
    assert(calls == Seq("arrived 1 5", "finished 1 5 true 3 100", "rejudged 5", "team 1 7", "contest 3"))
  }

  test("deferred hooks swallow an exception from resolving or calling the target") {
    val hooks = LiveWiring.deferred(throw new IllegalStateException("boom"))
    hooks.submitArrived(1, SubmitRec(5, 2, "A", new DateTime(0), 0, Pending))
    hooks.finished(1, 5, compiled = true, 3, 100)
    hooks.rejudged(5)
    hooks.teamChanged(1, LocalTeam(7, 1, "School", "S", 0, "", notRated = false, noPrint = false, disabled = false))
    hooks.contestChanged(Contest(3, "c", new DateTime(0), new DateTime(0), new DateTime(0), new DateTime(0), None, "ru", schoolMode = false))
  }
}
