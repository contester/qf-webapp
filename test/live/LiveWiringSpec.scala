package live

import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest
import org.scalatest.funsuite.AnyFunSuite

class LiveWiringSpec extends AnyFunSuite {
  private class Recording(name: String, calls: scala.collection.mutable.Buffer[String]) extends LiveHooks {
    override def submitArrived(contest: Int, rec: SubmitRec): Unit = calls += s"$name arrived $contest ${rec.id}"
    override def finished(contest: Int, id: Int, compiled: Boolean, points: Option[Double]): Unit =
      calls += s"$name finished $contest $id $compiled $points"
    override def rejudged(id: Int): Unit = calls += s"$name rejudged $id"
    override def teamsChanged(roundId: Int): Unit = calls += s"$name teams $roundId"
    override def contestChanged(contest: Contest): Unit = calls += s"$name contest ${contest.id}"
    override def contestRemoved(roundId: Int): Unit = calls += s"$name removed $roundId"
  }

  private def every(hooks: LiveHooks): Unit = {
    hooks.submitArrived(1, SubmitRec(5, 2, "A", new DateTime(0), 0, Pending))
    hooks.finished(1, 5, compiled = true, Some(2.5))
    hooks.rejudged(5)
    hooks.teamsChanged(1)
    hooks.contestChanged(Contest(3, "c", new DateTime(0), new DateTime(0), None, 1))
    hooks.contestRemoved(3)
  }

  private val expected = Seq("arrived 1 5", "finished 1 5 true Some(2.5)", "rejudged 5", "teams 1", "contest 3", "removed 3")

  test("ActorLiveHooks swallows an exception thrown by the send") {
    // A null ref makes `ref ! msg` throw a NullPointerException.
    every(new ActorLiveHooks(null))
  }

  test("deferred hooks resolve their target only on first use and forward every call") {
    var resolved = 0
    val calls = scala.collection.mutable.Buffer.empty[String]
    val hooks = LiveWiring.deferred { resolved += 1; new Recording("t", calls) }
    assert(resolved == 0)
    every(hooks)
    assert(resolved == 1)
    assert(calls == expected.map("t " + _))
  }

  test("deferred hooks swallow an exception from resolving or calling the target") {
    every(LiveWiring.deferred(throw new IllegalStateException("boom")))
  }

  test("LiveHooks.all forwards every call to each hook in turn, even when one of them throws") {
    val calls = scala.collection.mutable.Buffer.empty[String]
    every(LiveHooks.all(new Recording("a", calls), new ActorLiveHooks(null), new Recording("b", calls)))
    assert(calls == expected.flatMap(e => Seq("a " + e, "b " + e)))
  }
}
