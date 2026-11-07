package actors

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Contest
import palevo.{ContestStarting, PContest, PalevoEvent}

import scala.util.{Failure, Success}

class StatusActorTimeEventsSpec extends AnyFunSuite {
  private val now = new DateTime(2026, 11, 1, 10, 0)
  private val c = Contest(1, "C", now, now.plusHours(4), now.plusHours(5), now.plusHours(6), None, "en", schoolMode = false)
  private val event: PalevoEvent = ContestStarting(PContest(1, "C"), now)

  test("with the switch off the tracker is not asked at all") {
    var asked = 0
    val out = StatusActor.timeEventsOf(enabled = false, (_, _) => { asked += 1; Seq(event) }, c, now)
    assert(out == Success(Nil) && asked == 0)
  }

  test("with the switch on the tracker's events come back") {
    assert(StatusActor.timeEventsOf(enabled = true, (_, _) => Seq(event), c, now) == Success(Seq(event)))
  }

  test("a tracker that throws is reported, not propagated") {
    val boom = new IllegalStateException("boom")
    assert(StatusActor.timeEventsOf(enabled = true, (_, _) => throw boom, c, now) == Failure(boom))
  }
}
