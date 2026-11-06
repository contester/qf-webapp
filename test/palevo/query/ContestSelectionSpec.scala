package palevo.query

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Contest

class ContestSelectionSpec extends AnyFunSuite {
  private val now = new DateTime(2026, 11, 6, 12, 0, 0)
  private def h(n: Int) = now.plusHours(n)

  private def contest(id: Int, start: Int, freeze: Int, end: Int, expose: Int) =
    Contest(id, s"c$id", h(start), h(freeze), h(end), h(expose), None, "en", schoolMode = false)

  test("freezeInEffect rules") {
    // freeze >= end: never
    assert(!ContestSelection.freezeInEffect(contest(1, -5, 2, 2, 10), now))
    assert(!ContestSelection.freezeInEffect(contest(1, -5, 3, 2, 10), now))
    // before freeze
    assert(!ContestSelection.freezeInEffect(contest(1, -5, 1, 3, 10), now))
    // in effect (during freeze and after end, before expose)
    assert(ContestSelection.freezeInEffect(contest(1, -5, -1, 3, 10), now))
    assert(ContestSelection.freezeInEffect(contest(1, -5, -3, -1, 10), now))
    // now == freezeTime counts
    assert(ContestSelection.freezeInEffect(contest(1, -5, 0, 3, 10), now))
    // now == exposeTime: over
    assert(!ContestSelection.freezeInEffect(contest(1, -5, -3, -1, 0), now))
    // after expose
    assert(!ContestSelection.freezeInEffect(contest(1, -5, -3, -2, -1), now))
  }

  test("viewOf for both audiences") {
    val inEffect = contest(1, -5, -1, 3, 10)
    val notInEffect = contest(2, -5, 1, 3, 10)
    assert(ContestSelection.viewOf(inEffect, Public, now) == Frozen)
    assert(ContestSelection.viewOf(inEffect, Jury, now) == LiveJury)
    assert(ContestSelection.viewOf(notInEffect, Public, now) == NoFreeze)
    assert(ContestSelection.viewOf(notInEffect, Jury, now) == NoFreeze)
    assert(Seq(Frozen, LiveJury, NoFreeze).map(_.json) == Seq("frozen", "live-jury", "no-freeze"))
  }

  test("status") {
    assert(ContestSelection.status(contest(1, 1, 2, 3, 4), now) == "before")
    assert(ContestSelection.status(contest(1, 0, 2, 3, 4), now) == "running")
    assert(ContestSelection.status(contest(1, -2, 2, 3, 4), now) == "running")
    assert(ContestSelection.status(contest(1, -3, -2, 0, 4), now) == "over")
    assert(ContestSelection.status(contest(1, -3, -2, -1, 4), now) == "over")
  }

  private val all = Scope(Public, None)
  private def sel(cs: Seq[Contest], scope: Scope = all, allowed: Set[Int] = Set(1, 2, 3, 4, 5), explicit: Option[Int] = None) =
    ContestSelection.select(cs, scope, allowed, now, explicit)
  private def ids(r: Either[String, Seq[Contest]]) = r.map(_.map(_.id))

  test("scope * intersected with allowed excludes other ids") {
    val cs = Seq(contest(1, -2, 1, 2, 3), contest(11, -2, 1, 2, 3))
    assert(ids(sel(cs)) == Right(Seq(1)))
  }

  test("several running contests are all returned in id order") {
    val cs = Seq(contest(3, -2, 1, 2, 3), contest(1, -1, 1, 2, 3), contest(2, -9, -8, -7, -6))
    assert(ids(sel(cs)) == Right(Seq(1, 3)))
  }

  test("none running: the latest-ended finished one") {
    val cs = Seq(contest(1, -9, -8, -7, -6), contest(2, -5, -4, -2, -1), contest(3, -6, -5, -3, -1), contest(4, 2, 3, 4, 5))
    assert(ids(sel(cs)) == Right(Seq(2)))
  }

  test("finished tie on end time: highest id") {
    val cs = Seq(contest(1, -9, -8, -2, -1), contest(2, -9, -8, -2, -1))
    assert(ids(sel(cs)) == Right(Seq(2)))
  }

  test("none finished: the soonest upcoming, tie lowest id") {
    val cs = Seq(contest(1, 5, 6, 7, 8), contest(2, 2, 3, 4, 5), contest(3, 2, 3, 4, 5))
    assert(ids(sel(cs)) == Right(Seq(2)))
  }

  test("no candidates gives empty") {
    assert(sel(Nil) == Right(Nil))
    assert(sel(Seq(contest(11, -2, 1, 2, 3))) == Right(Nil))
    assert(sel(Seq(contest(1, -2, 1, 2, 3)), scope = Scope(Public, Some(Set(2)))) == Right(Nil))
  }

  test("scope restricts candidates") {
    val cs = Seq(contest(1, -2, 1, 2, 3), contest(2, -2, 1, 2, 3))
    assert(ids(sel(cs, scope = Scope(Jury, Some(Set(2))))) == Right(Seq(2)))
  }

  test("explicit narrows to a candidate, even a non-current one") {
    val cs = Seq(contest(1, -2, 1, 2, 3), contest(2, -9, -8, -7, -6))
    assert(ids(sel(cs, explicit = Some(2))) == Right(Seq(2)))
  }

  test("explicit that is not allowed, not in scope or nonexistent is Left") {
    val cs = Seq(contest(1, -2, 1, 2, 3), contest(11, -2, 1, 2, 3), contest(2, -2, 1, 2, 3))
    assert(sel(cs, explicit = Some(11)).isLeft)
    assert(sel(cs, scope = Scope(Public, Some(Set(1))), explicit = Some(2)).isLeft)
    assert(sel(cs, explicit = Some(99)).isLeft)
  }
}
