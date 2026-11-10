package live

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite

class LiveCellsSpec extends AnyFunSuite {
  private val start = new DateTime(2026, 11, 1, 10, 0)
  private val rules = FreezeRules(start.plusHours(4), start.plusHours(5), start.plusHours(6))
  private val during = start.plusHours(2)

  private def rec(id: Int, at: DateTime, state: SubmitState, sec: Int = 0) =
    SubmitRec(id, 1, "A", at, sec, state)
  private val t1 = start.plusHours(1)
  private val fail = Done(compiled = true, 1, 2)
  private val ok = Done(compiled = true, 3, 3)

  private def cell(recs: Seq[SubmitRec], kind: BoardKind = Real, now: DateTime = during) =
    LiveCells.cellOf(recs, kind, rules, now)

  test("empty and pending") {
    assert(cell(Nil) == LiveCell.Empty)
    val p = cell(Seq(rec(1, t1, Pending)))
    assert(p == LiveCell.Pending(0)); assert(p.text == "?1")
    val q = cell(Seq(rec(1, t1, fail), rec(2, t1.plusMinutes(1), Pending)))
    assert(q == LiveCell.Pending(1)); assert(q.text == "?2")
  }

  test("failures follow ACMScorer") {
    val f = cell(Seq(rec(1, t1, fail)))
    assert(f == LiveCell.Failed(1)); assert(f.text == "-1")
    assert(cell(Seq(rec(1, t1, Done(true, 0, 1)))) == LiveCell.Empty)
    assert(cell(Seq(rec(1, t1, Done(false, 0, 0)))) == LiveCell.Empty)
  }

  test("solved ignores later records") {
    val s = cell(Seq(rec(1, t1, fail), rec(2, t1.plusMinutes(1), ok, 600), rec(3, t1.plusMinutes(2), Pending)))
    assert(s == LiveCell.Solved(2, 600)); assert(s.text == "+1"); assert(s.solved)
  }

  test("first pending decides, even out of order") {
    assert(cell(Seq(rec(2, t1.plusMinutes(1), ok), rec(1, t1, Pending))) == LiveCell.Pending(0))
  }

  test("same arrival time orders by id") {
    assert(cell(Seq(rec(6, t1, ok, 5), rec(5, t1, fail))) == LiveCell.Solved(2, 5))
    assert(cell(Seq(rec(5, t1, ok, 5), rec(6, t1, fail))) == LiveCell.Solved(1, 5))
  }

  test("freeze hides on freezing board only") {
    val late = rec(1, start.plusHours(4).plusMinutes(30), ok, 15000)
    val now = start.plusHours(5).plusMinutes(30)
    assert(cell(Seq(late), Freezing, now) == LiveCell.Pending(0))
    assert(cell(Seq(late), Real, now).isInstanceOf[LiveCell.Solved])
    // frozen: every post-freeze submit counts as a try, after the visible failure
    val l2 = rec(2, start.plusHours(4).plusMinutes(40), fail)
    val l3 = rec(3, start.plusHours(4).plusMinutes(50), Pending)
    val pre = rec(0, start.plusHours(1), fail)
    val f = cell(Seq(pre, late, l2, l3), Freezing, now)
    assert(f == LiveCell.Pending(1, 3)); assert(f.text == "?4")
    assert(cell(Seq(pre, late, l2, l3), Real, now) == LiveCell.Solved(2, 15000))
    assert(cell(Seq(late), Freezing, start.plusHours(6)).isInstanceOf[LiveCell.Solved])
    assert(cell(Seq(late), Real, start.plusHours(6)).isInstanceOf[LiveCell.Solved])
    val early = rec(2, start.plusHours(3), ok, 100)
    assert(cell(Seq(early), Freezing, now).isInstanceOf[LiveCell.Solved])
  }

  test("freeze equal to end never hides") {
    val r = FreezeRules(start.plusHours(5), start.plusHours(5), start.plusHours(6))
    assert(!r.hides(start.plusHours(5).plusMinutes(30), start.plusHours(5)))
  }
}
