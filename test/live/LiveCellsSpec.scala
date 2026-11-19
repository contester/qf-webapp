package live

import models.PointsScorer
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite

/** The live cell is the points board's cell (PointsScorer) plus the in-flight count and the time of the full score. */
class LiveCellsSpec extends AnyFunSuite {
  private val t0 = new DateTime(2026, 11, 1, 10, 0)

  private def rec(id: Int, minute: Int, state: SubmitState): SubmitRec =
    SubmitRec(id, 1, "A", t0.plusMinutes(minute), minute * 60, state)

  private def pts(p: Double) = Done(compiled = true, Some(p))

  private def same(recs: Seq[SubmitRec], max: Double): Unit =
    assert(LiveCells.cellOf(recs, max).points == PointsScorer.cell(recs.map(_.toSubmit), max), recs)

  test("empty") {
    assert(LiveCells.cellOf(Nil, 10) == LiveCell.Empty)
    assert(LiveCell.Empty.isEmpty && LiveCell.Empty.points == models.PointsCell.empty)
  }

  test("best of, not a sum; full at the maximum; fullAt is the earliest full submit") {
    val recs = Seq(rec(1, 5, pts(4)), rec(2, 9, pts(10)), rec(3, 12, pts(10)), rec(4, 20, pts(7)))
    assert(LiveCells.cellOf(recs, 10) == LiveCell(Some(10.0), 4, full = true, 0, Some(540)))
    same(recs, 10)
  }

  test("in flight: counted as attempts and pending, never the best") {
    val recs = Seq(rec(1, 5, pts(4)), rec(2, 9, live.Pending), rec(3, 10, Done(compiled = true, None)))
    assert(LiveCells.cellOf(recs, 10) == LiveCell(Some(4.0), 3, full = false, 2, None))
    same(recs, 10)
  }

  test("a compile error is an attempt, neither best nor in flight; a 0 maximum is never full") {
    val recs = Seq(rec(1, 5, Done(compiled = false, None)), rec(2, 6, Done(compiled = false, Some(3.0))))
    assert(LiveCells.cellOf(recs, 10) == LiveCell(None, 2, full = false, 0, None))
    same(recs, 10)
    assert(!LiveCells.cellOf(Seq(rec(3, 1, pts(0))), 0).full)
    same(Seq(rec(3, 1, pts(0))), 0)
  }
}
