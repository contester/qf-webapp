package models

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Memory, TimeMs}

/** A team's attempts on a problem, numbered in arrival order; each row shows the attempt's own verdict and points. */
class DescribeAttemptsSpec extends AnyFunSuite {
  private val t0 = new DateTime(2026, 11, 5, 10, 0)

  private def attempt(id: Int, seconds: Int, passed: Int, taken: Int = 5, testing: Int) = {
    val points = if (passed == taken) 10.0 else 3.0

    Submit(SubmitId(id, Arrived(t0.plusSeconds(seconds), seconds, afterFreeze = false), 1, 1, RatedProblem("A", 30), "cpp"),
      finished = true, compiled = true, passed, taken, Some(testing), Some(points), 10)
  }

  private def wrongAnswer(testing: Int) =
    testing.toLong -> Seq(ResultEntry(testing, 3, 13, TimeMs(15), Memory(2048), 0, 0))

  private def accepted(testing: Int) =
    testing.toLong -> Seq(ResultEntry(testing, 1, 10, TimeMs(40), Memory(4096), 0, 0))

  private def row(fs: FullyDescribedSubmit) = views.html.singlesubmit(fs).body

  private val results = Map(wrongAnswer(1), wrongAnswer(2), accepted(3))

  test("attempts are numbered in arrival order and each shows its own points, with no ACM score") {
    val rows = Submits.describeAttempts(Seq(attempt(11, 600, 3, testing = 1), attempt(12, 1200, 3, testing = 2), attempt(13, 1800, 5, testing = 3)), results)
    assert(rows.map(_.index) == Seq(1, 2, 3))
    assert(rows.map(_.result.message) == Seq("3 из 10", "3 из 10", "10 из 10"))
    assert(row(rows(2)).contains("class=\"success\"") && !row(rows(2)).contains("+70") && !row(rows(2)).contains("score-"))
  }

  test("rejudging the first attempt changes only that attempt's row") {
    val rejudged = Map(accepted(4), wrongAnswer(2), accepted(3))
    val rows = Submits.describeAttempts(Seq(attempt(11, 600, 5, testing = 4), attempt(12, 1200, 3, testing = 2), attempt(13, 1800, 5, testing = 3)), rejudged)
    assert(rows.map(_.result.message) == Seq("10 из 10", "3 из 10", "10 из 10"))
    assert(row(rows(2)) == row(Submits.describeAttempts(Seq(attempt(13, 1800, 5, testing = 3)), results).head.copy(index = 3)))
  }

  test("attempts are taken in arrival order whatever order they are given") {
    val rows = Submits.describeAttempts(Seq(attempt(13, 1800, 5, testing = 3), attempt(11, 600, 3, testing = 1), attempt(12, 1200, 3, testing = 2)), results)
    assert(rows.map(_.submit.submitId.id) == Seq(11, 12, 13))
  }

  test("time and memory come from the testing's results; a submit without results has none") {
    val rows = Submits.describeAttempts(Seq(attempt(11, 600, 5, testing = 3), attempt(12, 1200, 5, testing = 9)), results)
    assert(rows.head.stats.timeMs == TimeMs(40) && rows.head.stats.memory == Memory(4096))
    assert(rows(1).stats.timeMs == TimeMs(0) && rows(1).details.isEmpty)
  }
}
