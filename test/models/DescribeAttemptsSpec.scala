package models

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Memory, TimeMs}

/**
  * A team's attempts on a problem are scored in arrival order, so rejudging an earlier attempt changes the score of
  * the later ones. The live submit table gets all of them re-rendered after each finished testing.
  */
class DescribeAttemptsSpec extends AnyFunSuite {
  private val t0 = new DateTime(2026, 11, 5, 10, 0)

  private def attempt(id: Int, seconds: Int, passed: Int, taken: Int = 5, testing: Int) =
    Submit(SubmitId(id, Arrived(t0.plusSeconds(seconds), seconds, afterFreeze = false), 1, 1, RatedProblem("A", 30), "cpp"),
      finished = true, compiled = true, passed, taken, Some(testing))

  private def wrongAnswer(testing: Int) =
    testing.toLong -> Seq(ResultEntry(testing, 3, 13, TimeMs(15), Memory(2048), 0, 0, Array.empty, Array.empty))

  private def accepted(testing: Int) =
    testing.toLong -> Seq(ResultEntry(testing, 1, 10, TimeMs(40), Memory(4096), 0, 0, Array.empty, Array.empty))

  private def row(fs: FullyDescribedSubmit, plus: String = "+") = views.html.singlesubmit(fs, plus).body

  private val results = Map(wrongAnswer(1), wrongAnswer(2), accepted(3))

  test("a late accepted attempt is scored after the failures before it") {
    val rows = Submits.describeAttempts(schoolMode = false,
      Seq(attempt(11, 600, 3, testing = 1), attempt(12, 1200, 3, testing = 2), attempt(13, 1800, 5, testing = 3)), results)
    assert(rows.map(_.index) == Seq(1, 2, 3))
    assert(rows.map(_.score.map(_.toString)) == Seq(None, None, Some("70")))
    assert(rows.map(_.result.message) == Seq("Wrong answer on test 3", "Wrong answer on test 3", "ACCEPTED"))
    assert(row(rows(2)).contains("+70") && row(rows(2)).contains("class=\"success\""))
  }

  test("rejudging the first attempt to accepted takes the score away from the later ones") {
    val rejudged = Map(accepted(4), wrongAnswer(2), accepted(3))
    val rows = Submits.describeAttempts(schoolMode = false,
      Seq(attempt(11, 600, 5, testing = 4), attempt(12, 1200, 3, testing = 2), attempt(13, 1800, 5, testing = 3)), rejudged)
    assert(rows.map(_.index) == Seq(1, 2, 3))
    assert(rows.map(_.score.map(_.toString)) == Seq(Some("10"), None, None))
    // the last attempt is accepted but no longer scores: its rendered row must change, not only the first one's
    assert(!row(rows(2)).contains("+70") && !row(rows(2)).contains("+10"))
    assert(row(rows.head).contains("+10"))
  }

  test("attempts are taken in arrival order whatever order they are given") {
    val rows = Submits.describeAttempts(schoolMode = false,
      Seq(attempt(13, 1800, 5, testing = 3), attempt(11, 600, 3, testing = 1), attempt(12, 1200, 3, testing = 2)), results)
    assert(rows.map(_.submit.submitId.id) == Seq(11, 12, 13))
  }

  test("time and memory come from the testing's results; a submit without results has none") {
    val rows = Submits.describeAttempts(schoolMode = false,
      Seq(attempt(11, 600, 5, testing = 3), attempt(12, 1200, 5, testing = 9)), results)
    assert(rows.head.stats.timeMs == TimeMs(40) && rows.head.stats.memory == Memory(4096))
    assert(rows(1).stats.timeMs == TimeMs(0) && rows(1).details.isEmpty)
  }

  test("school mode scores each attempt by the tests it passed") {
    val rows = Submits.describeAttempts(schoolMode = true,
      Seq(attempt(11, 600, 2, testing = 1), attempt(12, 1200, 5, testing = 3)), results)
    assert(rows.map(_.result.message) == Seq("2 из 5", "ACCEPTED"))
    assert(rows.forall(_.score.isDefined))
    assert(row(rows.head, plus = "").contains("<td id=\"score-11\">"))
  }
}
