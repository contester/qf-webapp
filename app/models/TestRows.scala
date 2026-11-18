package models

import org.stingray.contester.dbmodel.{Memory, TimeMs}

import scala.math.BigDecimal.RoundingMode

/** One per-test result of a testing (test_no >= 1, or 0 for the compile result). */
case class ResultRow(testNo: Int, resultCode: Int, timeMs: TimeMs, memoryBytes: Memory, group: Option[String], points: Double)

/** One group of a testing. `policy` is `complete-group` or `each-test`. */
case class GroupRow(name: String, policy: String, pointsEarned: Double, pointsMax: Double, passed: Boolean, skipped: Boolean)

/** A row of the per-test table, text ready to show. */
case class TestRowView(testNo: Int, verdict: String, timeMs: TimeMs, memoryBytes: Memory, group: Option[String],
                       policy: Option[String], pointsText: String)

object TestRows {
  val Skipped = 18
  val Accepted = 10

  /** Up to two decimals, trailing zeros trimmed, '.' separator. */
  def formatPoints(x: Double): String =
    if (x.isNaN || x.isInfinite) "0" // never stored, but a page must not fail on one
    else BigDecimal(x).setScale(2, RoundingMode.HALF_UP).bigDecimal.stripTrailingZeros.toPlainString

  def fromEntry(e: ResultEntry): ResultRow = ResultRow(e.test, e.result, e.time, e.memory, e.group, e.points)

  /** The rows of the per-test table: ascending by test number, the compile row (test 0) left out. */
  def describe(results: Seq[ResultRow], groups: Seq[GroupRow]): Seq[TestRowView] = {
    val tests = results.filter(_.testNo != 0).sortBy(_.testNo)
    val byName = groups.map(g => g.name -> g).toMap
    val lastOf = tests.flatMap(t => t.group.map(_ -> t.testNo)).groupBy(_._1).mapValues(_.map(_._2).max)

    tests.map { t =>
      val group = t.group.flatMap(byName.get)
      val passed = t.resultCode == Accepted
      val text =
        if (t.resultCode == Skipped) "не запускался"
        else group match {
          case None => formatPoints(t.points)
          case Some(g) if g.policy == "each-test" =>
            if (passed) s"${formatPoints(t.points)} (тест пройден)" else "0 (тест не пройден)"
          case Some(g) if t.group.flatMap(lastOf.get).contains(t.testNo) =>
            if (g.passed) s"${formatPoints(g.pointsEarned)} (все тесты группы пройдены)"
            else "0 (не все тесты группы пройдены)"
          case Some(_) => if (passed) "тест пройден" else "тест не пройден"
        }
      TestRowView(t.testNo, SubmitResult.message.getOrElse(t.resultCode, "unknown"), t.timeMs, t.memoryBytes,
        t.group, group.map(_.policy), text)
    }
  }
}
