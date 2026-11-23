package models

import org.stingray.contester.dbmodel.{Memory, TimeMs}

import scala.math.BigDecimal.RoundingMode

/** One per-test result of a testing (test_no >= 1, or 0 for the compile result). */
case class ResultRow(testNo: Int, resultCode: Int, timeMs: TimeMs, memoryBytes: Memory, group: Option[String], points: Double,
                     isSample: Boolean = false)

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

  /** Formats max points with localized pluralized points unit (Russian fallback when keys absent). */
  def formatPointsWithUnit(x: Double)(implicit messages: play.api.i18n.Messages): String = {
    val s = formatPoints(x)
    val hasFraction = s.contains('.')
    val n = BigDecimal(s).abs.toBigInt
    def pickRu(one: String, few: String, many: String): String =
      if (hasFraction) few
      else {
        val m100 = (n % 100).toInt
        val m10 = (n % 10).toInt
        if (m100 >= 11 && m100 <= 14) many
        else if (m10 == 1) one
        else if (m10 >= 2 && m10 <= 4) few
        else many
      }
    val unit =
      if (!messages.isDefinedAt("points.unit.one")) pickRu("балл", "балла", "баллов")
      else if (messages.lang.language == "en") {
        if (!hasFraction && n == BigInt(1)) messages("points.unit.one")
        else messages("points.unit.many")
      } else pickRu(messages("points.unit.one"), messages("points.unit.few"), messages("points.unit.many"))
    s"$s $unit"
  }

  def fromEntry(e: ResultEntry): ResultRow = ResultRow(e.test, e.result, e.time, e.memory, e.group, e.points, e.isSample)

  /** The rows of the per-test table: ascending by test number, the compile row (test 0) left out. */
  def describe(results: Seq[ResultRow], groups: Seq[GroupRow]): Seq[TestRowView] =
    describeImpl(results, groups, None)

  /** Localized per-test table rows using the request's Messages. */
  def describe(results: Seq[ResultRow], groups: Seq[GroupRow], messages: play.api.i18n.Messages): Seq[TestRowView] =
    describeImpl(results, groups, Some(messages))

  private def describeImpl(results: Seq[ResultRow], groups: Seq[GroupRow], messages: Option[play.api.i18n.Messages]): Seq[TestRowView] = {
    val tests = results.filter(_.testNo != 0).sortBy(_.testNo)
    val byName = groups.map(g => g.name -> g).toMap
    val lastOf = tests.filterNot(_.isSample).flatMap(t => t.group.map(_ -> t.testNo)).groupBy(_._1).mapValues(_.map(_._2).max)

    def tr(key: String, fallback: => String, args: Any*): String =
      messages.filter(_.isDefinedAt(key)).fold(fallback)(m => m(key, args: _*))

    tests.map { t =>
      val passed = t.resultCode == Accepted
      val (outGroup, outPolicy, text) =
        if (t.isSample) {
          val sText =
            if (t.resultCode == Skipped) tr("test.skipped", "не запускался")
            else if (passed) tr("test.sample.passed", "0 (тест из условия, пройден)")
            else tr("test.sample.failed", "0 (тест из условия, не пройден)")
          (None, None, sText)
        } else {
          val group = t.group.flatMap(byName.get)
          val gText =
            if (t.resultCode == Skipped) tr("test.skipped", "не запускался")
            else group match {
              case None => formatPoints(t.points)
              case Some(g) if g.policy == "each-test" =>
                if (passed) tr("test.each.passed", s"${formatPoints(t.points)} (тест пройден)", formatPoints(t.points))
                else tr("test.each.failed", "0 (тест не пройден)")
              case Some(g) if t.group.flatMap(lastOf.get).contains(t.testNo) =>
                if (g.passed) tr("test.group.passed", s"${formatPoints(g.pointsEarned)} (все тесты группы пройдены)", formatPoints(g.pointsEarned))
                else tr("test.group.failed", "0 (не все тесты группы пройдены)")
              case Some(_) =>
                if (passed) tr("test.passed", "тест пройден")
                else tr("test.failed", "тест не пройден")
            }
          (t.group, group.map(_.policy), gText)
        }
      val verdict = messages.fold(SubmitResult.message.getOrElse(t.resultCode, "unknown"))(implicit m => SubmitResult.statusMessage(t.resultCode))
      TestRowView(t.testNo, verdict, t.timeMs, t.memoryBytes, outGroup, outPolicy, text)
    }
  }
}
