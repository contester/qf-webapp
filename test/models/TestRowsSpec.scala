package models

import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Memory, TimeMs}

class TestRowsSpec extends AnyFunSuite {
  private def r(no: Int, code: Int, group: Option[String] = None, points: Double = 0, sample: Boolean = false) =
    ResultRow(no, code, TimeMs(15), Memory(2048), group, points, sample)

  private def eachGroup(name: String, earned: Double = 0, max: Double = 0, passed: Boolean = true) =
    GroupRow(name, "each-test", earned, max, passed, skipped = false)

  private def completeGroup(name: String, earned: Double, max: Double, passed: Boolean) =
    GroupRow(name, "complete-group", earned, max, passed, skipped = false)

  private def texts(rows: Seq[ResultRow], groups: Seq[GroupRow]) = TestRows.describe(rows, groups).map(_.pointsText)

  test("each-test: a passed test shows its fractional points, a failed one 0") {
    val g = Seq(eachGroup("1", 2.5, 5))
    assert(texts(Seq(r(1, 10, Some("1"), 2.5), r(2, 13, Some("1"), 0)), g) ==
      Seq("2.5 (тест пройден)", "0 (тест не пройден)"))
  }

  test("complete-group: middle tests only say passed or not, the last shows the group's points or 0") {
    val g = Seq(completeGroup("2", 12, 12, passed = true))
    val rows = Seq(r(1, 10, Some("2")), r(2, 13, Some("2")), r(3, 10, Some("2")))
    assert(texts(rows, g) == Seq("тест пройден", "тест не пройден", "12 (все тесты группы пройдены)"))
    val failed = Seq(completeGroup("2", 0, 12, passed = false))
    assert(texts(Seq(r(1, 10, Some("2")), r(2, 13, Some("2"))), failed) ==
      Seq("тест пройден", "0 (не все тесты группы пройдены)"))
  }

  test("a skipped test is not run, whatever the policy") {
    val g = Seq(eachGroup("1"), completeGroup("2", 0, 4, passed = false))
    assert(texts(Seq(r(1, 18, Some("1")), r(2, 10, Some("2")), r(3, 18, Some("2"))), g) ==
      Seq("не запускался", "тест пройден", "не запускался"))
    assert(TestRows.describe(Seq(r(1, 18, Some("1"))), g).head.verdict == "не запускался")
  }

  test("a test without a group shows just its points") {
    assert(texts(Seq(r(1, 10, None, 3), r(2, 13, None, 0)), Nil) == Seq("3", "0"))
    assert(TestRows.describe(Seq(r(1, 10)), Nil).head.policy.isEmpty)
  }

  test("tests are ascending by number and the compile row is left out") {
    val rows = TestRows.describe(Seq(r(3, 10), r(0, 1), r(1, 10), r(2, 13)), Nil)
    assert(rows.map(_.testNo) == Seq(1, 2, 3))
    assert(rows.map(_.verdict) == Seq("Ok", "Wrong answer", "Ok"))
  }

  test("time, memory, group and policy are carried over") {
    val v = TestRows.describe(Seq(r(1, 10, Some("1"), 1)), Seq(eachGroup("1"))).head
    assert(v.timeMs == TimeMs(15) && v.memoryBytes == Memory(2048))
    assert(v.group.contains("1") && v.policy.contains("each-test"))
  }

  test("problem D: each-test group 1, complete-groups 2 and 3, a failure in group 3") {
    val groups = Seq(
      eachGroup("1", 5, 5),
      completeGroup("2", 8, 8, passed = true),
      completeGroup("3", 0, 12, passed = false))
    val pts = Seq(0.0, 2.0, 3.0)
    val rows = (1 to 3).map(i => r(i, 10, Some("1"), pts(i - 1))) ++
      (4 to 11).map(i => r(i, 10, Some("2"))) ++
      (12 to 21).map(i => r(i, if (i == 15) 13 else 10, Some("3")))
    val t = texts(rows.reverse, groups)
    assert(t.take(3) == Seq("0 (тест пройден)", "2 (тест пройден)", "3 (тест пройден)"))
    assert(t.slice(3, 10) == Seq.fill(7)("тест пройден"))
    assert(t(10) == "8 (все тесты группы пройдены)")
    assert(t.slice(11, 14) == Seq.fill(3)("тест пройден"))
    assert(t(14) == "тест не пройден")
    assert(t.slice(15, 20) == Seq.fill(5)("тест пройден"))
    assert(t(20) == "0 (не все тесты группы пройдены)")
    assert(t.size == 21)
  }

  test("number format: up to two decimals, trailing zeros trimmed") {
    assert(TestRows.formatPoints(3.0) == "3")
    assert(TestRows.formatPoints(10) == "10")
    assert(TestRows.formatPoints(100) == "100")
    assert(TestRows.formatPoints(0) == "0")
    assert(TestRows.formatPoints(2.5) == "2.5")
    assert(TestRows.formatPoints(0.005) == "0.01")
    assert(TestRows.formatPoints(2.499) == "2.5")
    assert(TestRows.formatPoints(545) == "545")
  }

  test("number format: NaN and infinities never throw, they read 0") {
    assert(TestRows.formatPoints(Double.NaN) == "0")
    assert(TestRows.formatPoints(Double.PositiveInfinity) == "0")
    assert(TestRows.formatPoints(Double.NegativeInfinity) == "0")
  }

  test("a finished, compiled submit whose points are NULL is in progress, not 0 of max") {
    val s = Submit(SubmitId(1, Arrived(new org.joda.time.DateTime(2030, 1, 1, 0, 0), 0), 1, 1,
      RatedProblem("A", 30), "cpp"), finished = true, compiled = true, 0, 0, None, None, 20)
    assert(SubmitResult.fromSubmit(s) == BeingRetestedResult)
    assert(SubmitResult.fromSubmit(s.copy(points = Some(0))) == SubmitPointsResult(0, 20))
    assert(SubmitResult.fromSubmit(s.copy(compiled = false)) == SubmitCompileError)
    assert(SubmitResult.fromSubmit(s.copy(finished = false)) == SubmitWaiting)
  }

  test("SubmitPointsResult reads points of max") {
    assert(SubmitPointsResult(2.5, 10).message == "Баллы: 2.5 из 10")
    assert(SubmitPointsResult(10, 10).success)
    assert(!SubmitPointsResult(0, 0).success)
    assert(!SubmitPointsResult(3, 10).success)
  }

  test("sample tests clear group and policy, show sample points text, and do not steal lastOf in complete-group") {
    val g = Seq(eachGroup("0", 0, 0), completeGroup("2", 12, 12, passed = true))
    val rows = Seq(
      r(1, 10, Some("0"), 0, sample = true),
      r(2, 13, Some("0"), 0, sample = true),
      r(3, 18, Some("0"), 0, sample = true),
      r(4, 10, Some("2")),
      r(5, 10, Some("2")),
      r(6, 10, Some("2"), 0, sample = true))
    val described = TestRows.describe(rows, g)
    assert(described.map(_.pointsText) == Seq(
      "0 (тест из условия, пройден)",
      "0 (тест из условия, не пройден)",
      "не запускался",
      "тест пройден",
      "12 (все тесты группы пройдены)",
      "0 (тест из условия, пройден)"))
    for (idx <- Seq(0, 1, 2, 5)) {
      assert(described(idx).group.isEmpty && described(idx).policy.isEmpty)
    }
    assert(described(3).group.contains("2") && described(3).policy.contains("complete-group"))
  }

  test("formatPointsWithUnit pluralizes Russian, English, and fallback point units") {
    import play.api.i18n.{DefaultLangs, DefaultMessagesApi, Lang, MessagesImpl}
    val emptyEn = MessagesImpl(Lang("en"), new DefaultMessagesApi())
    assert(TestRows.formatPointsWithUnit(10)(emptyEn) == "10 баллов")
    assert(TestRows.formatPointsWithUnit(2.5)(emptyEn) == "2.5 балла")

    val ruApi = new DefaultMessagesApi(Map("ru" -> Map(
      "points.unit.one" -> "балл", "points.unit.few" -> "балла", "points.unit.many" -> "баллов"
    )), new DefaultLangs(Seq(Lang("ru"))))
    val ru = MessagesImpl(Lang("ru"), ruApi)
    val ruCases = Seq(
      0.0 -> "0 баллов", 1.0 -> "1 балл", 2.0 -> "2 балла", 4.0 -> "4 балла",
      5.0 -> "5 баллов", 11.0 -> "11 баллов", 12.0 -> "12 баллов", 14.0 -> "14 баллов",
      21.0 -> "21 балл", 22.0 -> "22 балла", 30.0 -> "30 баллов", 2.5 -> "2.5 балла")
    for ((n, expected) <- ruCases) assert(TestRows.formatPointsWithUnit(n)(ru) == expected, n)

    val enApi = new DefaultMessagesApi(Map("en" -> Map(
      "points.unit.one" -> "point", "points.unit.few" -> "points", "points.unit.many" -> "points"
    )), new DefaultLangs(Seq(Lang("en"))))
    val en = MessagesImpl(Lang("en"), enApi)
    val enCases = Seq(
      0.0 -> "0 points", 1.0 -> "1 point", 2.0 -> "2 points", 21.0 -> "21 points", 2.5 -> "2.5 points")
    for ((n, expected) <- enCases) assert(TestRows.formatPointsWithUnit(n)(en) == expected, n)
  }
}
