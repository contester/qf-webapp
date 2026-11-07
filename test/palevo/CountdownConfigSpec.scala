package palevo

import org.scalatest.funsuite.AnyFunSuite
import play.api.Configuration

class CountdownConfigSpec extends AnyFunSuite {
  private val d = Seq(60, 30, 20, 10, 5, 2, 1)

  test("defaults") {
    assert(CountdownConfig.Default == CountdownConfig(Seq(60, 30, 20, 10, 5, 2, 1), Seq(20, 10, 5, 1)))
  }

  test("sanitize sorts descending and drops duplicates") {
    assert(CountdownConfig.sanitize(Seq(5, 20, 5), d) == Seq(20, 5))
  }

  test("sanitize falls back to the default when any entry is not positive") {
    assert(CountdownConfig.sanitize(Seq(10, 0), d) == d)
    assert(CountdownConfig.sanitize(Seq(-3), d) == d)
  }

  test("an empty list stays empty: it disables the countdown") {
    assert(CountdownConfig.sanitize(Nil, d) == Nil)
  }

  test("a value of the wrong type falls back to the default instead of failing") {
    val asString = CountdownConfig(Configuration.from(Map("palevo.countdown.start" -> "60,30")))
    assert(asString == CountdownConfig.Default)
    val words = CountdownConfig(Configuration.from(Map("palevo.countdown.end" -> Seq("soon"))))
    assert(words.end == CountdownConfig.Default.end)
  }

  test("apply reads palevo.countdown.start and .end; a missing key gives the default") {
    val cfg = CountdownConfig(Configuration.from(Map("palevo.countdown.start" -> Seq(15, 5))))
    assert(cfg == CountdownConfig(Seq(15, 5), CountdownConfig.Default.end))
    assert(CountdownConfig(Configuration.empty) == CountdownConfig.Default)
    assert(CountdownConfig(Configuration.from(Map("palevo.countdown.end" -> Seq.empty[Int]))).end == Nil)
  }
}
