package actors

import org.scalatest.funsuite.AnyFunSuite

class ContestRowGuardSpec extends AnyFunSuite {
  test("a rescan that started before an edit does not overrule it, for that contest only") {
    val g = new ContestRowGuard
    val started = g.beginRescan()
    g.changed(1)
    assert(!g.believes(1, started))
    assert(g.believes(2, started))
  }

  test("a rescan that started after the edit is believed") {
    val g = new ContestRowGuard
    g.changed(1)
    val started = g.beginRescan()
    assert(g.believes(1, started))
  }

  test("without any edit every rescan is believed, and a later edit makes only older rescans stale") {
    val g = new ContestRowGuard
    val first = g.beginRescan()
    assert(g.believes(1, first))
    g.changed(1)
    val second = g.beginRescan()
    assert(!g.believes(1, first) && g.believes(1, second))
  }
}
