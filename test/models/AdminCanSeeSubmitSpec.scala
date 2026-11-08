package models

import org.scalatest.funsuite.AnyFunSuite

class AdminCanSeeSubmitSpec extends AnyFunSuite {
  private def admin(spectator: Set[Int] = Set.empty, administrator: Set[Int] = Set.empty, unrestricted: Set[Int] = Set.empty) =
    Admin("a", "h", spectator, administrator, Set.empty, unrestricted)

  test("a restricted spectator sees a submit from before the freeze but not from after it") {
    val a = admin(spectator = Set(3))
    assert(a.canSeeSubmit(3, afterFreeze = false))
    assert(!a.canSeeSubmit(3, afterFreeze = true))
  }

  test("unrestricted view, for the contest or for all, and modify rights see everything of their contest") {
    assert(admin(spectator = Set(3), unrestricted = Set(3)).canSeeSubmit(3, afterFreeze = true))
    assert(admin(spectator = Set(3), unrestricted = Set(-1)).canSeeSubmit(3, afterFreeze = true))
    assert(admin(administrator = Set(3)).canSeeSubmit(3, afterFreeze = true))
  }

  test("nothing in a contest the admin cannot spectate, even with unrestricted view of it") {
    val a = admin(spectator = Set(3), unrestricted = Set(4))
    assert(!a.canSeeSubmit(4, afterFreeze = false))
    assert(!a.canSeeSubmit(4, afterFreeze = true))
  }
}
