package models

import org.scalatest.funsuite.AnyFunSuite

class RoomLoginsSpec extends AnyFunSuite {
  test("logins are d<room> / <letter><room>, sorted and deduplicated") {
    val l = InitialImportTools.roomLogins(Seq("305", "101", "305"), "l")
    assert(l == Seq(InitialImportTools.RoomLogin("d101", "l101", "101"), InitialImportTools.RoomLogin("d305", "l305", "305")))
  }
}
