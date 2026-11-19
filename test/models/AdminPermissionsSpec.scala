package models

import org.scalatest.funsuite.AnyFunSuite

class AdminPermissionsSpec extends AnyFunSuite {
  private val a = Admin("a@example.org", "h")

  test("every admin has every permission in every round") {
    for (c <- Seq(-1, 0, 1, 3, 42)) {
      assert(a.canSpectate(c))
      assert(a.canModify(c))
    }
    assert(a.isSuperAdmin)
  }

  test("the session id is the email plus a fingerprint of the hash") {
    assert(a.toId.username == "a@example.org")
    assert(a.toId.fingerprint == controllers.PasswordHash.fingerprint("h"))
  }
}
