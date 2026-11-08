package controllers

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}

class CustomTestRerunSpec extends AnyFunSuite {
  private def read(p: String) = new String(Files.readAllBytes(Paths.get(p)), "UTF-8")

  test("a rerun is a POST, needs modify rights on the contest, and rings the director's eval queue") {
    assert(read("conf/routes").contains("POST /admin/customtest/:contestId/:id/rerun controllers.AdminApplication.rerunCustomTest(contestId: Int, id: Long)"))
    val admin = read("app/controllers/AdminApplication.scala")
    val at = admin.indexOf("def rerunCustomTest")
    assert(at >= 0)
    val body = admin.substring(at, math.min(admin.length, at + 600))
    assert(body.contains("AdminPermissions.withModify(contestId)"))
    assert(body.contains("""queue = "contester.evalrequests""""))
    // only after the row is reset, and only for a test of this contest
    assert(body.indexOf("AdminCustomTests.reset(db, contestId, id)") < body.indexOf("rabbitMq !"))
  }

  test("the button is shown only to those who can modify") {
    val admin = read("app/controllers/AdminApplication.scala")
    assert(admin.contains("request.identity.canModify(contestId))"))
    assert(read("app/views/admin/showcustomtest.scala.html").contains("@if(canRerun)"))
  }
}
