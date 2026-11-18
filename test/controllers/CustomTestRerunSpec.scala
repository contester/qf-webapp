package controllers

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}

class CustomTestRerunSpec extends AnyFunSuite {
  private def read(p: String) = new String(Files.readAllBytes(Paths.get(p)), "UTF-8")

  test("a rerun is a POST, needs modify rights on the contest, and inserts a copy (RejudgeResetDbSpec covers the copy)") {
    assert(read("conf/routes").contains("POST /admin/customtest/:contestId/:id/rerun controllers.AdminApplication.rerunCustomTest(contestId: Int, id: Long)"))
    val admin = read("app/controllers/AdminApplication.scala")
    val at = admin.indexOf("def rerunCustomTest")
    assert(at >= 0)
    val body = admin.substring(at, math.min(admin.length, at + 600))
    assert(body.contains("AdminPermissions.withModify(contestId)"))
    // a copy of a test of this contest; the page then shows the copy
    assert(body.contains("AdminCustomTests.rerun(db, contestId, id)"))
    assert(body.contains("routes.AdminApplication.showCustomTest(contestId, copy)"))
  }

  test("the button is shown only to those who can modify") {
    val admin = read("app/controllers/AdminApplication.scala")
    assert(admin.contains("request.identity.canModify(contestId))"))
    assert(read("app/views/admin/showcustomtest.scala.html").contains("@if(canRerun)"))
  }
}
