package live

import actors.LiveMonitorActor.{JoinAdmin, JoinAdminStatus, JoinPublic, JoinPublicPreview, JoinTeam}
import controllers.LiveStreams
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path, Paths}
import scala.collection.JavaConverters._

class LiveStreamsSpec extends AnyFunSuite {
  test("every live join is counted under the kind of feed and the contest it joins") {
    assert(LiveStreams.feedOf(JoinPublic(4)) == (FeedKind.PublicMonitor, 4))
    assert(LiveStreams.feedOf(JoinPublicPreview(4)) == (FeedKind.PublicPreview, 4))
    assert(LiveStreams.feedOf(JoinTeam(4, 9)) == (FeedKind.TeamMonitor, 4))
    assert(LiveStreams.feedOf(JoinAdmin(4, real = true)) == (FeedKind.AdminMonitorUnrestricted, 4))
    assert(LiveStreams.feedOf(JoinAdmin(4, real = false)) == (FeedKind.AdminMonitorRestricted, 4))
    assert(LiveStreams.feedOf(JoinAdminStatus(4, real = true)) == (FeedKind.StatusRows, 4))
    assert(LiveStreams.feedOf(JoinAdminStatus(4, real = false)) == (FeedKind.StatusRows, 4))
  }

  test("every route that opens a stream counts it") {
    def src(f: String) = new String(Files.readAllBytes(Paths.get(f)), "UTF-8")
    assert(src("app/controllers/PublicMonitor.scala").contains("stats.track(FeedKind.PublicMonitor, contestId)"))
    assert(src("app/controllers/Application.scala").contains("FeedKind.TeamFeed, contestId)"))
    assert(src("app/controllers/AdminApplication.scala").contains("FeedKind.AdminFeed, contestId)"))
    // The monitor routes of the team and the admin go through LiveStreams.stream, which counts.
    assert(src("app/controllers/LiveStreams.scala").contains("stats.track(kind, contest)"))
  }

  test("the contester page and its stream are for global admins only, and nothing else shows the link") {
    def src(f: String) = new String(Files.readAllBytes(Paths.get(f)), "UTF-8")
    val admin = src("app/controllers/AdminApplication.scala")
    Seq("def contester(contestId: Int)", "def contesterEvents").foreach { d =>
      val at = admin.indexOf(d)
      assert(at >= 0, d)
      assert(admin.substring(at, at + d.length + 80).contains("AdminPermissions.withSuperAdmin"), d)
    }
    val routes = src("conf/routes")
    assert(routes.contains("controllers.AdminApplication.contester(contestId: Int)"))
    assert(routes.contains("controllers.AdminApplication.contesterEvents"))
    assert(src("app/views/admin/adminmain.scala.html").contains("@if(contest.isSuperAdmin) {@navlink(\"contester\""))
    assert(src("app/models/Admins.scala").contains("case \"contester\" => routes.AdminApplication.contester(_)"))
  }

  test("only an account that can see everything gets the real admin stream") {
    assert(LiveStreams.adminJoin(7, canSeeAll = true) == JoinAdmin(7, real = true))
    assert(LiveStreams.adminJoin(7, canSeeAll = false) == JoinAdmin(7, real = false))
  }

  test("the status page stream is real only for an account that can see everything") {
    assert(LiveStreams.adminStatusJoin(7, canSeeAll = true) == JoinAdminStatus(7, real = true))
    assert(LiveStreams.adminStatusJoin(7, canSeeAll = false) == JoinAdminStatus(7, real = false))
    val admin = new String(Files.readAllBytes(Paths.get("app/controllers/AdminApplication.scala")), "UTF-8")
    assert(admin.contains("LiveStreams.adminStatusJoin(contestId, request.identity.canSeeAll(contestId))"))
  }

  test("no code outside the actor and LiveStreams builds a real-board join") {
    val root = Paths.get("app")
    assert(Files.isDirectory(root), "tests must run from the project root")
    val files = Files.walk(root).iterator().asScala.filter(p => p.toString.endsWith(".scala") || p.toString.endsWith(".html")).toList
    assert(files.size > 50)
    val allowed = Set("app/actors/LiveMonitorActor.scala", "app/controllers/LiveStreams.scala")
    val offenders = files.filter { p =>
      val text = new String(Files.readAllBytes(p), "UTF-8")
      text.contains("LiveMonitorActor.JoinAdmin") || text.contains("JoinAdmin(") && text.contains("LiveMonitorActor")
    }.map(_.toString).filterNot(allowed)
    assert(offenders.isEmpty, offenders.toString)
    // The one place that builds it is called with the account's own permission.
    val admin = new String(Files.readAllBytes(Paths.get("app/controllers/AdminApplication.scala")), "UTF-8")
    assert(admin.contains("LiveStreams.adminJoin(contestId, request.identity.canSeeAll(contestId))"))
  }

  test("the public-preview join is built only in LiveStreams and used only by the admin routes") {
    val files = Files.walk(Paths.get("app")).iterator().asScala.filter(p => p.toString.endsWith(".scala") || p.toString.endsWith(".html")).toList
    val users = files.filter(p => new String(Files.readAllBytes(p), "UTF-8").toLowerCase.contains("publicpreview")).map(_.toString).toSet
    assert(users == Set("app/actors/LiveMonitorActor.scala", "app/controllers/LiveStreams.scala", "app/controllers/AdminApplication.scala", "app/live/ConnectionStats.scala"), users.toString)
    val admin = new String(Files.readAllBytes(Paths.get("app/controllers/AdminApplication.scala")), "UTF-8")
    Seq("def publicPreview(id: Int)", "def publicPreviewEvents(id: Int)").foreach { d =>
      val at = admin.indexOf(d)
      assert(at >= 0 && admin.substring(at, at + d.length + 80).contains("AdminPermissions.withSpectate(id)"), d)
    }
  }
}
