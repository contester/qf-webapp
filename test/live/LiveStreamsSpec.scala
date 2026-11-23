package live

import actors.LiveMonitorActor.{JoinAdmin, JoinAdminStatus, JoinTeam}
import controllers.LiveStreams
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

class LiveStreamsSpec extends AnyFunSuite {
  private def src(f: String) = new String(Files.readAllBytes(Paths.get(f)), "UTF-8")

  test("every live join is counted under the kind of feed and the contest it joins") {
    assert(LiveStreams.feedOf(JoinTeam(4)) == (FeedKind.TeamMonitor, 4))
    assert(LiveStreams.feedOf(JoinAdmin(4)) == (FeedKind.AdminMonitor, 4))
    assert(LiveStreams.feedOf(JoinAdminStatus(4)) == (FeedKind.StatusRows, 4))
    assert(FeedKind.all.map(_.id) == Seq("teamMonitor", "teamFeed", "adminMonitor", "adminFeed", "statusRows"))
  }

  test("every route that opens a stream counts it") {
    assert(src("app/controllers/Application.scala").contains("FeedKind.TeamFeed, contestId)"))
    assert(src("app/controllers/AdminApplication.scala").contains("FeedKind.AdminFeed, contestId)"))
    // The team monitor, admin monitor and status routes go through LiveStreams.stream, which counts.
    assert(src("app/controllers/LiveStreams.scala").contains("stats.track(kind, contest)"))
  }

  test("the team board join is built only by the actor and LiveStreams, and used only by authenticated Application monitor routes") {
    assert(LiveStreams.teamJoin(7) == JoinTeam(7))
    val root = Paths.get("app")
    assert(Files.isDirectory(root), "tests must run from the project root")
    val files = Files.walk(root).iterator().asScala.filter(p => p.toString.endsWith(".scala") || p.toString.endsWith(".html")).toList
    val allowed = Set("app/actors/LiveMonitorActor.scala", "app/controllers/LiveStreams.scala")
    val offenders = files.filter { p =>
      val text = new String(Files.readAllBytes(p), "UTF-8")
      text.contains("LiveMonitorActor.JoinTeam(") || text.contains("JoinTeam(") && text.contains("LiveMonitorActor")
    }.map(_.toString).filterNot(allowed)
    assert(offenders.isEmpty, offenders.toString)
    val users = files.filter(p => new String(Files.readAllBytes(p), "UTF-8").contains("LiveStreams.teamJoin(")).map(_.toString).toSet
    assert(users == Set("app/controllers/Application.scala"), users.toString)
    val app = src("app/controllers/Application.scala")
    Seq("def monitorDefault", "def monitorEvents").foreach { d =>
      val at = app.indexOf(d)
      assert(at >= 0 && app.substring(at, at + d.length + 80).contains("silhouette.SecuredAction"), d)
    }
  }

  test("the contester page and its stream are for global admins only, and nothing else shows the link") {
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

  test("the board join is built only by the actor and LiveStreams, and used only by the admin monitor routes") {
    assert(LiveStreams.adminJoin(7) == JoinAdmin(7))
    val root = Paths.get("app")
    assert(Files.isDirectory(root), "tests must run from the project root")
    val files = Files.walk(root).iterator().asScala.filter(p => p.toString.endsWith(".scala") || p.toString.endsWith(".html")).toList
    assert(files.size > 50)
    val allowed = Set("app/actors/LiveMonitorActor.scala", "app/controllers/LiveStreams.scala")
    val offenders = files.filter { p =>
      val text = new String(Files.readAllBytes(p), "UTF-8")
      text.contains("LiveMonitorActor.JoinAdmin(") || text.contains("JoinAdmin(") && text.contains("LiveMonitorActor")
    }.map(_.toString).filterNot(allowed)
    assert(offenders.isEmpty, offenders.toString)
    val users = files.filter(p => new String(Files.readAllBytes(p), "UTF-8").contains("LiveStreams.adminJoin(")).map(_.toString).toSet
    assert(users == Set("app/controllers/AdminApplication.scala"), users.toString)
    val admin = src("app/controllers/AdminApplication.scala")
    Seq("def monitor(id: Int)", "def monitorEvents(contestId: Int)", "def statusEvents(contestId: Int)").foreach { d =>
      val at = admin.indexOf(d)
      assert(at >= 0 && admin.substring(at, at + d.length + 80).contains("silhouette.SecuredAction(AdminPermissions.withSpectate("), d)
    }
  }

  test("the status page stream carries every submit of the round: there is no freeze") {
    assert(LiveStreams.adminStatusJoin(7) == JoinAdminStatus(7))
    assert(src("app/controllers/AdminApplication.scala").contains("LiveStreams.adminStatusJoin(contestId)"))
  }
}
