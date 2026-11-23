package live

import models.{AdminViewer, Anonymous, TeamViewer, Visibility}
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, Team}
import play.api.i18n.{DefaultMessagesApi, Lang, MessagesImpl}
import play.api.libs.typedmap.TypedMap
import play.api.mvc.{AnyContentAsEmpty, Headers}
import play.api.mvc.request.{RemoteConnection, RequestFactory, RequestTarget}

import java.nio.file.{Files, Path, Paths}
import scala.collection.JavaConverters._

/**
  * No leak of public or cross-round boards: no unauthenticated route streams or serves a scoreboard, every monitor
  * route is protected by Silhouette, nothing writes monitor files, and the contestant navbar always links to /monitor.
  */
class LiveLeakSpec extends AnyFunSuite {
  private def src(f: String) = new String(Files.readAllBytes(Paths.get(f)), "UTF-8")

  private val routes = src("conf/routes").linesIterator.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).toSeq

  test("no public stream or preview routes exist; every stream route is authenticated") {
    assert(!routes.exists(_.contains("/public/monitor")))
    assert(!routes.exists(_.contains("publicPreview")))
    val streams = routes.filter(_.contains("events"))
    assert(streams.nonEmpty)
    val admin = src("app/controllers/AdminApplication.scala")
    val app = src("app/controllers/Application.scala")
    streams.foreach { r =>
      val cols = r.split("\\s+")
      val path = cols(1)
      val target = cols(2).takeWhile(_ != '(')
      if (path.startsWith("/admin/")) {
        val m = target.stripPrefix("controllers.AdminApplication.")
        val at = admin.indexOf(s"def $m")
        assert(at >= 0 && admin.substring(at, at + 200).contains("silhouette.SecuredAction("), r)
      } else {
        assert(path == "/monitor/events" && target == "controllers.Application.monitorEvents", r)
        val at = app.indexOf("def monitorEvents")
        assert(at >= 0 && app.substring(at, at + 150).contains("silhouette.SecuredAction"), r)
      }
    }
  }

  test("no public scoreboard routes remain in conf/routes and /monitor requires authentication") {
    assert(!routes.exists(_.contains("/public/")))
    assert(!routes.exists(_.contains("PublicMonitor")))
    val monitorRoutes = routes.filter(r => r.split("\\s+").lift(1).exists(p => p == "/monitor" || p == "/monitor/"))
    assert(monitorRoutes.size == 2)
    assert(monitorRoutes.forall(_.endsWith("controllers.Application.monitorDefault")))
    val app = src("app/controllers/Application.scala")
    val at = app.indexOf("def monitorDefault")
    assert(at >= 0 && app.substring(at, at + 150).contains("silhouette.SecuredAction"))
  }

  test("the live monitor has only team and admin joins") {
    val actor = src("app/actors/LiveMonitorActor.scala")
    val joins = "case class (\\w+)\\([^)]*\\) extends Join".r.findAllMatchIn(actor).map(_.group(1)).toSet
    assert(joins == Set("JoinTeam", "JoinAdmin", "JoinAdminStatus"))
  }

  test("nothing in the application writes monitor files") {
    val files: Seq[Path] = Files.walk(Paths.get("app")).iterator().asScala.filter(p => p.toString.endsWith(".scala")).toList
    val writers = files.filter { p =>
      val t = new String(Files.readAllBytes(p), "UTF-8")
      Seq("writeStringToFile", "Files.write", "Files.move", "FileOutputStream", "static_location").exists(t.contains)
    }
    assert(writers.isEmpty, writers.toString)
  }

  test("visibility: admins always, participants only their own round, anonymous never") {
    val start = new DateTime(2030, 1, 1, 10, 0)
    val c = Contest(5, "R", start, start.plusHours(3), None, 1)
    val during = start.plusHours(1)
    assert(Visibility.canSeeBoard(c, AdminViewer, during))
    for (now <- Seq(start.minusHours(1), during, c.endTime); f <- Seq(false, true)) {
      val cc = c.copy(isFinished = f)
      assert(Visibility.canSeeBoard(cc, TeamViewer(1, 5), now))
      assert(!Visibility.canSeeBoard(cc, TeamViewer(1, 6), now))
      assert(!Visibility.canSeeBoard(cc, Anonymous, now))
    }
  }

  test("the contestant navbar renders the scoreboard link before, during, and after the round") {
    implicit val messages: MessagesImpl = MessagesImpl(Lang("en"), new DefaultMessagesApi())
    implicit val request = RequestFactory.plain.createRequest(RemoteConnection("127.0.0.1", secure = false, None), "GET",
      RequestTarget("/", "/", Map.empty), "HTTP/1.1", Headers(), TypedMap.empty, AnyContentAsEmpty)
    val now = DateTime.now
    def page(c: Contest) =
      views.html.main(models.LoggedInTeam("t", c, Team(1, 1, "T", "S", "C", None), Nil))(play.twirl.api.Html("")).body
    for (cc <- Seq(
      Contest(5, "R", now.plusHours(1), now.plusHours(3), None, 1),
      Contest(5, "R", now.minusHours(1), now.plusHours(1), None, 1),
      Contest(5, "R", now.minusHours(3), now.minusHours(1), None, 1),
      Contest(5, "R", now.minusHours(1), now.plusHours(1), None, 1, isFinished = true)
    )) {
      assert(page(cc).contains("""href="/monitor""""))
    }
  }
}
