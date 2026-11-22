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
  * No leak while a round runs or after: no route but the admin ones streams or serves a scoreboard, nothing writes
  * monitor files, and the contestant navbar never includes a scoreboard tab.
  */
class LiveLeakSpec extends AnyFunSuite {
  private def src(f: String) = new String(Files.readAllBytes(Paths.get(f)), "UTF-8")

  private val routes = src("conf/routes").linesIterator.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).toSeq

  test("the contestant and public stream routes are gone; every remaining stream route is an admin one") {
    assert(!routes.exists(_.startsWith("GET /monitor/events")))
    assert(!routes.exists(_.contains("/public/monitor/:contestId/events")))
    assert(!routes.exists(_.contains("publicPreview")))
    assert(!routes.exists(r => r.contains("monitorEvents") && !r.contains("AdminApplication")))
    val streams = routes.filter(_.contains("events"))
    assert(streams.nonEmpty)
    assert(streams.forall(_.split("\\s+")(1).startsWith("/admin/")), streams)
    val admin = src("app/controllers/AdminApplication.scala")
    streams.map(_.split("\\s+")(2)).map(_.stripPrefix("controllers.AdminApplication.").takeWhile(_ != '(')).foreach { m =>
      val at = admin.indexOf(s"def $m")
      assert(at >= 0 && admin.substring(at, at + 200).contains("silhouette.SecuredAction("), m)
    }
  }

  test("no public or contestant scoreboard routes remain in conf/routes") {
    assert(!routes.exists(_.contains("/public/")))
    assert(!routes.exists(_.split("\\s+").lift(1).exists(p => p == "/monitor" || p == "/monitor/")))
  }

  test("the live monitor has only admin joins") {
    val actor = src("app/actors/LiveMonitorActor.scala")
    val joins = "case class (\\w+)\\([^)]*\\) extends Join".r.findAllMatchIn(actor).map(_.group(1)).toSet
    assert(joins == Set("JoinAdmin", "JoinAdminStatus"))
  }

  test("nothing in the application writes monitor files") {
    val files: Seq[Path] = Files.walk(Paths.get("app")).iterator().asScala.filter(p => p.toString.endsWith(".scala")).toList
    val writers = files.filter { p =>
      val t = new String(Files.readAllBytes(p), "UTF-8")
      Seq("writeStringToFile", "Files.write", "Files.move", "FileOutputStream", "static_location").exists(t.contains)
    }
    assert(writers.isEmpty, writers.toString)
  }

  test("visibility: admins always; teams and the public never") {
    val start = new DateTime(2030, 1, 1, 10, 0)
    val c = Contest(5, "R", start, start.plusHours(3), None, 1)
    val during = start.plusHours(1)
    assert(Visibility.canSeeBoard(c, AdminViewer, during))
    for (now <- Seq(during, c.endTime); f <- Seq(false, true)) {
      val cc = c.copy(isFinished = f)
      assert(!Visibility.canSeeBoard(cc, TeamViewer(1, 5), now) && !Visibility.canSeeBoard(cc, Anonymous, now))
    }
  }

  test("the contestant navbar never renders a scoreboard link") {
    implicit val messages: MessagesImpl = MessagesImpl(Lang("en"), new DefaultMessagesApi())
    implicit val request = RequestFactory.plain.createRequest(RemoteConnection("127.0.0.1", secure = false, None), "GET",
      RequestTarget("/", "/", Map.empty), "HTTP/1.1", Headers(), TypedMap.empty, AnyContentAsEmpty)
    val now = DateTime.now
    def page(c: Contest) =
      views.html.main(models.LoggedInTeam("t", c, Team(1, 1, "T", "S", "C", None), Nil))(play.twirl.api.Html("")).body
    for (cc <- Seq(
      Contest(5, "R", now.minusHours(1), now.plusHours(1), None, 1),
      Contest(5, "R", now.minusHours(3), now.minusHours(1), None, 1),
      Contest(5, "R", now.minusHours(1), now.plusHours(1), None, 1, isFinished = true)
    )) {
      assert(!page(cc).contains("""href="/monitor""""))
    }
  }
}
