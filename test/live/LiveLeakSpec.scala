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
  * No leak while a round runs: no route but the admin ones streams a board, nothing writes monitor files, and the
  * contestant navbar has no scoreboard tab until the round is over. (BoardAccessSpec and PublicBoardCacheSpec drive
  * the 403 of /public/monitor/:id; /monitor uses the same BoardAccess.decide with a TeamViewer.)
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

  test("the public routes are the page only, and it is the 403/404-gated PublicMonitor.page") {
    assert(routes.filter(_.contains("/public/")) == Seq("GET /public/monitor/:contestId controllers.PublicMonitor.page(contestId: Int)"))
    assert(src("app/controllers/PublicMonitor.scala").contains("BoardAccess.decide(round, Anonymous, now())"))
    assert(src("app/controllers/Application.scala").contains("BoardAccess.decide(round, TeamViewer(team.team.id, team.contest.id)"))
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

  test("visibility: admins always, teams and the public only once the round is over") {
    val start = new DateTime(2030, 1, 1, 10, 0)
    val c = Contest(5, "R", start, start.plusHours(3), None, 1)
    val during = start.plusHours(1)
    assert(Visibility.canSeeBoard(c, AdminViewer, during))
    assert(!Visibility.canSeeBoard(c, TeamViewer(1, 5), during) && !Visibility.canSeeBoard(c, Anonymous, during))
    assert(Visibility.canSeeBoard(c, Anonymous, c.endTime) && Visibility.canSeeBoard(c.copy(isFinished = true), Anonymous, during))
  }

  test("the contestant navbar shows the scoreboard tab only once the round is over") {
    implicit val messages: MessagesImpl = MessagesImpl(Lang("en"), new DefaultMessagesApi())
    implicit val request = RequestFactory.plain.createRequest(RemoteConnection("127.0.0.1", secure = false, None), "GET",
      RequestTarget("/", "/", Map.empty), "HTTP/1.1", Headers(), TypedMap.empty, AnyContentAsEmpty)
    val now = DateTime.now
    def page(c: Contest) =
      views.html.main(models.LoggedInTeam("t", c, Team(1, 1, "T", "S", "C", None), Nil))(play.twirl.api.Html("")).body
    val tab = controllers.routes.Application.monitorDefault.url
    assert(!page(Contest(5, "R", now.minusHours(1), now.plusHours(1), None, 1)).contains(s"""href="$tab""""))
    assert(page(Contest(5, "R", now.minusHours(3), now.minusHours(1), None, 1)).contains(s"""href="$tab""""))
    assert(page(Contest(5, "R", now.minusHours(1), now.plusHours(1), None, 1, isFinished = true)).contains(s"""href="$tab""""))
  }
}
