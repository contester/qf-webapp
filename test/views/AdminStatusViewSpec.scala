package views

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import models._
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, Memory, Team, TimeMs}
import play.api.http.HttpConfiguration
import play.api.i18n.{DefaultMessagesApi, Lang, MessagesImpl}
import play.api.mvc.{Headers, RequestHeader}
import play.api.mvc.request.{DefaultRequestFactory, RemoteConnection, RequestTarget}
import play.api.libs.typedmap.TypedMap

import scala.io.Source

/**
  * The markup contract of the admin status page: what the server pushes as a row (submitrow.scala.html) and what the
  * page script (listenOnStatus in main.js) works on. test/js/status-check.js runs the script on these same pinned rows.
  * The pinned files hold a row with placeholders: submit 4242, team 7777, verdict @@VERDICT@@. When the template
  * changes on purpose, regenerate them: PIN_FIXTURES=1 sbt "testOnly views.AdminStatusViewSpec"
  */
class AdminStatusViewSpec extends AnyFunSuite {
  private def loadMessages(file: String): Map[String, String] = {
    val s = Source.fromFile(file, "UTF-8")
    try s.getLines().map(_.trim).filter(l => l.contains("=") && !l.startsWith("#"))
      .map { l => val i = l.indexOf('='); l.substring(0, i).trim -> l.substring(i + 1).trim }.toMap
    finally s.close()
  }

  private implicit val messages: MessagesImpl =
    MessagesImpl(Lang("en"), new DefaultMessagesApi(Map("en" -> loadMessages("conf/messages")),
      new play.api.i18n.DefaultLangs(Seq(Lang("en")))))

  private implicit val request: RequestHeader = new DefaultRequestFactory(HttpConfiguration()).createRequestHeader(
    RemoteConnection(java.net.InetAddress.getLoopbackAddress, secure = false, None), "GET",
    RequestTarget("/admin/submits/3", "/admin/submits/3", Map.empty), "HTTP/1.1", new Headers(Seq.empty), TypedMap.empty)

  private object Placeholder extends SubmitResult {
    val success = false
    val message = "@@VERDICT@@"
  }

  private val row: AdminSubmitRow = {
    val s = Submit(SubmitId(4242, Arrived(new DateTime(2026, 11, 1, 10, 0), 600), 7777, 3,
      RatedProblem("A", 30), "cpp"), finished = true, compiled = true, passed = 1, taken = 3, None)
    AdminSubmitRow(s, Placeholder, SubmitStats(TimeMs(31), Memory(2048)), runs = 2)
  }

  private def rendered(rejudge: Boolean) = html.admin.submitrow(row, Map.empty, rejudge, linkTeam = true).body.trim

  private val dir = Paths.get("test/resources/admin")

  private def pinned(name: String) = {
    val s = Source.fromFile(dir.resolve(name).toFile, "UTF-8")
    try s.mkString.trim finally s.close()
  }

  if (sys.env.contains("PIN_FIXTURES")) {
    Files.write(dir.resolve("statusrow-rw.html"), (rendered(true) + "\n").getBytes(UTF_8))
    Files.write(dir.resolve("statusrow-ro.html"), (rendered(false) + "\n").getBytes(UTF_8))
  }

  test("the row pushed to those who can rejudge is the pinned markup") {
    assert(rendered(true) == pinned("statusrow-rw.html"))
  }

  test("the row pushed to those who cannot differs from it only by the rejudge cell") {
    assert(rendered(false) == pinned("statusrow-ro.html"))
    assert(rendered(true).contains("rejudge-4242") && !rendered(false).contains("rejudge-4242"))
    def squeezed(h: String) = h.replaceAll("\\s+", " ").replace("> <", "><")
    val withoutCell = rendered(true).replaceAll("(?s)<td>\\s*<div class=\"btn-group.*?</div>\\s*</td>", "")
    assert(squeezed(withoutCell) == squeezed(rendered(false)))
  }

  test("what the script looks for is in the row: its id, the verdict cell, the team link") {
    val r = rendered(true)
    assert(r.startsWith("<tr id=\"result-row-4242\">"))
    assert(r.contains("id=\"result-4242\""))
    assert(r.contains("<a class=\"team-link\" href=\"/admin/submits/3?team=7777\">Team 7777</a>"))
    assert(r.endsWith("</tr>"))
  }

  private val contest =
    Contest(3, "C", DateTime.now.minusHours(1), DateTime.now.plusHours(4), None, 1)

  private def admin() = Admin("a@example.org", "h")

  private def page(live: Boolean, newest: Boolean = true, team: Option[Int] = None, modify: Boolean = true) =
    html.admin.submits(AdminSubmitPage(Seq(row), None, None), Map.empty[Int, org.stingray.contester.dbmodel.Team],
      SelectedContest(contest, Seq(3 -> "C"), modify), admin(), team, live, newest).body

  test("a live page has the bar, the table and the script with the stream, the rejudge right, the page and the team") {
    val p = page(live = true)
    assert(p.contains("<div id=\"newSubmits\""))
    assert(p.contains("<table id=\"submits\""))
    assert(p.contains("listenOnStatus('/admin/submits/3/events', true, true, 0);"), p)
    assert(p.contains("window.statusLive = true;"))
    assert(p.contains(rendered(true)), "the page lists rows in the markup that is pushed")
    assert(p.indexOf("id=\"newSubmits\"") < p.indexOf("<table id=\"submits\""))
  }

  test("the script runs after jQuery and main.js are loaded, not in the body before them") {
    val p = page(live = true)
    val jq = p.indexOf("lib/jquery/jquery")
    val main = p.indexOf("js/main.js")
    val mine = p.indexOf("listenOnStatus(")
    assert(jq >= 0 && main > jq, "the page loads jquery and then main.js")
    assert(mine > main, "listenOnStatus is called after main.js defines it")
    assert(p.indexOf("window.statusLive = true;") > jq)
  }

  test("the script is told what the viewer may do and which page this is") {
    // every admin may rejudge, so the rejudge right passed to the script is always true
    assert(page(live = true, modify = false).contains("listenOnStatus('/admin/submits/3/events', true, true, 0);"))
    assert(page(live = true, newest = false).contains("listenOnStatus('/admin/submits/3/events', true, false, 0);"))
    assert(page(live = true, team = Some(7777)).contains("listenOnStatus('/admin/submits/3/events', true, true, 7777);"))
  }

  test("a page without the live monitor is static: no bar, no script") {
    val p = page(live = false)
    assert(!p.contains("newSubmits") && !p.contains("listenOnStatus") && !p.contains("statusLive"))
    assert(p.contains("<table id=\"submits\""))
  }

  test("the page script exists in main.js with the arguments the page passes") {
    val js = { val s = Source.fromFile("app/assets/js/main.js", "UTF-8"); try s.mkString finally s.close() }
    assert(js.contains("function listenOnStatus(path, canRejudge, newest, teamFilter)"))
  }

  test("Russian messages localize the pushed row button, fallback team name and verdict") {
    val ru = MessagesImpl(Lang("ru"), new DefaultMessagesApi(Map("ru" -> loadMessages("conf/messages.ru")),
      new play.api.i18n.DefaultLangs(Seq(Lang("ru")))))
    val r = html.admin.submitrow(row, Map.empty, showRejudge = true, linkTeam = true)(ru).body
    assert(r.contains("Перепроверить") && !r.contains(">Rejudge<"))
    assert(r.contains("Команда 7777") && !r.contains("Team 7777"))
  }
}
