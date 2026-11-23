package views

import live.{LiveBoard, LiveCell, LiveJson}
import models.{LoggedInTeam, ProblemColumn}
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, NotRated, Team}
import play.api.i18n.{DefaultMessagesApi, Lang, MessagesImpl}
import play.api.libs.json.Json
import play.api.libs.typedmap.TypedMap
import play.api.mvc.{AnyContentAsEmpty, Headers}
import play.api.mvc.request.{RemoteConnection, RequestFactory, RequestTarget}

import scala.io.Source

// The admin live table markup contract: livetable.scala.html (the page) and liveTableHtml in main.js (the stream)
// must both produce test/resources/live/livetable.html for the board in test/resources/live/livetable.json.
object LiveTableFixture {
  val problems = Seq(ProblemColumn("A", "Alpha's", 10), ProblemColumn("B", "B&B", 2.5))

  val teams = Seq(
    Team(1, 2, "<Alpha>", "Lyceum & Co", "City", None),
    Team(2, 5, "Beta's", "School 2", "City", Some(NotRated)))

  val board: LiveBoard = LiveBoard.build(problems, teams, Map(
    1 -> Map("A" -> LiveCell(Some(10), 1, full = true, 0, Some(1260)), "B" -> LiveCell(None, 2, full = false, 1, None)),
    2 -> Map("A" -> LiveCell(Some(4), 2, full = false, 1, None))))

  private def resource(name: String): String = {
    val s = Source.fromResource(s"live/$name", getClass.getClassLoader)
    try s.mkString finally s.close()
  }

  def html: String = resource("livetable.html").trim
  def json: String = resource("livetable.json").trim
}

class LiveTableSpec extends AnyFunSuite {
  private implicit val messages: MessagesImpl = MessagesImpl(Lang("en"), new DefaultMessagesApi())

  test("livetable renders the exact contract markup: full, partial, no points, in flight, unranked, escaping") {
    assert(html.livetable(LiveTableFixture.board).body.trim == LiveTableFixture.html)
  }

  test("the JSON fixture used by the browser-side check is LiveJson.board of the same board") {
    assert(Json.parse(LiveTableFixture.json) == LiveJson.board(LiveTableFixture.board))
  }

  test("the admin page shell carries the table and the labels the script needs, and no freeze banner") {
    val shell = html.liveshell(Contest(1, "R", org.joda.time.DateTime.now.minusHours(1),
      org.joda.time.DateTime.now.plusHours(1), None, 1), LiveTableFixture.board).body
    assert(shell.contains(html.livetable(LiveTableFixture.board).body.trim))
    assert(shell.contains("data-label-total=\"label.total\""))
    assert(!shell.contains("contest-frozen") && !shell.contains("frozen"))
  }

  test("currentTeam adds class=\"info\" only to that team's row, and teamsOnly renders without problem columns") {
    val rendered = html.livetable(LiveTableFixture.board, currentTeam = 2).body.trim
    assert(rendered.contains("""<tr data-team="1">"""))
    assert(rendered.contains("""<tr data-team="2" class="info">"""))

    val teamsOnly = LiveBoard.build(Nil, LiveTableFixture.teams, Map.empty)
    val teamsRendered = html.livetable(teamsOnly, currentTeam = 1).body.trim
    assert(teamsRendered.contains("""<tr data-team="1" class="info">"""))
    assert(teamsRendered.contains("""<tr data-team="2">"""))
    assert(!teamsRendered.contains("points-cell"))

    implicit val request = RequestFactory.plain.createRequest(RemoteConnection("127.0.0.1", secure = false, None), "GET",
      RequestTarget("/monitor", "/monitor", Map.empty), "HTTP/1.1", Headers(), TypedMap.empty, AnyContentAsEmpty)
    val contest = Contest(1, "R", org.joda.time.DateTime.now.minusHours(1), org.joda.time.DateTime.now.plusHours(1), None, 1)
    val loggedIn = LoggedInTeam("u2", contest, LiveTableFixture.teams(1), Nil)
    val page = html.livemonitor(contest, loggedIn, LiveTableFixture.board).body
    assert(page.contains("liveMonitor('/monitor/events', '#live-monitor', 2);"))
    assert(page.contains("""<tr data-team="2" class="info">"""))
    assert(page.contains("tr.info > td.full-score"))
  }
}
