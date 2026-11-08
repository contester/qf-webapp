package views

import live.{LiveBoard, LiveCell, LiveJson, StaticRender}
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, LocalTeam}
import play.api.i18n.{DefaultMessagesApi, Lang, MessagesImpl}
import play.api.libs.json.Json

import scala.io.Source

// The live table markup contract: livetable.scala.html (static files) and liveTableHtml in main.js (live pages)
// must both produce test/resources/live/livetable.html for the board in test/resources/live/livetable.json.
object LiveTableFixture {
  val teams = Seq(
    LocalTeam(1, 1, "Lyceum & Co", "L", 2, "<Alpha>", notRated = false, noPrint = false, disabled = false),
    LocalTeam(2, 1, "School 2", "S2", 0, "Beta's", notRated = true, noPrint = false, disabled = false))

  val board: LiveBoard = LiveBoard.build(Seq("A", "B", "C", "D"), teams, Map(
    1 -> Map("A" -> LiveCell.Solved(2, 1260), "B" -> LiveCell.Failed(2), "C" -> LiveCell.Pending(0)),
    2 -> Map("A" -> LiveCell.Pending(1))))

  private def resource(name: String): String = {
    val s = Source.fromResource(s"live/$name", getClass.getClassLoader)
    try s.mkString finally s.close()
  }

  def html: String = resource("livetable.html").trim
  def json: String = resource("livetable.json").trim
}

class LiveTableSpec extends AnyFunSuite {
  private implicit val messages: MessagesImpl = MessagesImpl(Lang("en"), new DefaultMessagesApi())

  private def contest(start: DateTime) =
    Contest(1, "Open & Cup", start, start.plusHours(4), start.plusHours(5), start.plusHours(6), None, "en", schoolMode = false)

  test("livetable renders the exact contract markup: solved with time, failed, pending, empty, not-rated rank, highlight") {
    assert(html.livetable(LiveTableFixture.board, currentTeam = 1).body.trim == LiveTableFixture.html)
  }

  test("livetable shows DSQ and a red row for a disqualified team") {
    val dsq = LocalTeam(3, 1, "School 3", "S3", 0, "", notRated = true, noPrint = false, disabled = false, disqualified = true)
    val b = LiveBoard.build(Seq("A"), Seq(dsq), Map(3 -> Map("A" -> LiveCell.Solved(1, 60))))
    val out = html.livetable(b, currentTeam = 3).body
    assert(out.contains("""<tr data-team="3" class="danger"><th scope="row">DSQ</th>"""), out)
  }

  test("the JSON fixture used by the browser-side check is LiveJson.board of the same board") {
    assert(Json.parse(LiveTableFixture.json) == LiveJson.board(LiveTableFixture.board))
  }

  test("static page after start: the table without highlight, escaped title, 60 s reload") {
    val page = StaticRender.render(contest(DateTime.now.minusHours(1)), LiveTableFixture.board)
    assert(page.contains(html.livetable(LiveTableFixture.board, currentTeam = 0).body.trim))
    assert(!page.contains("class=\"info\""))
    assert(page.contains("<title>Open &amp; Cup</title>"))
    assert(page.contains("location.reload()"))
    assert(page.contains("60000"))
  }

  test("static page before start: no table body") {
    val page = StaticRender.render(contest(DateTime.now.plusHours(1)), LiveTableFixture.board)
    assert(page.contains("<title>Open &amp; Cup</title>"))
    assert(!page.contains("<tbody"))
    assert(!page.contains("<table"))
  }
}
