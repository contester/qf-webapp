package views

import models._
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{NotRated, Team}
import play.api.i18n.{DefaultMessagesApi, Lang, MessagesImpl}

import scala.io.Source

/** The points board renders: problem columns with their maximum, points, the full-score class, totals and places. */
class MonitorTableViewSpec extends AnyFunSuite {
  private def ruMessages: Map[String, String] = {
    val s = Source.fromFile("conf/messages.ru", "UTF-8")
    try s.getLines().map(_.trim).filter(l => l.contains("=") && !l.startsWith("#"))
      .map { l => val i = l.indexOf('='); l.substring(0, i).trim -> l.substring(i + 1).trim }.toMap
    finally s.close()
  }

  private implicit val messages: MessagesImpl = MessagesImpl(Lang("ru"), new DefaultMessagesApi(Map("ru" -> ruMessages)))

  private val problems = Seq(ProblemColumn("A", "Alpha", 20), ProblemColumn("B", "Beta", 25))
  private val status = MonitorPoints(problems, Seq(
    RankedRow(Some(1), Team(11, 7, "Seven", "Lyceum 1", "Town", None),
      Seq(PointsCell(Some(20), 2, full = true), PointsCell(Some(12.5), 1, full = false)), 32.5),
    RankedRow(None, Team(12, 8, "Eight", "School 2", "Village", Some(NotRated)),
      Seq(PointsCell(None, 0, full = false), PointsCell(None, 1, full = false)), 0)))

  test("cells, the full-score class, totals, places and team data are on the table") {
    val h = html.monitortable(status, current = 11).body
    assert("points-cell full-score".r.findAllMatchIn(h).size == 1, "only the full cell carries the class")
    assert("points-cell".r.findAllMatchIn(h).size == 4)
    for (t <- Seq(">32.5<", "12.5", "Seven", "Lyceum 1, Town", "Eight", "title=\"Alpha\"", "title=\"Beta\"",
      "20 баллов", "25 баллов", "—", "Место", "Итого", "Участник"))
      assert(h.contains(t), t)
    assert(!h.contains("Команда"))
    assert(!h.contains("Alpha (20)") && !h.contains("(20)") && !h.contains("(25)"))
    // the unrated row has no place; the ranked row's place is 1
    assert(h.contains("<th scope=\"row\">1</th>"))
    assert(h.contains("<th scope=\"row\"></th>"))
    assert(h.indexOf("Seven") < h.indexOf("Eight"))
    assert(h.indexOf("Итого") < h.indexOf("20 баллов"))
    assert(h.indexOf(">32.5<") < h.indexOf("12.5"))
  }

  test("the stylesheet gives full-score a colour") {
    val css = html.boardstyle().body
    assert(css.contains(".full-score"))
  }
}
