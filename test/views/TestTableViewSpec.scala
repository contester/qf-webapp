package views

import models._
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Memory, TimeMs}
import play.api.i18n.{DefaultMessagesApi, Lang, MessagesImpl}

import scala.io.Source

/** The per-test table renders for a sample list, with the Russian texts of conf/messages.ru. */
class TestTableViewSpec extends AnyFunSuite {
  private def ruMessages: Map[String, String] = {
    val s = Source.fromFile("conf/messages.ru", "UTF-8")
    try s.getLines().map(_.trim).filter(l => l.contains("=") && !l.startsWith("#"))
      .map { l => val i = l.indexOf('='); l.substring(0, i).trim -> l.substring(i + 1).trim }.toMap
    finally s.close()
  }

  private implicit val messages: MessagesImpl = MessagesImpl(Lang("ru"), new DefaultMessagesApi(Map("ru" -> ruMessages)))

  private val rows = TestRows.describe(Seq(
    ResultRow(1, 10, TimeMs(10), Memory(2048), Some("1"), 2.5),
    ResultRow(2, 13, TimeMs(20), Memory(4096), Some("1"), 0),
    ResultRow(3, 10, TimeMs(5), Memory(1024), Some("2"), 0),
    ResultRow(4, 18, TimeMs(0), Memory(0), Some("2"), 0)),
    Seq(GroupRow("1", "each-test", 2.5, 5, passed = false, skipped = false),
      GroupRow("2", "complete-group", 0, 3, passed = false, skipped = false)))

  test("the table has the headers, group, policy and points texts in Russian") {
    val h = html.testtable(rows).body
    for (t <- Seq("Группа", "Политика", "Баллы", "все тесты", "каждый тест", "2.5 (тест пройден)",
      "0 (тест не пройден)", "тест пройден", "не запускался"))
      assert(h.contains(t), t)
    assert(h.indexOf("Wrong answer") > h.indexOf(">Ok<"))
  }
}
