package models

import org.scalatest.funsuite.AnyFunSuite
import protos.Tickets.PrinterAlert

class PrinterAlertStateSpec extends AnyFunSuite {
  private def alert(open: Boolean, condition: String = "", printer: String = "p1", agent: String = "a") =
    PrinterAlert(agent = agent, printer = printer, open = open, condition = condition)

  test("a new problem is news, its repeat is not") {
    val s = new PrinterAlertState
    assert(s.isNews(alert(open = true, "paper out")))
    assert(!s.isNews(alert(open = true, "paper out")))
  }

  test("a changed condition is news again") {
    val s = new PrinterAlertState
    assert(s.isNews(alert(open = true, "paper out")))
    assert(s.isNews(alert(open = true, "paper out, door open")))
  }

  test("clearing is news only for a problem that was announced") {
    val s = new PrinterAlertState
    assert(!s.isNews(alert(open = false)))
    assert(s.isNews(alert(open = true, "jam")))
    assert(s.isNews(alert(open = false)))
    assert(!s.isNews(alert(open = false)))
    assert(s.isNews(alert(open = true, "jam")))
  }

  test("printers and agents are told apart") {
    val s = new PrinterAlertState
    assert(s.isNews(alert(open = true, "jam")))
    assert(s.isNews(alert(open = true, "jam", printer = "p2")))
    assert(s.isNews(alert(open = true, "jam", agent = "b")))
    assert(s.isNews(alert(open = false, printer = "p2")))
    assert(!s.isNews(alert(open = true, "jam")))
  }
}
