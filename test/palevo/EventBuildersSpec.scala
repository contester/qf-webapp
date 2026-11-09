package palevo

import models.StoredWaiterTask
import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.ClarificationRequest

class EventBuildersSpec extends AnyFunSuite {
  private val now = new DateTime(2026, 11, 1, 12, 0)
  private val c = PContest(12, "C")
  private val team = PTeam(7, "Name")

  test("printerProblem sorts the areas") {
    assert(EventBuilders.printerProblem("p", Seq("B", "A"), open = true, "jam", 1, now) ==
      PrinterProblem("p", Seq("A", "B"), open = true, "jam", 1, now))
  }

  test("clarificationRequested maps the request") {
    val req = ClarificationRequest(5, 12, 7, "B", "what?", "", now.minusMinutes(1), answered = false)
    assert(EventBuilders.clarificationRequested(req, c, team, now) ==
      ClarificationRequested(5, c, now, team, "B", "what?"))
  }

  test("clarificationRequested keeps an empty problem as empty string (general question)") {
    val req = ClarificationRequest(5, 12, 7, "", "what?", "", now, answered = false)
    assert(EventBuilders.clarificationRequested(req, c, team, now).asInstanceOf[ClarificationRequested].problem == "")
  }

  test("clarificationAnswered: the stored request, with its question and answer") {
    val req = ClarificationRequest(4, 12, 7, "B", "why?", "because", now, answered = true)
    assert(EventBuilders.clarificationAnswered(req, c, PTeam(7, "N"), now) ==
      ClarificationAnswered(4, c, now, PTeam(7, "N"), "B", "why?", "because"))
  }

  test("clarificationPosted: empty problem becomes None, non-empty is kept") {
    assert(EventBuilders.clarificationPosted(3, c, updated = false, "", "t", now) ==
      ClarificationPosted(3, c, now, updated = false, None, "t"))
    assert(EventBuilders.clarificationPosted(3, c, updated = true, "A", "t", now) ==
      ClarificationPosted(3, c, now, updated = true, Some("A"), "t"))
  }

  test("taskCreated copies message and sorted rooms") {
    val task = StoredWaiterTask(8, now, "bring water", Set("102", "101"), Map.empty)
    assert(EventBuilders.taskCreated(task, now) == TaskCreated(8, now, "bring water", Seq("101", "102")))
  }

  test("taskCreated flags a task posted to all rooms") {
    val task = StoredWaiterTask(9, now, "bring water", Set("101", "102"), Map.empty, allRooms = true)
    assert(EventBuilders.taskCreated(task, now) == TaskCreated(9, now, "bring water", Seq("101", "102"), allRooms = true))
    assert((EventBuilders.taskCreated(task, now).data \ "all_rooms").as[Boolean])
  }

  test("unknown team gets a placeholder name rather than dropping the event") {
    assert(EventBuilders.teamOrPlaceholder(7, None) == PTeam(7, "team 7"))
  }

  test("printError with and without a team") {
    assert(EventBuilders.printError(4, c, Some(team), "no paper", now) == PrintError(4, c, now, Some(team), "no paper"))
    assert(EventBuilders.printError(4, c, None, "no paper", now) == PrintError(4, c, now, None, "no paper"))
  }
}
