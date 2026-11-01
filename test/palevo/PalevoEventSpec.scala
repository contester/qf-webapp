package palevo

import java.nio.charset.StandardCharsets

import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.funsuite.AnyFunSuite
import play.api.libs.json.Json

class PalevoEventSpec extends AnyFunSuite {
  private val ts = new DateTime(2026, 11, 1, 12, 34, 56, DateTimeZone.UTC)
  private val c = PContest(12, "C")
  private val team = PTeam(7, "Name")

  test("envelope has id/type/ts/contest/data") {
    val e = FirstSolveProblem(c, ts, team, "B", 2467)
    assert(PalevoEvent.toJson(e) == Json.parse(
      """{"id":"qf-first-problem-12-B","type":"first_solve_problem","ts":"2026-11-01T12:34:56Z",
        |"contest":{"id":12,"name":"C"},
        |"data":{"team":{"id":7,"name":"Name"},"problem":"B","elapsed_seconds":2467}}""".stripMargin))
  }

  test("ts is rendered in UTC whatever the zone of the DateTime") {
    val local = ts.withZone(DateTimeZone.forOffsetHours(3))
    assert((PalevoEvent.toJson(ContestStarted(c, local)) \ "ts").as[String] == "2026-11-01T12:34:56Z")
  }

  test("task_created has no contest and carries rooms") {
    val e = TaskCreated(5, ts, "bring water", Seq("101", "102"))
    val j = PalevoEvent.toJson(e)
    assert((j \ "id").as[String] == "qf-task-5")
    assert((j \ "contest").get == Json.obj("id" -> 0, "name" -> ""))
    assert((j \ "data").get == Json.obj("text" -> "bring water", "rooms" -> Seq("101", "102")))
  }

  test("clarification_posted omits problem when None and keeps it when present") {
    val none = PalevoEvent.toJson(ClarificationPosted(3, c, ts, updated = false, None, "t"))
    assert((none \ "data").get == Json.obj("text" -> "t", "updated" -> false))
    assert((none \ "id").as[String] == s"qf-clr-3-${ts.getMillis}")
    val some = PalevoEvent.toJson(ClarificationPosted(3, c, ts, updated = true, Some("A"), "t"))
    assert((some \ "data").get == Json.obj("problem" -> "A", "text" -> "t", "updated" -> true))
  }

  test("clarification_requested") {
    val j = PalevoEvent.toJson(ClarificationRequested(9, c, ts, team, "A", "why?"))
    assert((j \ "id").as[String] == "qf-clr-req-9")
    assert((j \ "type").as[String] == "clarification_requested")
    assert((j \ "data").get == Json.obj("team" -> Json.obj("id" -> 7, "name" -> "Name"), "problem" -> "A", "text" -> "why?"))
  }

  test("print_error omits team when unknown") {
    val j = PalevoEvent.toJson(PrintError(4, c, ts, None, "no paper"))
    assert((j \ "id").as[String] == "qf-print-4")
    assert((j \ "data").get == Json.obj("error" -> "no paper"))
    val k = PalevoEvent.toJson(PrintError(4, c, ts, Some(team), "no paper"))
    assert((k \ "data" \ "team").get == Json.obj("id" -> 7, "name" -> "Name"))
  }

  test("type names and ids of the remaining events") {
    def tp(e: PalevoEvent) = ((PalevoEvent.toJson(e) \ "type").as[String], (PalevoEvent.toJson(e) \ "id").as[String])
    assert(tp(ContestStarted(c, ts)) == ("contest_started", "qf-contest_started-12"))
    assert(tp(ContestFrozen(c, ts)) == ("contest_frozen", "qf-contest_frozen-12"))
    assert(tp(ContestFinished(c, ts)) == ("contest_finished", "qf-contest_finished-12"))
    assert(tp(FirstSolveContest(c, ts, team, "B", 60)) == ("first_solve_contest", "qf-first-contest-12"))
    assert(tp(NewLeader(c, ts, team, 3)) == ("new_leader", "qf-leader-12-7-3"))
    assert((PalevoEvent.toJson(ContestStarted(c, ts)) \ "data").get == Json.obj())
    assert((PalevoEvent.toJson(NewLeader(c, ts, team, 3)) \ "data").get ==
      Json.obj("team" -> Json.obj("id" -> 7, "name" -> "Name"), "solved" -> 3))
  }

  test("Cyrillic text survives UTF-8 serialization") {
    val text = "Почему задача B не принимает ввод?"
    val e = ClarificationRequested(1, c, ts, PTeam(1, "Школа №5"), "B", text)
    val bytes = Json.stringify(PalevoEvent.toJson(e)).getBytes(StandardCharsets.UTF_8)
    val back = Json.parse(new String(bytes, StandardCharsets.UTF_8))
    assert((back \ "data" \ "text").as[String] == text)
    assert((back \ "data" \ "team" \ "name").as[String] == "Школа №5")
  }
}
