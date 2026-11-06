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
    assert((j \ "id").as[String] == s"qf-task-5-${ts.getMillis}")
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
    assert((j \ "id").as[String] == s"qf-clr-req-9-${ts.getMillis}")
    assert((j \ "type").as[String] == "clarification_requested")
    assert((j \ "data").get == Json.obj("team" -> Json.obj("id" -> 7, "name" -> "Name"), "problem" -> "A", "text" -> "why?"))
  }

  test("print_error omits team when unknown") {
    val j = PalevoEvent.toJson(PrintError(4, c, ts, None, "no paper"))
    assert((j \ "id").as[String] == s"qf-print-4-${ts.getMillis}")
    assert((j \ "data").get == Json.obj("error" -> "no paper"))
    val k = PalevoEvent.toJson(PrintError(4, c, ts, Some(team), "no paper"))
    assert((k \ "data" \ "team").get == Json.obj("id" -> 7, "name" -> "Name"))
  }

  test("printer_problem: public, no contest, condition only while open") {
    val open = PalevoEvent.toJson(PrinterProblem("""\\\\srv\\p1""", Seq("Hall A"), open = true, "out of paper", 2, ts))
    assert((open \ "type").as[String] == "printer_problem")
    assert((open \ "audience").toOption.isEmpty)
    assert((open \ "contest" \ "id").as[Int] == 0)
    assert((open \ "data").get == Json.obj("printer" -> """\\\\srv\\p1""", "areas" -> Seq("Hall A"), "state" -> "open",
      "condition" -> "out of paper", "waiting_jobs" -> 2))
    val cleared = PalevoEvent.toJson(PrinterProblem("""\\\\srv\\p1""", Nil, open = false, "", 0, ts))
    assert((cleared \ "data").get == Json.obj("printer" -> """\\\\srv\\p1""", "areas" -> Seq.empty[String], "state" -> "cleared"))
    assert((open \ "id").as[String] != (cleared \ "id").as[String])
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

  test("ids of one-shot events differ when a database sequence restarts and reuses a row id") {
    val later = ts.plusDays(30)
    assert(ClarificationRequested(1, c, ts, team, "A", "t").id != ClarificationRequested(1, c, later, team, "A", "t").id)
    assert(TaskCreated(1, ts, "t", Nil).id != TaskCreated(1, later, "t", Nil).id)
    assert(PrintError(1, c, ts, None, "e").id != PrintError(1, c, later, None, "e").id)
  }

  test("a retry of the same event keeps the same id") {
    val e = ClarificationRequested(1, c, ts, team, "A", "t")
    assert(PalevoEvent.toJson(e) == PalevoEvent.toJson(e))
  }

  test("public events carry no audience key") {
    val events = Seq(ContestStarted(c, ts), FirstSolveProblem(c, ts, team, "B", 1), FirstSolveContest(c, ts, team, "B", 1),
      NewLeader(c, ts, team, 1), ClarificationPosted(1, c, ts, updated = false, None, "t"))
    events.foreach { e =>
      assert((PalevoEvent.toJson(e) \ "audience").toOption.isEmpty, e.eventType)
      assert(e.audience == PublicAudience)
    }
  }

  test("jury standings events carry audience and qf-jury- ids") {
    val fp = FirstSolveProblem(c, ts, team, "B", 60, JuryAudience)
    val fc = FirstSolveContest(c, ts, team, "B", 60, JuryAudience)
    val nl = NewLeader(c, ts, team, 3, JuryAudience)
    assert(fp.id == "qf-jury-first-problem-12-B")
    assert(fc.id == "qf-jury-first-contest-12")
    assert(nl.id == "qf-jury-leader-12-7-3")
    for (e <- Seq[PalevoEvent](fp, fc, nl)) {
      assert((PalevoEvent.toJson(e) \ "audience").as[String] == "jury", e.eventType)
      assert((PalevoEvent.toJson(e) \ "id").as[String].startsWith("qf-jury-"))
    }
    // the payload itself is unchanged
    assert((PalevoEvent.toJson(fp) \ "data").get == (PalevoEvent.toJson(FirstSolveProblem(c, ts, team, "B", 60)) \ "data").get)
  }

  test("a public and a jury event for the same fact never share an id") {
    assert(FirstSolveProblem(c, ts, team, "B", 60).id != FirstSolveProblem(c, ts, team, "B", 60, JuryAudience).id)
    assert(FirstSolveContest(c, ts, team, "B", 60).id != FirstSolveContest(c, ts, team, "B", 60, JuryAudience).id)
    assert(NewLeader(c, ts, team, 3).id != NewLeader(c, ts, team, 3, JuryAudience).id)
  }
}
