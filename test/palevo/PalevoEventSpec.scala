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
    assert((PalevoEvent.toJson(ContestStarting(c, local)) \ "ts").as[String] == "2026-11-01T12:34:56Z")
  }

  test("clarification_posted omits problem when None and keeps it when present") {
    val none = PalevoEvent.toJson(ClarificationPosted(3, c, ts, updated = false, None, "t"))
    assert((none \ "data").get == Json.obj("text" -> "t", "updated" -> false))
    assert((none \ "id").as[String] == s"qf-clr-3-${ts.getMillis}")
    val some = PalevoEvent.toJson(ClarificationPosted(3, c, ts, updated = true, Some("A"), "t"))
    assert((some \ "data").get == Json.obj("problem" -> "A", "text" -> "t", "updated" -> true))
  }

  test("clarification_answered carries the team, the problem, the question and the answer") {
    val j = PalevoEvent.toJson(ClarificationAnswered(9, c, ts, team, "A", "why?", "because"))
    assert((j \ "id").as[String] == s"qf-clr-ans-9-${ts.getMillis}")
    assert((j \ "type").as[String] == "clarification_answered")
    assert((j \ "data").get == Json.obj("team" -> Json.obj("id" -> 7, "name" -> "Name"), "problem" -> "A",
      "question" -> "why?", "answer" -> "because"))
  }

  test("clarification_requested") {
    val j = PalevoEvent.toJson(ClarificationRequested(9, c, ts, team, "A", "why?"))
    assert((j \ "id").as[String] == s"qf-clr-req-9-${ts.getMillis}")
    assert((j \ "type").as[String] == "clarification_requested")
    assert((j \ "data").get == Json.obj("team" -> Json.obj("id" -> 7, "name" -> "Name"), "problem" -> "A", "text" -> "why?"))
  }

  test("type names and ids of the remaining events") {
    def tp(e: PalevoEvent) = ((PalevoEvent.toJson(e) \ "type").as[String], (PalevoEvent.toJson(e) \ "id").as[String])
    assert(tp(ContestStarting(c, ts)) == ("contest_starting", "qf-contest_starting-12"))
    assert(tp(ContestEnding(c, ts)) == ("contest_ending", "qf-contest_ending-12"))
    assert(tp(FirstSolveContest(c, ts, team, "B", 60)) == ("first_solve_contest", "qf-first-contest-12"))
    assert(tp(NewLeader(c, ts, team, 3)) == ("new_leader", "qf-leader-12-7-3"))
    assert((PalevoEvent.toJson(ContestStarting(c, ts)) \ "data").get == Json.obj())
    assert((PalevoEvent.toJson(NewLeader(c, ts, team, 3)) \ "data").get ==
      Json.obj("team" -> Json.obj("id" -> 7, "name" -> "Name"), "solved" -> 3))
  }

  test("phase events ahead carry in_minutes and at, and an id with both") {
    val at = new DateTime(2026, 11, 1, 15, 30, DateTimeZone.UTC)
    val e = ContestStarting(c, ts, inMinutes = 10, at = Some(at))
    val j = PalevoEvent.toJson(e)
    assert((j \ "type").as[String] == "contest_starting")
    assert((j \ "id").as[String] == s"qf-contest_starting-12-in10-${at.getMillis}")
    assert((j \ "data").get == Json.obj("in_minutes" -> 10, "at" -> "2026-11-01T15:30:00Z"))
    assert((PalevoEvent.toJson(ContestEnding(c, ts, 5, Some(at))) \ "id").as[String] == s"qf-contest_ending-12-in5-${at.getMillis}")
  }

  test("a phase event ahead without a target time is a programming error") {
    assertThrows[IllegalArgumentException](ContestStarting(c, ts, inMinutes = 10))
  }

  test("contest_rescheduled lists changes with old and new in UTC") {
    val old = new DateTime(2026, 11, 1, 14, 0, DateTimeZone.UTC)
    val nw = new DateTime(2026, 11, 1, 15, 30, DateTimeZone.forOffsetHours(3)) // 12:30 UTC
    val e = ContestRescheduled(c, ts, Seq(TimeChange("start", old, nw)))
    val j = PalevoEvent.toJson(e)
    assert((j \ "type").as[String] == "contest_rescheduled")
    assert((j \ "id").as[String] == s"qf-contest_rescheduled-12-${ts.getMillis}")
    assert((j \ "data").get == Json.parse(
      """{"changes":[{"what":"start","old":"2026-11-01T14:00:00Z","new":"2026-11-01T12:30:00Z"}]}"""))
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
    assert(ClarificationPosted(1, c, ts, updated = false, None, "t").id != ClarificationPosted(1, c, later, updated = false, None, "t").id)
    assert(FinalStandings(c, ts, Nil, updated = false).id != FinalStandings(c, later, Nil, updated = true).id)
  }

  test("a retry of the same event keeps the same id") {
    val e = ClarificationRequested(1, c, ts, team, "A", "t")
    assert(PalevoEvent.toJson(e) == PalevoEvent.toJson(e))
  }

  test("public events carry no audience key") {
    val events = Seq(ContestStarting(c, ts), FirstSolveProblem(c, ts, team, "B", 1), FirstSolveContest(c, ts, team, "B", 1),
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

  test("final_standings: public, the top places with team, points and solved, and whether it is an update") {
    val e = FinalStandings(c, ts, Seq(FinalRow(1, team, 545.0, 12), FinalRow(2, PTeam(8, "Other"), 100.5, 2)), updated = false)
    val j = PalevoEvent.toJson(e)
    assert((j \ "audience").toOption.isEmpty && e.audience == PublicAudience)
    assert(j == Json.parse(
      s"""{"id":"qf-final-12-${ts.getMillis}","type":"final_standings","ts":"2026-11-01T12:34:56Z",
         |"contest":{"id":12,"name":"C"},
         |"data":{"standings":[{"place":1,"team":{"id":7,"name":"Name"},"points":545,"solved":12},
         |{"place":2,"team":{"id":8,"name":"Other"},"points":100.5,"solved":2}],"updated":false}}""".stripMargin))
  }

  test("the remaining event types: no freeze, printing or waiter events") {
    val at = Some(ts.plusHours(1))
    val all: Seq[PalevoEvent] = Seq(
      ClarificationRequested(1, c, ts, team, "A", "t"), ClarificationAnswered(1, c, ts, team, "A", "q", "a"),
      ClarificationPosted(1, c, ts, updated = false, None, "t"), ContestStarting(c, ts), ContestStarting(c, ts, 5, at),
      ContestEnding(c, ts), ContestEnding(c, ts, 5, at), ContestRescheduled(c, ts, Nil), FirstSolveContest(c, ts, team, "A", 1),
      FirstSolveProblem(c, ts, team, "A", 1), NewLeader(c, ts, team, 1), FinalStandings(c, ts, Nil, updated = false))
    assert(all.map(_.eventType).toSet == Set("clarification_requested", "clarification_answered", "clarification_posted",
      "contest_starting", "contest_ending", "contest_rescheduled", "first_solve_contest", "first_solve_problem",
      "new_leader", "final_standings"))
  }
}
