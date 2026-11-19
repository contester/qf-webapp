package live

import models.ProblemColumn
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{NotRated, Team}
import play.api.libs.json.{JsNull, JsObject, Json}

class LiveJsonSpec extends AnyFunSuite {
  private def team(id: Int, notRated: Boolean = false): Team =
    Team(id, 10 + id, s"Team $id", f"School$id%02d", "City", if (notRated) Some(NotRated) else None)

  private val problems = Seq(ProblemColumn("A", "Alpha", 10), ProblemColumn("B", "Beta", 2.5))
  private val b = LiveBoard.build(problems, Seq(team(7), team(8), team(9, notRated = true)), Map(
    7 -> Map("A" -> LiveCell(Some(10), 2, full = true, 1, Some(600)), "B" -> LiveCell(None, 1, full = false, 1, None)),
    8 -> Map("B" -> LiveCell(Some(1.25), 1, full = false, 0, None)),
    9 -> Map("A" -> LiveCell(Some(10), 1, full = true, 0, Some(50)))))

  test("board json: problems with name and maximum, rows in board order, places (null unrated), totals, cells") {
    val js = LiveJson.board(b)
    assert((js \ "problems").get == Json.arr(Json.obj("id" -> "A", "name" -> "Alpha", "max" -> "10"),
      Json.obj("id" -> "B", "name" -> "Beta", "max" -> "2.5")))
    val rows = (js \ "rows").as[Seq[JsObject]]
    assert(rows.map(r => (r \ "team").as[Int]) == Seq(7, 8, 9))
    assert(rows.map(r => (r \ "place").get) == Seq(Json.toJson(1), Json.toJson(2), JsNull))
    assert(rows.head == Json.obj("team" -> 7, "number" -> 17, "name" -> "Team 7", "school" -> "School07, City",
      "place" -> 1, "total" -> "10", "cells" -> Json.obj(
        "A" -> Json.obj("n" -> 2, "f" -> true, "p" -> 1, "b" -> "10"),
        "B" -> Json.obj("n" -> 1, "f" -> false, "p" -> 1))))
    assert((rows(1) \ "total").as[String] == "1.25")
  }

  test("row json carries the position and the places from rankFrom") {
    val (_, out) = b.update(team(8), Map("A" -> LiveCell(Some(10), 1, full = true, 0, Some(70)), "B" -> LiveCell(Some(1.25), 1, full = false, 0, None)))
    val ch = out.collect { case RowOut(c) => c }.get
    val js = LiveJson.row(ch)
    assert((js \ "team").as[Int] == 8 && (js \ "pos").as[Int] == 0 && (js \ "rankFrom").as[Int] == 0)
    assert((js \ "ranks").get == Json.arr(1, 2))
    assert((js \ "row" \ "total").as[String] == "11.25")
  }

  test("event") {
    val e = LiveJson.event("row", Json.obj("a" -> 1))
    assert(e.name.contains("row") && e.data == """{"a":1}""")
  }
}
