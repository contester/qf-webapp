package live

import models.ACM
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{LocalTeam, Team}
import play.api.libs.json.Json

class LiveJsonSpec extends AnyFunSuite {
  import LiveCell._

  private def team(id: Int, notRated: Boolean = false): Team =
    LocalTeam(id, 1, f"School$id%02d", s"S$id", 0, "", notRated, noPrint = false, disabled = false)

  private val problems = Seq("A", "B", "C", "D")
  private val c7: Map[String, LiveCell] = Map("A" -> Solved(2, 600), "B" -> Failed(3), "C" -> Pending(1), "D" -> Empty)
  private val real7: Map[String, LiveCell] = Map("A" -> Solved(2, 600), "B" -> Failed(3), "C" -> Solved(1, 4000), "D" -> Empty)
  private val c8: Map[String, LiveCell] = Map("A" -> Solved(1, 100))

  private val b = LiveBoard.build(problems, Seq(team(7), team(8), team(9, notRated = true)),
    Map(7 -> c7, 8 -> c8, 9 -> Map("A" -> Solved(1, 50))))

  test("board json") {
    val js = LiveJson.board(b)
    assert((js \ "problems").get == Json.arr("A", "B", "C", "D"))
    assert((js \ "solved").get == Json.obj("A" -> 3, "B" -> 0, "C" -> 0, "D" -> 0))
    val rows = (js \ "rows").as[Seq[play.api.libs.json.JsObject]]
    assert(rows.map(r => (r \ "team").as[Int]) == Seq(9, 8, 7))
    assert(rows.map(r => (r \ "rank").as[String]) == Seq("*", "1", "2"))
    val r7 = rows(2)
    assert(r7 == Json.obj(
      "team" -> 7, "school" -> team(7).schoolFullNameWithNum, "name" -> team(7).teamName,
      "rank" -> "2", "solved" -> 1, "penalty" -> b.rowOf(7).get.score.penalty,
      "cells" -> Json.obj(
        "A" -> Json.obj("t" -> "ok", "s" -> Solved(2, 600).text, "at" -> "00:10"),
        "B" -> Json.obj("t" -> "fail", "s" -> Failed(3).text),
        "C" -> Json.obj("t" -> "pend", "s" -> "?1"),
        "D" -> Json.obj("t" -> "empty", "s" -> ""))))
  }

  test("board with own differs only in that team's cells") {
    val plain = LiveJson.board(b).as[play.api.libs.json.JsObject]
    val own = LiveJson.board(b, Some((7, real7))).as[play.api.libs.json.JsObject]
    assert(plain != own)
    val pr = (plain \ "rows").as[Seq[play.api.libs.json.JsObject]]
    val or = (own \ "rows").as[Seq[play.api.libs.json.JsObject]]
    assert(pr.size == or.size)
    pr.zip(or).foreach { case (p, o) =>
      if ((p \ "team").as[Int] == 7) {
        assert(p - "cells" == o - "cells")
        assert((o \ "cells" \ "C" \ "t").as[String] == "ok")
      } else assert(p == o)
    }
    assert((plain - "rows") == (own - "rows"))
  }

  test("row and cells json") {
    val ch = RowChange(7, 1, b.rowOf(7).get, 1, Seq(2, 3), Map("A" -> 3))
    val js = LiveJson.row(ch).as[play.api.libs.json.JsObject]
    assert((js \ "team").as[Int] == 7)
    assert((js \ "pos").as[Int] == 1)
    assert((js \ "rankFrom").as[Int] == 1)
    assert((js \ "ranks").as[Seq[Int]] == Seq(2, 3))
    assert((js \ "solved").get == Json.obj("A" -> 3))
    val own = LiveJson.row(ch, Some(real7)).as[play.api.libs.json.JsObject]
    assert(own - "row" == js - "row")
    assert((own \ "row").as[play.api.libs.json.JsObject] - "cells" == (js \ "row").as[play.api.libs.json.JsObject] - "cells")
    assert((own \ "row" \ "cells" \ "C" \ "t").as[String] == "ok")
    assert(LiveJson.cells(7, c8) == Json.obj("team" -> 7, "cells" -> Json.obj("A" -> Json.obj("t" -> "ok", "s" -> Solved(1, 100).text, "at" -> "00:01"))))
  }

  test("event") {
    val e = LiveJson.event("cells", Json.obj("a" -> 1))
    assert(e.name == Some("cells"))
    assert(e.data == """{"a":1}""")
  }
}
