package live

import models.SecondsToTimeStr
import play.api.libs.EventSource.Event
import play.api.libs.json.{JsObject, JsValue, Json}

object LiveJson {
  private def cell(c: LiveCell): JsObject = c match {
    case LiveCell.Solved(_, seconds) => Json.obj("t" -> "ok", "s" -> c.text, "at" -> SecondsToTimeStr(seconds))
    case LiveCell.Failed(_) => Json.obj("t" -> "fail", "s" -> c.text)
    case LiveCell.Pending(_) => Json.obj("t" -> "pend", "s" -> c.text)
    case LiveCell.Empty => Json.obj("t" -> "empty", "s" -> "")
  }

  private def cellsObj(keys: Seq[String], cells: Map[String, LiveCell]): JsObject =
    JsObject(keys.map(p => p -> cell(cells.getOrElse(p, LiveCell.Empty))))

  private def rowObj(r: LiveRow, cells: Map[String, LiveCell], keys: Seq[String]): JsObject =
    Json.obj(
      "team" -> r.team.id,
      "school" -> r.team.schoolFullNameWithNum,
      "name" -> r.team.teamName,
      "rank" -> (if (r.team.disqualified) "DSQ" else if (r.rank == 0) "*" else r.rank.toString),
      "solved" -> r.score.solved,
      "penalty" -> r.score.penalty,
      "cells" -> cellsObj(keys, cells))

  private def counts(m: Map[String, Int]): JsObject = JsObject(m.toSeq.map { case (k, v) => k -> Json.toJson(v) })

  def board(b: LiveBoard, own: Option[(Int, Map[String, LiveCell])] = None): JsValue =
    Json.obj(
      "problems" -> b.problems,
      "solved" -> counts(b.solvedCounts),
      "rows" -> b.rows.map { r =>
        val cs = own.collect { case (id, c) if id == r.team.id => c }.getOrElse(r.cells)
        rowObj(r, cs, b.problems)
      })

  def row(c: RowChange, own: Option[Map[String, LiveCell]] = None): JsValue = {
    val cs = own.getOrElse(c.row.cells)
    val keys = (c.row.cells.keys ++ cs.keys).toSeq.distinct.sorted
    Json.obj(
      "team" -> c.team,
      "pos" -> c.position,
      "row" -> rowObj(c.row, cs, keys),
      "rankFrom" -> c.rankFrom,
      "ranks" -> c.ranks,
      "solved" -> counts(c.solved))
  }

  def cells(team: Int, cells: Map[String, LiveCell]): JsValue =
    Json.obj("team" -> team, "cells" -> cellsObj(cells.keys.toSeq.sorted, cells))

  def event(name: String, js: JsValue): Event = Event(Json.stringify(js), None, Some(name))
}
