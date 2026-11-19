package live

import models.TestRows.formatPoints
import play.api.libs.EventSource.Event
import play.api.libs.json.{JsNull, JsObject, JsValue, Json}

/**
  * The admin live monitor's JSON. A cell: `b` the best points as shown (absent: none), `n` the attempts, `f` the
  * cell is at the problem's maximum, `p` how many submits are in flight. A place is a number, or null for a team that
  * is not rated.
  */
object LiveJson {
  def cell(c: LiveCell): JsObject =
    Json.obj("n" -> c.attempts, "f" -> c.full, "p" -> c.pending) ++
      c.best.fold(Json.obj())(b => Json.obj("b" -> formatPoints(b)))

  private def place(p: Option[Int]): JsValue = p.fold[JsValue](JsNull)(Json.toJson(_))

  private def rowObj(r: LiveRow): JsObject =
    Json.obj(
      "team" -> r.team.id,
      "number" -> r.team.number,
      "name" -> r.team.name,
      "school" -> r.team.schoolWithCity,
      "place" -> place(r.place),
      "total" -> formatPoints(r.total),
      "cells" -> JsObject(r.cells.toSeq.sortBy(_._1).map { case (p, c) => p -> cell(c) }))

  def board(b: LiveBoard): JsValue =
    Json.obj(
      "problems" -> b.problems.map(p => Json.obj("id" -> p.id, "name" -> p.name, "max" -> formatPoints(p.maxPoints))),
      "rows" -> b.rows.map(rowObj))

  def row(c: RowChange): JsValue =
    Json.obj(
      "team" -> c.team,
      "pos" -> c.position,
      "row" -> rowObj(c.row),
      "rankFrom" -> c.rankFrom,
      "ranks" -> c.ranks.map(place))

  def event(name: String, js: JsValue): Event = Event(Json.stringify(js), None, Some(name))
}
