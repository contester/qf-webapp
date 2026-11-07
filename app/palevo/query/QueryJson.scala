package palevo.query

import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.ISODateTimeFormat
import org.stingray.contester.dbmodel.Contest
import play.api.libs.json.{JsNull, JsObject, JsValue, Json}

/** JSON writers for the query API: the response envelope and the per-contest pieces shared by the endpoints. */
object QueryJson {
  private val format = ISODateTimeFormat.dateTimeNoMillis().withZoneUTC()

  /** ISO-8601 in UTC without milliseconds, e.g. `2026-11-06T12:34:56Z`. */
  def iso(t: DateTime): String = format.print(t.withZone(DateTimeZone.UTC))

  def envelope(now: DateTime, contests: Seq[JsObject]): JsObject =
    Json.obj("as_of" -> iso(now), "contests" -> contests)

  /** The envelope when no contest is selectable. */
  def noContest(now: DateTime): JsObject = envelope(now, Nil) + ("note" -> Json.toJson("no contest"))

  def error(text: String): JsObject = Json.obj("error" -> text)

  /** `{id, name, status, view}`: what every contest-bound endpoint says about each selected contest. */
  def contestBase(c: Contest, audience: Audience, now: DateTime): JsObject =
    Json.obj(
      "id" -> c.id,
      "name" -> c.name,
      "status" -> ContestSelection.status(c, now),
      "view" -> ContestSelection.viewOf(c, audience, now).json)

  /** Start/end/freeze times (freeze null when none is configured) and elapsed/remaining seconds, both clamped to
    * the contest's duration and never negative. */
  def contestTimes(c: Contest, now: DateTime): JsObject = {
    val start = c.startTime.getMillis
    val end = math.max(c.endTime.getMillis, start)
    val t = math.min(math.max(now.getMillis, start), end)
    val freeze: JsValue = if (c.freezeTime.isBefore(c.endTime)) Json.toJson(iso(c.freezeTime)) else JsNull
    Json.obj(
      "start" -> iso(c.startTime),
      "end" -> iso(c.endTime),
      "freeze" -> freeze,
      "elapsed_seconds" -> (t - start) / 1000,
      "remaining_seconds" -> (end - t) / 1000)
  }

  /** `{"teams": {regular, out_of_competition, total}}`: the contest's participants, out of competition being those
    * with the not-rated flag. */
  def teamCounts(t: TeamCounts): JsObject =
    Json.obj("teams" -> Json.obj("regular" -> t.regular, "out_of_competition" -> t.outOfCompetition, "total" -> t.total))

  /** Problems in the given id order; a problem without a known name is named by its id. */
  def problems(ids: Seq[String], names: Map[String, String]): JsObject =
    Json.obj("problems" -> ids.map { id =>
      val name: String = names.getOrElse(id, id)
      Json.obj("id" -> id, "name" -> name)
    })

  /** A standings row; `team` is the display name the monitor and palevo's announcements use. Rank 0 (not rated)
    * is shown as `*`, as on the monitor. */
  def standingsRow(r: live.LiveRow): JsObject =
    Json.obj(
      "rank" -> (if (r.rank == 0) "*" else r.rank.toString),
      "team" -> r.team.teamFullName,
      "team_id" -> r.team.id,
      "solved" -> r.score.solved,
      "penalty" -> r.score.penalty)
}
