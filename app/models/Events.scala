package models

import play.api.libs.EventSource.{EventDataExtractor, EventNameExtractor}
import play.api.libs.json.Json

case class ClarificationRequestState(contest: Long, pending: Int, newRequest: Boolean)

object ClarificationRequestState {
  implicit val format = Json.format[ClarificationRequestState]

  implicit val eventNameExtractor = EventNameExtractor[ClarificationRequestState](_ => Some("clarificationRequestState"))
  implicit val eventDataExtractor = EventDataExtractor[ClarificationRequestState] { clrState =>
    Json.stringify(Json.toJson(clrState))
  }
}