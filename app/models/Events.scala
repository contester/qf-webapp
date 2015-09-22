package models

import actors.Message2
import play.api.libs.EventSource.{EventDataExtractor, EventNameExtractor}
import play.api.libs.json.{JsNumber, JsObject, Json}
import slick.jdbc.GetResult

case class ClarificationRequestState(contest: Int, pending: Int)

object ClarificationRequestState {
  implicit val getResult = GetResult(r =>
    ClarificationRequestState(r.nextInt(), r.nextInt())
  )

  implicit val format = Json.format[ClarificationRequestState]

  implicit val eventNameExtractor = EventNameExtractor[ClarificationRequestState](_ => Some("clarificationRequestState"))
  implicit val eventDataExtractor = EventDataExtractor[ClarificationRequestState] { clrState =>
    Json.stringify(Json.toJson(clrState))
  }
}