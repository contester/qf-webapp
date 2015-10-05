package models

import com.github.nscala_time.time.Imports._
import org.joda.time.DateTime
import slick.jdbc.GetResult

case class Clarification(id: Int, contest: Int, problem: String, text: String, arrived: DateTime, hidden: Boolean)

object Clarification {
  implicit val getResult = GetResult(
    r => Clarification(r.nextInt(), r.nextInt(), r.nextString().toUpperCase, r.nextString(), new DateTime(r.nextTimestamp()), r.nextBoolean()))
}

case class ClarificationRequest(id: Int, contest: Int, team: Int, problem: String, request: String,
                                answer: String, arrived: DateTime, answered: Boolean)

object ClarificationRequest {
  implicit val getResult = GetResult(
    r => ClarificationRequest(r.nextInt(), r.nextInt(), r.nextInt(),
      r.nextString().toUpperCase, r.nextString(), r.nextString(), new DateTime(r.nextTimestamp()), r.nextBoolean())
  )
}

