package models

import play.api.libs.json.Json

object ContesterResults {
  case class SubmitObject(id: Int, team: Int, contest: Int, problem: String)

  object SubmitObject {
    implicit val submitObjectFormat = Json.format[SubmitObject]
  }

  /** A finished testing of a submit; `points` are the submit's points now (None: not set). */
  case class FinishedTesting(submit: SubmitObject, testingId: Int, compiled: Boolean, passed: Int, taken: Int,
                             points: Option[Double] = None)

  object FinishedTesting {
    implicit val finishedTestingFormat = Json.format[FinishedTesting]
  }

  case class CustomTestResult(id: Int, contest:Int, team: Int)

  object CustomTestResult {
    implicit val formatCustomTestResult = Json.format[CustomTestResult]
  }
}