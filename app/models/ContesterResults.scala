package models

import play.api.libs.json.Json

object ContesterResults {
  case class SubmitObject(id: Int, team: Int, contest: Int, problem: String,
                          schoolMode: Boolean)

  object SubmitObject {
    implicit val submitObjectFormat = Json.format[SubmitObject]
  }

  case class FinishedTesting(submit: SubmitObject, testingId: Int, compiled: Boolean, passed: Int, taken: Int)

  object FinishedTesting {
    implicit val finishedTestingFormat = Json.format[FinishedTesting]
  }

  case class CustomTestResult(id: Int, contest:Int, team: Int)

  object CustomTestResult {
    implicit val formatCustomTestResult = Json.format[CustomTestResult]
  }
}