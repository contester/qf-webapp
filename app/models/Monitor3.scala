package models

sealed trait TestingResultLike

sealed trait ContestTimestampLike

trait IncomingSubmitLike {
  def arrived: ContestTimestampLike
  def problemID: String
  def testingResult: Option[TestingResultLike]
}
