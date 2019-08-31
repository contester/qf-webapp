package models

import org.joda.time.Duration

import scala.collection.{SortedMap, mutable}

sealed trait TestingResultLike

sealed trait ContestTimestampLike

trait IncomingSubmitLike {
  def id: Long
  def arrived: ContestTimestampLike
  def teamID: Long
  def problemID: String
  def testingResult: Option[TestingResultLike]
}

case class Cell3(success: Option[Duration], failedAttempts: Int)

case class MonitorRow3(team: Long, rank: Option[Int], penalty: Long, cells: Map[String, Cell3])

class ProblemSubmits {
  val tsubmits = mutable.HashMap[Long, IncomingSubmitLike]()

  def update(s: IncomingSubmitLike) = {
    tsubmits.put(s.id, s)
  }
}

class TeamSubmits {
  val tsubmits = mutable.HashMap[String, ProblemSubmits]()

  def update(s: IncomingSubmitLike) = {
    tsubmits.getOrElseUpdate(s.problemID, new ProblemSubmits).update(s)
  }
}

class MutableMonitor(teams: Map[Long, LocalTeam]) {
  val tsubmits = mutable.HashMap[Long, TeamSubmits]()

  def update(s: IncomingSubmitLike) = {
    tsubmits.getOrElseUpdate(s.teamID, new TeamSubmits).update(s)
  }
}