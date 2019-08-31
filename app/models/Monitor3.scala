package models

import org.joda.time.Duration

import scala.collection.{SortedMap, mutable}

case class Cell3(success: Option[Duration], failedAttempts: Int)

case class MonitorRow3(team: Long, rank: Option[Int], penalty: Long, cells: Map[String, Cell3])

case class ProblemSubmits(data: Map[Int, Submit]) {
  def update(s: Submit): ProblemSubmits =
    copy(data = data.updated(s.submitId.id, s))
}

object ProblemSubmits {
  val empty = ProblemSubmits(Map.empty)
}

case class TeamSubmits(data: Map[String, ProblemSubmits]) {
  def update(s: Submit): TeamSubmits =
    copy(data = data.updated(s.submitId.problem.id, data.getOrElse(s.submitId.problem.id, ProblemSubmits.empty).update(s)))
}

object TeamSubmits {
  val empty = TeamSubmits(Map.empty)
}

case class ImmutableMonitor(data: Map[Int, TeamSubmits]) {
  def update(s: Submit): ImmutableMonitor =
    copy(data = data.updated(s.submitId.teamId, data.getOrElse(s.submitId.teamId, TeamSubmits.empty).update(s)))
}