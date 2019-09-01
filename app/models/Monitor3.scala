package models

import models.Monitor3.CellScore
import org.joda.time.Duration

import scala.collection.{SortedMap, mutable}

case class Cell3(success: Option[Duration], failedAttempts: Int)

case class MonitorRow3(team: Long, rank: Option[Int], penalty: Long, cells: Map[String, Cell3])

case class ProblemSubmits(data: Map[Int, Submit]) {
  def update(s: Submit): ProblemSubmits =
    copy(data = data.updated(s.submitId.id, s))
}

object Monitor3 {
  type SubmitScore = Option[Long]

  case class CellScore(success: SubmitScore, failedAttempts: Int)
}

case class ProblemSubmitsScored(data: Map[Int, Submit], score: Monitor3.CellScore)

object ProblemSubmits {
  val empty = ProblemSubmits(Map.empty)

  def score(data: Map[Int, Submit], submitToScore: Option[Int]): (Monitor3.CellScore, Monitor3.SubmitScore) = {
    val (failures, successes) = data.values.toSeq.sortBy(_.submitId.arrived).span(!_.success)
    val success = successes.headOption
    val successScore: Monitor3.SubmitScore = if(success.isEmpty) { None } else Some(failures.length * 20)
    val ss = submitToScore.flatMap { sid =>
      if (success.exists(_.submitId.id == sid)) {
        successScore
      } else None
    }
    (CellScore(successScore, failures.length), ss)
  }

  def update(prev: ProblemSubmitsScored, submit: Submit): (ProblemSubmitsScored, Monitor3.SubmitScore) = {
    val next = prev.data.updated(submit.submitId.id, submit)
    val (nextCell, submitScore) = score(next, Some(submit.submitId.id))
    (ProblemSubmitsScored(next, nextCell), submitScore)
  }
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

object ImmutableMonitor {
  def fromSubmits(submits: Iterable[Submit]): Map[Int, ImmutableMonitor] = {
    submits.groupBy(_.submitId.contestId).mapValues { forContest =>
      ImmutableMonitor(forContest.groupBy(_.submitId.teamId).mapValues { forTeam =>
        TeamSubmits(forTeam.groupBy(_.submitId.problem.id).mapValues { forProblem =>
          ProblemSubmits(forProblem.map(x => x.submitId.id -> x).toMap)
        })
      })
    }
  }
}