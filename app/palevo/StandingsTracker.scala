package palevo

import models.{ACM, AnyStatus}
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest

import scala.collection.mutable

/**
  * Derives first_solve_contest / first_solve_problem / new_leader events from successive scoreboards.
  *
  * Feed it only the pre-freeze status (StoredContestStatus.frozen): that status cannot change after the
  * freeze, which is what keeps scoreboard information from leaking. The first observation of a contest only
  * records its state, so a restart mid-contest announces nothing. Teams that are not rated are ignored.
  * Not thread-safe: call it from one actor.
  */
final class StandingsTracker(audience: Audience = PublicAudience) {
  import StandingsTracker._

  private val states = mutable.Map[Int, State]()

  def observe(contest: Contest, status: AnyStatus, now: DateTime): Seq[PalevoEvent] = status match {
    case acm: ACM.Status => observeAcm(contest, acm, now)
    case _ => Nil
  }

  private def observeAcm(contest: Contest, status: ACM.Status, now: DateTime): Seq[PalevoEvent] = {
    val rated = status.rows.filter(!_.team.notRated)
    val solves = for {
      row <- rated
      (problem, cell) <- row.cells.toSeq
      if cell.fullSolution
    } yield Solve(PTeam(row.team.id, row.team.teamFullName), problem, cell.arrivedSeconds)

    val firstByProblem: Map[String, Solve] =
      solves.groupBy(_.problem).map { case (p, ss) => p -> ss.minBy(s => (s.seconds, s.team.id)) }

    val leaders = rated.filter(r => r.rank == 1 && r.score.solved >= 1)
    val currentLeader: Option[(PTeam, Int)] = leaders match {
      case Seq(only) => Some((PTeam(only.team.id, only.team.teamFullName), only.score.solved))
      case _ => None
    }

    states.get(contest.id) match {
      case None =>
        states.put(contest.id, new State(firstByProblem.keySet, solves.nonEmpty, currentLeader.map(_._1.id)))
        Nil
      case Some(state) =>
        val pc = PContest(contest.id, contest.name)
        val hadSolves = state.contestFirst

        val contestFirst =
          if (!state.contestFirst && solves.nonEmpty) {
            val s = solves.minBy(x => (x.seconds, x.team.id))
            Seq(FirstSolveContest(pc, now, s.team, s.problem, s.seconds, audience))
          } else Nil

        val problemFirsts = (firstByProblem -- state.problems).values.toSeq
          .sortBy(s => (s.seconds, s.problem))
          .map(s => FirstSolveProblem(pc, now, s.team, s.problem, s.seconds, audience))

        val leaderChange = currentLeader match {
          case Some((t, solved)) if hadSolves && !state.leader.contains(t.id) => Seq(NewLeader(pc, now, t, solved, audience))
          case _ => Nil
        }

        state.problems = state.problems ++ firstByProblem.keySet
        state.contestFirst = state.contestFirst || solves.nonEmpty
        currentLeader.foreach(l => state.leader = Some(l._1.id))

        contestFirst ++ problemFirsts ++ leaderChange
    }
  }
}

object StandingsTracker {
  /**
    * True while the public scoreboard is frozen and the real one is not yet exposed. Contests without a freeze
    * (freezeTime >= endTime) have no such window.
    */
  def liveWindow(c: Contest, now: DateTime): Boolean =
    c.freezeTime.isBefore(c.endTime) && !now.isBefore(c.freezeTime) && now.isBefore(c.exposeTime)

  private final class State(var problems: Set[String], var contestFirst: Boolean, var leader: Option[Int])

  private final case class Solve(team: PTeam, problem: String, seconds: Int)
}
