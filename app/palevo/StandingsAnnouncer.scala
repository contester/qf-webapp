package palevo

import models.Visibility
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest

import scala.collection.mutable

/**
  * Standings announcements from the one (live) points board; there is no frozen board.
  *
  * While a round runs only the jury hears them: first_solve_contest / first_solve_problem / new_leader with the jury
  * audience (StandingsTracker). The public learns nothing of the board before the end.
  *
  * Once the round is over (Visibility.isOver: is_finished, or its end has come) the jury tracker is no longer fed and
  * the public hears the final standings (FinalStandings: the top places) exactly once, and again only when a later
  * change (a rejudge, a credential change, a submit judged after the end) alters them.
  *
  * Restart rule: a contest whose first observation finds it already over only records its final standings, so a
  * restart after the end announces nothing; a contest first seen running is announced at its end.
  * Not thread-safe: call it from one actor.
  */
final class StandingsAnnouncer {
  private val juryTracker = new StandingsTracker(JuryAudience)
  private val seen = mutable.Set[Int]()
  // The final standings last announced (or recorded) per contest.
  private val finals = mutable.Map[Int, Seq[FinalRow]]()

  def observe(contest: Contest, board: StandingsView, now: DateTime): Seq[PalevoEvent] = {
    val firstSight = seen.add(contest.id)
    if (!Visibility.isOver(contest, now)) juryTracker.observe(contest, board, now)
    else {
      val rows = StandingsAnnouncer.finalRows(board)
      val before = finals.put(contest.id, rows)
      if (firstSight || before.contains(rows)) Nil
      else Seq(FinalStandings(EventBuilders.contestOf(contest), now, rows, updated = before.isDefined))
    }
  }
}

object StandingsAnnouncer {
  /** How many places the final standings name (ties included: every team on one of these places). */
  val FinalPlaces = 3

  /** The rated teams with points on the first FinalPlaces places, in rank order (empty when nobody scored). */
  def finalRows(board: StandingsView): Seq[FinalRow] =
    board.rows.filter(r => !r.team.notRated && r.rank >= 1 && r.rank <= FinalPlaces && r.points > 0)
      .sortBy(_.rank)
      .map(r => FinalRow(r.rank, EventBuilders.teamOf(r.team), r.points, r.solved))
}
