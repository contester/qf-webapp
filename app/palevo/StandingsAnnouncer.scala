package palevo

import models.Visibility
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest

/**
  * Standings announcements from the one (real) points board. While the round runs only the jury hears them: the
  * public must learn nothing of the board before the end. Once the round is over the public tracker takes over; its
  * first observation only records the state, so the end itself announces nothing. Not thread-safe: call it from one
  * actor.
  *
  * TODO(task18): rework the public standings events for after the round (spec: public standings events only once the
  * round has ended) and the countdown edges.
  */
final class StandingsAnnouncer {
  private val publicTracker = new StandingsTracker
  private val juryTracker = new StandingsTracker(JuryAudience)

  def observe(contest: Contest, board: StandingsView, now: DateTime): Seq[PalevoEvent] = {
    val jury = juryTracker.observe(contest, board, now)
    if (Visibility.isOver(contest, now)) publicTracker.observe(contest, board, now) else jury
  }
}
