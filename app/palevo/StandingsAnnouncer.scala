package palevo

import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest

/**
  * Standings announcements for both audiences. The public tracker watches the pre-freeze scoreboard, which cannot
  * change after the freeze, so nothing leaks. The jury tracker watches the real scoreboard and is heard only while
  * the public one is frozen; it is fed at all times so its state is current when the freeze starts. A fact that
  * both trackers see in the same refresh (a submit that arrived before the freeze and was judged after it) is
  * announced once, publicly. Not thread-safe: call it from one actor.
  */
final class StandingsAnnouncer {
  private val publicTracker = new StandingsTracker
  private val juryTracker = new StandingsTracker(JuryAudience)

  def observe(contest: Contest, frozen: StandingsView, exposed: StandingsView, now: DateTime): Seq[PalevoEvent] = {
    val publicEvents = publicTracker.observe(contest, frozen, now)
    val juryEvents = juryTracker.observe(contest, exposed, now)
    val live =
      if (StandingsTracker.liveWindow(contest, now))
        juryEvents.filterNot(j => publicEvents.exists(p => p.eventType == j.eventType && p.data == j.data))
      else Nil
    publicEvents ++ live
  }
}
