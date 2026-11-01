package palevo

import org.joda.time.DateTime
import org.stingray.contester.dbmodel.Contest

import scala.collection.mutable

/**
  * Turns the passage of time into contest_started / contest_frozen / contest_finished events. The first
  * observation of a contest only records its phase, so restarting the application mid-contest does not
  * re-announce anything. Not thread-safe: call it from one actor.
  */
final class ContestPhaseTracker {
  private val Started = 1
  private val Frozen = 2
  private val Finished = 3

  private val seen = mutable.Map[Int, Int]()

  private def freezeEnabled(c: Contest) = c.freezeTime.isBefore(c.endTime)

  private def phaseAt(c: Contest, now: DateTime): Int =
    if (!now.isBefore(c.endTime)) Finished
    else if (freezeEnabled(c) && !now.isBefore(c.freezeTime)) Frozen
    else if (!now.isBefore(c.startTime)) Started
    else 0

  def observe(c: Contest, now: DateTime): Seq[PalevoEvent] = {
    val phase = phaseAt(c, now)
    val pc = PContest(c.id, c.name)
    seen.get(c.id) match {
      case Some(prev) if phase > prev =>
        seen.put(c.id, phase)
        ((prev + 1) to phase).flatMap {
          case Started => Some(ContestStarted(pc, now))
          case Frozen if freezeEnabled(c) => Some(ContestFrozen(pc, now))
          case Finished => Some(ContestFinished(pc, now))
          case _ => None
        }
      case Some(_) => Nil
      case None =>
        seen.put(c.id, phase)
        Nil
    }
  }
}
