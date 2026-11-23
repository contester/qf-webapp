package models

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Contest

/** Who sees the scoreboard when: admins always; participants see their own round (teams only before start); public never in qf-webapp (offline results are in teacher-web). */
class BoardVisibilitySpec extends AnyFunSuite {
  private val start = new DateTime(2030, 1, 1, 10, 0)
  private val end = start.plusHours(3)
  private def round(finished: Boolean = false) = Contest(5, "R", start, end, None, 1, isFinished = finished)

  test("an admin always sees the board: before the start, while running, after the end, when finished") {
    for (now <- Seq(start.minusHours(1), start, end.minusMillis(1), end, end.plusDays(1)); f <- Seq(false, true))
      assert(Visibility.canSeeBoard(round(f), AdminViewer, now))
  }

  test("participants always see their own round's board and never another round's; anonymous never sees qf-webapp boards") {
    for (now <- Seq(start.minusHours(1), start, start.plusHours(1), end.minusMillis(1), end, end.plusDays(2)); f <- Seq(false, true)) {
      assert(Visibility.canSeeBoard(round(f), TeamViewer(11, 5), now), s"own round at $now finished=$f")
      assert(!Visibility.canSeeBoard(round(f), TeamViewer(11, 6), now), s"other round at $now finished=$f")
      assert(!Visibility.canSeeBoard(round(f), Anonymous, now), s"anonymous at $now finished=$f")
    }
  }

  test("isStarted is true from starts_at inclusive or when finished early") {
    assert(!Visibility.isStarted(round(), start.minusMillis(1)))
    assert(Visibility.isStarted(round(), start))
    assert(Visibility.isStarted(round(), start.plusHours(1)))
    assert(Visibility.isStarted(round(finished = true), start.minusHours(1)))
  }
}
