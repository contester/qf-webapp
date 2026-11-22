package models

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Contest

/** Who sees the scoreboard when: admins always; teams and the public never in qf-webapp (offline results are in teacher-web). */
class BoardVisibilitySpec extends AnyFunSuite {
  private val start = new DateTime(2030, 1, 1, 10, 0)
  private val end = start.plusHours(3)
  private def round(finished: Boolean = false) = Contest(5, "R", start, end, None, 1, isFinished = finished)

  private val nonAdmins: Seq[Viewer] = Seq(TeamViewer(11, 5), Anonymous)

  test("an admin always sees the board: before the start, while running, after the end, when finished") {
    for (now <- Seq(start.minusHours(1), start, end.minusMillis(1), end, end.plusDays(1)); f <- Seq(false, true))
      assert(Visibility.canSeeBoard(round(f), AdminViewer, now))
  }

  test("teams and the public never see qf-webapp boards even after ends_at or when finished") {
    for (v <- nonAdmins; now <- Seq(start.minusHours(1), start, start.plusHours(1), end.minusMillis(1), end, end.plusDays(2)); f <- Seq(false, true))
      assert(!Visibility.canSeeBoard(round(f), v, now), s"$v at $now finished=$f")
  }
}
