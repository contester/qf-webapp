package models

import org.joda.time.DateTime
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Contest

/** Who sees the scoreboard when: admins always; teams and the public only once the round is over. */
class BoardVisibilitySpec extends AnyFunSuite {
  private val start = new DateTime(2030, 1, 1, 10, 0)
  private val end = start.plusHours(3)
  private def round(finished: Boolean = false) = Contest(5, "R", start, end, None, 1, isFinished = finished)

  private val everyone: Seq[Viewer] = Seq(AdminViewer, TeamViewer(11, 5), Anonymous)
  private val nonAdmins: Seq[Viewer] = Seq(TeamViewer(11, 5), Anonymous)

  test("an admin always sees the board: before the start, while running, after the end, when finished") {
    for (now <- Seq(start.minusHours(1), start, end.minusMillis(1), end, end.plusDays(1)); f <- Seq(false, true))
      assert(Visibility.canSeeBoard(round(f), AdminViewer, now))
  }

  test("teams and the public do not see it before the start or while the round runs") {
    for (v <- nonAdmins; now <- Seq(start.minusHours(1), start, start.plusHours(1), end.minusMillis(1)))
      assert(!Visibility.canSeeBoard(round(), v, now), s"$v at $now")
  }

  test("teams and the public see it from ends_at inclusive and after") {
    for (v <- nonAdmins; now <- Seq(end, end.plusMillis(1), end.plusDays(2)))
      assert(Visibility.canSeeBoard(round(), v, now), s"$v at $now")
  }

  test("is_finished makes it visible to everyone even before ends_at") {
    for (v <- everyone; now <- Seq(start.plusMinutes(5), end.minusMillis(1)))
      assert(Visibility.canSeeBoard(round(finished = true), v, now), s"$v at $now")
  }

  test("a team of another round is treated like anyone else") {
    assert(!Visibility.canSeeBoard(round(), TeamViewer(11, 6), start.plusHours(1)))
    assert(Visibility.canSeeBoard(round(), TeamViewer(11, 6), end))
  }
}
