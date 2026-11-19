package palevo

import org.stingray.contester.dbmodel.Team

/** What the standings tracker needs to know about a scoreboard, independent of where it comes from. */
trait StandingsView {
  def rows: Seq[StandingsRow]
}

/**
  * rank: the place, 0 for a team without one; solves: problem -> seconds from the start of every cell at its maximum;
  * points: the team's total.
  */
case class StandingsRow(team: Team, rank: Int, solved: Int, solves: Map[String, Int], points: Double = 0)

object StandingsView {
  val empty: StandingsView = new StandingsView { val rows: Seq[StandingsRow] = Nil }

  def apply(rs: Seq[StandingsRow]): StandingsView = new StandingsView { val rows: Seq[StandingsRow] = rs }
}
