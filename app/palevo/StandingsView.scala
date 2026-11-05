package palevo

import models.{ACM, AnyStatus}
import org.stingray.contester.dbmodel.Team

/** What the standings tracker needs to know about a scoreboard, independent of where it comes from. */
trait StandingsView {
  def rows: Seq[StandingsRow]
}

/** solves: problem -> seconds from the contest start of every solved cell. */
case class StandingsRow(team: Team, rank: Int, solved: Int, solves: Map[String, Int])

object StandingsView {
  val empty: StandingsView = new StandingsView { val rows: Seq[StandingsRow] = Nil }

  def apply(rs: Seq[StandingsRow]): StandingsView = new StandingsView { val rows: Seq[StandingsRow] = rs }

  /** Only ACM statuses carry standings; any other status gives an empty view. */
  def of(status: AnyStatus): StandingsView = status match {
    case acm: ACM.Status =>
      apply(acm.rows.map { r =>
        StandingsRow(r.team, r.rank, r.score.solved,
          r.cells.collect { case (p, c) if c.fullSolution => p -> c.arrivedSeconds })
      })
    case _ => empty
  }
}
