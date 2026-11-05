package live

import palevo.{StandingsRow, StandingsView}

object PalevoView {
  def of(board: LiveBoard): StandingsView =
    StandingsView(board.rows.map { r =>
      StandingsRow(r.team, r.rank, r.score.solved,
        r.cells.collect { case (p, LiveCell.Solved(_, seconds)) => p -> seconds })
    })
}
