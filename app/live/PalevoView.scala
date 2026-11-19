package live

import palevo.{StandingsRow, StandingsView}

/** The live points board (the only board; there is no frozen one) as palevo sees it: a cell at its maximum is a solve. */
object PalevoView {
  def of(board: LiveBoard): StandingsView =
    StandingsView(board.rows.map { r =>
      val solves = r.cells.collect { case (p, LiveCell(_, _, true, _, Some(at))) => p -> at }
      StandingsRow(r.team, r.place.getOrElse(0), solves.size, solves, r.total)
    })
}
