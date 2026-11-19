package live

import models.ProblemColumn
import org.stingray.contester.dbmodel.{Contest, Team}

import scala.collection.mutable

/** What one event did to the board, in order. */
case class Delta(board: Seq[BoardOut]) {
  def nonEmpty: Boolean = board.nonEmpty
}

object Delta {
  val empty: Delta = Delta(Nil)
}

/**
  * The live points board of one round, kept from its submit events. There is one board, the real one: admins see it
  * always, and nobody else gets a live board. Mutable, single-threaded: one actor owns each instance.
  */
class LiveContest(initial: Contest, val problems: Seq[ProblemColumn], teams: Seq[Team], subs: Seq[SubmitRec]) {
  private[this] var current: Contest = initial

  private[this] var teamById: Map[Int, Team] = teams.map(t => t.id -> t).toMap
  private[this] val maxPoints: Map[String, Double] = problems.map(p => p.id -> p.maxPoints).toMap

  private[this] val byId = mutable.Map.empty[Int, SubmitRec]
  private[this] val byCell = mutable.Map.empty[(Int, String), Vector[SubmitRec]]

  // Every submit of a known problem is kept, also those of a team that is not listed (an inactive credential), so that
  // the team's work is back when it is listed again.
  subs.foreach(r => if (maxPoints.contains(r.problem)) insert(r))

  private[this] var liveBoard: LiveBoard = buildBoard()

  def board: LiveBoard = liveBoard
  def contest: Contest = current
  def teamList: Seq[Team] = teamById.values.toSeq.sortBy(_.id)
  def records: Seq[SubmitRec] = byId.values.toSeq.sortBy(_.id)

  /** Whether an arrival can be on this board: a listed team and a known problem. */
  def accepts(rec: SubmitRec): Boolean = teamById.contains(rec.team) && maxPoints.contains(rec.problem)

  // A repeated arrival (e.g. a late message after the row was loaded for an early finish) is ignored:
  // the stored record and its state win.
  def onArrived(rec: SubmitRec): Delta =
    if (!accepts(rec) || byId.contains(rec.id)) Delta.empty
    else {
      insert(rec)
      applyCell(rec.team, rec.problem)
    }

  /** A testing finished: the submit's points are now `points` (they replace the old ones, never add). */
  def onFinished(id: Int, compiled: Boolean, points: Option[Double]): Option[Delta] =
    setState(id, Done(compiled, points))

  /** A rejudge was asked for: the submit is in flight again and does not count for the best until it finishes. */
  def onRejudge(id: Int): Option[Delta] =
    setState(id, Pending)

  /** A team's details changed (name, school, not rated, disqualified), or a team new to the round. */
  def onTeamChanged(team: Team): Delta = {
    teamById += team.id -> team
    val (nb, out) = liveBoard.update(team, teamCells(team.id))
    liveBoard = nb
    Delta(out.toSeq)
  }

  /** A team left the round (its credential was deactivated or deleted). Its submits are kept. */
  def onTeamRemoved(teamId: Int): Delta = {
    teamById -= teamId
    val (nb, out) = liveBoard.remove(teamId)
    liveBoard = nb
    Delta(out.toSeq)
  }

  /** The round's whole team list as now stored: each changed, new or vanished team is applied in turn. */
  def onTeams(teams: Seq[Team]): Delta = {
    val now = teams.map(t => t.id -> t).toMap
    val removed = teamById.keys.filterNot(now.contains).toSeq.sorted.map(onTeamRemoved)
    val changed = teams.sortBy(_.id).filterNot(t => teamById.get(t.id).contains(t)).map(onTeamChanged)
    Delta((removed ++ changed).flatMap(_.board))
  }

  /** The round's row changed (name, times): the board does not depend on it. */
  def onContest(c: Contest): Delta = {
    current = c
    Delta.empty
  }

  def rebuilt(problems: Seq[ProblemColumn], teams: Seq[Team], subs: Seq[SubmitRec]): LiveContest =
    new LiveContest(current, problems, teams, subs)

  def differences(other: LiveContest): Seq[String] = LiveContest.boardDifferences(board, other.board)

  private[this] def insert(r: SubmitRec): Unit = {
    byId(r.id) = r
    val key = (r.team, r.problem)
    byCell(key) = (byCell.getOrElse(key, Vector.empty).filterNot(_.id == r.id) :+ r)
      .sortBy(x => (x.arrivedAt.getMillis, x.id))
  }

  private[this] def setState(id: Int, state: SubmitState): Option[Delta] =
    byId.get(id).map { old =>
      if (old.state == state) Delta.empty
      else {
        insert(old.copy(state = state))
        applyCell(old.team, old.problem)
      }
    }

  private[this] def cellOf(teamId: Int, problem: String): LiveCell =
    LiveCells.cellOf(byCell.getOrElse((teamId, problem), Vector.empty), maxPoints.getOrElse(problem, 0))

  private[this] def teamCells(teamId: Int): Map[String, LiveCell] =
    problems.flatMap { p =>
      val c = cellOf(teamId, p.id)
      if (c.isEmpty) None else Some(p.id -> c)
    }.toMap

  private[this] def buildBoard(): LiveBoard =
    LiveBoard.build(problems, teamById.values.toSeq, teamById.keys.map(id => id -> teamCells(id)).toMap)

  // Recomputes the one touched cell and re-ranks.
  private[this] def applyCell(teamId: Int, problem: String): Delta =
    teamById.get(teamId) match {
      case None => Delta.empty // a team that is not listed: its submit is kept, it is on no board
      case Some(team) =>
        val old = liveBoard.rowOf(teamId).map(_.cells).getOrElse(Map.empty[String, LiveCell])
        val c = cellOf(teamId, problem)
        val (nb, out) = liveBoard.update(team, if (c.isEmpty) old - problem else old + (problem -> c))
        liveBoard = nb
        Delta(out.toSeq)
    }
}

object LiveContest {
  private[live] def boardDifferences(a: LiveBoard, b: LiveBoard): Seq[String] = {
    val header = if (a.problems != b.problems) Seq(s"problems ${a.problems} vs ${b.problems}") else Nil
    val rows = (0 until a.rows.size.max(b.rows.size)).flatMap { i =>
      (a.rows.lift(i), b.rows.lift(i)) match {
        case (Some(x), Some(y)) if x == y => None
        case (Some(x), Some(y)) if x.team.id != y.team.id =>
          Some(s"position $i has team ${x.team.id} vs team ${y.team.id}")
        case (Some(x), Some(y)) =>
          Some(s"team ${x.team.id} at position $i: place ${x.place}/${y.place}, total ${x.total}/${y.total}, " +
            s"cells ${x.cells}/${y.cells}, team ${x.team}/${y.team}")
        case (Some(x), None) => Some(s"team ${x.team.id} only on the first board")
        case (None, Some(y)) => Some(s"team ${y.team.id} only on the second board")
        case _ => None
      }
    }
    header ++ rows
  }
}
