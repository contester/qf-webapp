package live

import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{Contest, Team}

import scala.collection.mutable

sealed trait BoardOut
case class RowOut(change: RowChange) extends BoardOut
case class WholeOut(board: LiveBoard) extends BoardOut

// `public` is the change of the public board: the freezing board without the contest jury teams.
case class Delta(freezing: Seq[BoardOut], real: Seq[BoardOut], ownCells: Seq[(Int, Map[String, LiveCell])],
                 public: Seq[BoardOut] = Nil)

object Delta {
  val empty: Delta = Delta(Nil, Nil, Nil)
}

// Mutable, single-threaded: one actor owns each instance.
class LiveContest(initial: Contest, problems: Seq[String], teams: Seq[Team], subs: Seq[SubmitRec], now: DateTime) {
  private[this] var current: Contest = initial
  private[this] var currentRules: FreezeRules = FreezeRules.of(initial)
  private[this] var lastNow: DateTime = now

  private[this] var teamList: Vector[Team] = teams.toVector
  private[this] var teamById: Map[Int, Team] = teams.map(t => t.id -> t).toMap
  private[this] val problemSet: Set[String] = problems.toSet

  private[this] val byId = mutable.Map.empty[Int, SubmitRec]
  private[this] val byCell = mutable.Map.empty[(Int, String), Vector[SubmitRec]]

  subs.foreach(r => if (accepts(r)) insert(r))

  private[this] var freezingBoard: LiveBoard = buildBoard(Freezing, now)
  private[this] var realBoard: LiveBoard = buildBoard(Real, now)
  private[this] var pubBoard: LiveBoard = buildPublic(now)

  /** What the public sees: the freezing board without the contest jury teams (coach teams stay). */
  def publicBoard: LiveBoard = pubBoard

  def board(kind: BoardKind): LiveBoard = kind match {
    case Freezing => freezingBoard
    case Real => realBoard
  }

  def contest: Contest = current
  def rules: FreezeRules = currentRules
  def records: Seq[SubmitRec] = byId.values.toSeq.sortBy(_.id)

  // A repeated arrival (e.g. a late message after the row was loaded for an early finish) is ignored:
  // the stored record and its state win.
  def onArrived(rec: SubmitRec, now: DateTime): Delta =
    if (!accepts(rec) || byId.contains(rec.id)) Delta.empty
    else {
      insert(rec)
      applyCells(Seq(rec.team -> rec.problem), now)
    }

  def onFinished(id: Int, compiled: Boolean, passed: Int, taken: Int, now: DateTime): Option[Delta] =
    setState(id, Done(compiled, passed, taken), now)

  def onRejudge(id: Int, now: DateTime): Option[Delta] =
    setState(id, Pending, now)

  def onTick(now: DateTime): Option[Delta] =
    if (crossed(now)) Some(rebuildAll(now)) else {
      lastNow = now
      None
    }

  /**
    * A team's details changed (not rated, name, school, number): replace it, or add a team new to the contest, and
    * rebuild both boards, since the flag and the name change the order and the ranks.
    */
  def onTeamChanged(team: Team, now: DateTime): Delta = {
    val i = teamList.indexWhere(_.id == team.id)
    teamList = if (i >= 0) teamList.updated(i, team) else teamList :+ team
    teamById += team.id -> team
    rebuildAll(now)
  }

  def onContest(c: Contest, now: DateTime): Delta = {
    current = c
    currentRules = FreezeRules.of(c)
    rebuildAll(now)
  }

  def rebuilt(problems: Seq[String], teams: Seq[Team], subs: Seq[SubmitRec], now: DateTime): LiveContest =
    new LiveContest(current, problems, teams, subs, now)

  def differences(other: LiveContest): Seq[String] =
    Seq(Freezing, Real).flatMap(kind => LiveContest.boardDifferences(kind, board(kind), other.board(kind))) ++
      LiveContest.boardDifferences(Freezing, publicBoard, other.publicBoard).map("public " + _)

  private[this] def accepts(r: SubmitRec): Boolean = teamById.contains(r.team) && problemSet(r.problem)

  private[this] def insert(r: SubmitRec): Unit = {
    byId(r.id) = r
    val key = (r.team, r.problem)
    byCell(key) = (byCell.getOrElse(key, Vector.empty).filterNot(_.id == r.id) :+ r)
      .sortBy(x => (x.arrivedAt.getMillis, x.id))
  }

  private[this] def setState(id: Int, state: SubmitState, now: DateTime): Option[Delta] =
    byId.get(id).map { old =>
      if (old.state == state && !crossed(now)) {
        lastNow = now
        Delta.empty
      } else {
        insert(old.copy(state = state))
        applyCells(Seq(old.team -> old.problem), now)
      }
    }

  private[this] def cellOf(teamId: Int, problem: String, kind: BoardKind, now: DateTime): LiveCell =
    LiveCells.cellOf(byCell.getOrElse((teamId, problem), Vector.empty), kind, currentRules, now)

  private[this] def teamCells(teamId: Int, kind: BoardKind, now: DateTime): Map[String, LiveCell] =
    problems.flatMap { p =>
      val c = cellOf(teamId, p, kind, now)
      if (c == LiveCell.Empty) None else Some(p -> c)
    }.toMap

  private[this] def buildBoard(kind: BoardKind, now: DateTime): LiveBoard =
    LiveBoard.build(problems, teamList, teamList.map(t => t.id -> teamCells(t.id, kind, now)).toMap)

  private[this] def buildPublic(now: DateTime): LiveBoard = {
    val shown = teamList.filterNot(LiveContest.isJury)
    LiveBoard.build(problems, shown, shown.map(t => t.id -> teamCells(t.id, Freezing, now)).toMap)
  }

  // True when now and the last seen instant lie on different sides of start, freeze or expose.
  // The live monitor wakes at these instants (ContestTimeTracker.nextDeadline lists them): keep the two in step.
  private[this] def crossed(now: DateTime): Boolean =
    Seq(current.startTime, current.freezeTime, current.exposeTime).exists(t => lastNow.isBefore(t) != now.isBefore(t))

  private[this] def rebuildAll(now: DateTime): Delta = {
    lastNow = now
    freezingBoard = buildBoard(Freezing, now)
    realBoard = buildBoard(Real, now)
    pubBoard = buildPublic(now)
    Delta(Seq(WholeOut(freezingBoard)), Seq(WholeOut(realBoard)), Nil, Seq(WholeOut(pubBoard)))
  }

  private[this] def freezeActive(now: DateTime): Boolean =
    currentRules.freezeTime.isBefore(currentRules.endTime) && !now.isBefore(currentRules.freezeTime) &&
      now.isBefore(currentRules.exposeTime)

  // Recomputes only the touched (team, problem) cells; a crossed time boundary rebuilds both boards instead.
  private[this] def applyCells(touched: Seq[(Int, String)], now: DateTime): Delta =
    if (crossed(now)) rebuildAll(now)
    else {
      lastNow = now
      val teamIds = touched.map(_._1).distinct
      def step(kind: BoardKind, start: LiveBoard, ids: Seq[Int] = teamIds): (LiveBoard, Seq[BoardOut]) =
        ids.foldLeft((start, Seq.empty[BoardOut])) { case ((b, outs), tid) =>
          val old = b.rowOf(tid).map(_.cells).getOrElse(Map.empty[String, LiveCell])
          val cells = touched.filter(_._1 == tid).map(_._2).distinct.foldLeft(old) { (m, p) =>
            val c = cellOf(tid, p, kind, now)
            if (c == LiveCell.Empty) m - p else m + (p -> c)
          }
          val (nb, ch) = b.update(teamById(tid), cells)
          (nb, outs ++ ch.map(RowOut))
        }
      val (nf, fo) = step(Freezing, freezingBoard)
      val (nr, ro) = step(Real, realBoard)
      val (np, po) = step(Freezing, pubBoard, teamIds.filterNot(tid => LiveContest.isJury(teamById(tid))))
      freezingBoard = nf
      realBoard = nr
      pubBoard = np
      val own =
        if (!freezeActive(now)) Nil
        else teamIds.flatMap { tid =>
          val real = nr.rowOf(tid).map(_.cells).getOrElse(Map.empty[String, LiveCell])
          val frozen = nf.rowOf(tid).map(_.cells).getOrElse(Map.empty[String, LiveCell])
          val realChanged = ro.exists { case RowOut(c) => c.team == tid; case _ => false }
          if (realChanged || real != frozen) Some(tid -> real) else None
        }
      Delta(fo, ro, own, po)
    }
}

object LiveContest {
  /** The contest jury teams (see InitialImportTools.addServiceTeams); coach teams are not. */
  def isJury(t: Team): Boolean = t.schoolFullName == models.InitialImportTools.JurySchoolFullName

  private[live] def boardDifferences(kind: BoardKind, a: LiveBoard, b: LiveBoard): Seq[String] = {
    val header =
      (if (a.problems != b.problems) Seq(s"$kind: problems ${a.problems} vs ${b.problems}") else Nil) ++
        (if (a.solvedCounts != b.solvedCounts) Seq(s"$kind: solved counts ${a.solvedCounts} vs ${b.solvedCounts}") else Nil)
    val rows = (0 until a.rows.size.max(b.rows.size)).flatMap { i =>
      (a.rows.lift(i), b.rows.lift(i)) match {
        case (Some(x), Some(y)) if x == y => None
        case (Some(x), Some(y)) if x.team.id != y.team.id =>
          Some(s"$kind: position $i has team ${x.team.id} vs team ${y.team.id}")
        case (Some(x), Some(y)) =>
          Some(s"$kind: team ${x.team.id} at position $i: rank ${x.rank}/${y.rank}, score ${x.score}/${y.score}, " +
            s"cells ${x.cells}/${y.cells}, team ${x.team}/${y.team}")
        case (Some(x), None) => Some(s"$kind: team ${x.team.id} only on the first board")
        case (None, Some(y)) => Some(s"$kind: team ${y.team.id} only on the second board")
        case _ => None
      }
    }
    header ++ rows
  }
}
