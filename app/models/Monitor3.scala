package models

//trait Scorer3 {
//  def score(success: Submit, failures: Seq[Submit]): Long
//}
//
//object ACMScorer3 extends Scorer3 {
//  override def score(success: Submit, failures: Seq[Submit]): Long = {
//    success.submitId.arrived.seconds / 60 + failures.length * 20
//  }
//}
//
//object Monitor3 {
//  type SubmitScore = Option[Long]
//
//  case class CellScore(success: SubmitScore, failedAttempts: Int)
//  case class RowScore(solved: Int, penalty: Long)
//  case class MonitorRow3(team: Int, rank: Option[Int])
//
//  case object RowScoreOrdering extends Ordering[RowScore] {
//    override def compare(x: RowScore, y: RowScore): Int = {
//      val sc = y.solved.compareTo(x.solved)
//      if (sc == 0)
//        x.penalty.compareTo(y.penalty)
//      else
//        sc
//    }
//  }
//}
//
//case class ProblemSubmitsScored(data: Map[Int, Submit], score: Monitor3.CellScore)
//
//object ProblemSubmitsScored {
//  val empty = ProblemSubmitsScored(Map.empty, Monitor3.CellScore(None, 0))
//  def build(submits: Iterable[Submit]): ProblemSubmitsScored = {
//    val data = submits.map(x => x.submitId.id -> x).toMap
//    val (sc, _) = score(data, None)
//    ProblemSubmitsScored(data, sc)
//  }
//
//  def score(data: Map[Int, Submit], submitToScore: Option[Int]): (Monitor3.CellScore, Monitor3.SubmitScore) = {
//    val (failures, successes) = data.values.toSeq.sortBy(_.submitId.arrived.seconds).span(!_.success)
//    val success = successes.headOption
//
//    val successScore = success.map(ACMScorer3.score(_, failures))
//    val ss = submitToScore.flatMap { sid =>
//      if (success.exists(_.submitId.id == sid)) {
//        successScore
//      } else None
//    }
//    (Monitor3.CellScore(successScore, failures.length), ss)
//  }
//
//  def update(prev: ProblemSubmitsScored, submit: Submit): (ProblemSubmitsScored, Monitor3.SubmitScore) = {
//    val next = prev.data.updated(submit.submitId.id, submit)
//    val (nextCell, submitScore) = score(next, Some(submit.submitId.id))
//    (ProblemSubmitsScored(next, nextCell), submitScore)
//  }
//}
//
//case class TeamSubmitsScored(data: Map[String, ProblemSubmitsScored], score: Monitor3.RowScore)
//
//object TeamSubmitsScored {
//  val empty = TeamSubmitsScored(Map.empty, Monitor3.RowScore(0, 0))
//
//  def build(submits: Iterable[Submit]): TeamSubmitsScored = {
//    val data = submits.groupBy(_.submitId.problem.id).mapValues(ProblemSubmitsScored.build(_))
//    TeamSubmitsScored(data, score(data))
//  }
//
//  def score(data: Map[String, ProblemSubmitsScored]): Monitor3.RowScore =
//    data.values.foldLeft(Monitor3.RowScore(0, 0)) {
//      case (c, next) =>
//        next.score.success match {
//          case Some(v) => Monitor3.RowScore(c.solved+1, c.penalty+v)
//          case None => c
//        }
//    }
//
//  def update(prev: TeamSubmitsScored, submit: Submit): (TeamSubmitsScored, Monitor3.SubmitScore) = {
//    // TODO:refactor
//    prev.data.get(submit.submitId.problem.id) match {
//      case Some(prevCell) =>
//        val (next, sc) = ProblemSubmitsScored.update(prevCell, submit)
//        val nd = prev.data.updated(submit.submitId.problem.id, next)
//        (TeamSubmitsScored(nd, score(nd)), sc)
//      case None =>
//        val (next, sc) = ProblemSubmitsScored.update(ProblemSubmitsScored.empty, submit)
//        val nd = prev.data.updated(submit.submitId.problem.id, next)
//        (TeamSubmitsScored(nd, score(nd)), sc)
//    }
//  }
//}
//
//case class ScoredSubmit(submit: Submit, score: Monitor3.SubmitScore)
//
//case class MonitorInstance(data: Map[Int, TeamSubmitsScored], display: Seq[Monitor3.MonitorRow3])
//
//object MonitorInstance {
//  def score(data: Map[Int, TeamSubmitsScored], teams: Map[Int, Team]): Seq[Monitor3.MonitorRow3] = {
//    val md = data.filterKeys(teams.contains(_)).mapValues(_.score).toSeq.sortBy(_._2)
//    md.foldLeft()
//  }
//
//
//  def update(prev: MonitorInstance, submit: Submit): (MonitorInstance, Monitor3.SubmitScore) = {
//    prev.data.get()
//  }
//}