package models

import java.nio.charset.StandardCharsets

import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{Memory, TimeMs}
import play.api.Logging
import slick.jdbc.{GetResult, JdbcBackend}
import slick.lifted.MappedTo
import spire.math.Rational
import spire.math.extras.{FixedPoint, FixedScale}

import scala.concurrent.{ExecutionContext, Future}

case class Arrived(timestamp: DateTime, seconds: Int, afterFreeze: Boolean) {
  def asString = SecondsToTimeStr(seconds)
}

case class RatedProblem(id: String, rating: Int)

case class SubmitId(id: Int, arrived: Arrived, teamId: Int, contestId: Int, problem: RatedProblem, ext: String)

case class ProblemData(id: String, name: String, rating: Int)
case class TeamData(id: Long, name: String)
case class UntestedSubmit(id: Long, arrived: Arrived, team: TeamData, contest: Long, problem: ProblemData, ext: String)
case class SubmitTestingData(testingId: Long, finished: Boolean, compiled: Boolean, passed: Int, taken: Int,
                             maxTimeMs: Long, maxMemory: Long)
case class DatabaseSubmitRow(submit: UntestedSubmit, testingData: Option[SubmitTestingData])

case class Submit(submitId: SubmitId, finished: Boolean, compiled: Boolean, passed: Int, taken: Int, testingId: Option[Int]) {
  val success = finished && compiled && taken > 0 && passed == taken
  def afterFreeze = submitId.arrived.afterFreeze
  val failedOnFirst = taken == 1 && passed == 0
}

trait SubmitScorer[Cell] {
  def apply(cell: Cell, submit: Submit): (Cell, Option[Score])
}

trait Score

trait CellScore[Cell] {
  def withCell(cell: Cell): CellScore[Cell]
}

object SchoolCell {
  def calculate(base: Int, fraction: Rational, attempt: Int): Rational = {
    val tops = {
      val x = base - attempt + 1
      if (x < 20)
        20
      else
        x
    }
    if (fraction == 1)
      tops
    else {
      val x = tops * 2 * fraction / 3
      if (x < 5)
        0
      else
        x
    }
  }

  def empty = SchoolCell(0, 0, fullSolution = false)
}

object RationalToScoreStr {
  implicit private val scale = FixedScale(100)

  def apply(r: Rational): String =
    if (r.isWhole()) r.toString
    else FixedPoint(r).toString(scale)
}

object SchoolScorer extends SubmitScorer[SchoolCell] with Logging {
  def apply(cell: SchoolCell, submit: Submit) =
    if (!submit.compiled || !submit.finished || submit.taken == 0 || submit.failedOnFirst) {
      if (submit.compiled && submit.taken == 0) {
        logger.info(s"Compiled but taken = 0: $submit")
      }
      (cell, None)
    } else {
      val newScore = SchoolCell.calculate(submit.submitId.problem.rating,
        Rational(submit.passed, submit.taken), cell.attempt + 1)

      (SchoolCell(cell.attempt + 1, cell.score.max(newScore), cell.fullSolution || submit.success),
        if (newScore > cell.score) Some(SchoolScore(newScore))
        else None)
    }
}

case class SchoolScore(value: Rational) extends Score {
  override def toString: String = RationalToScoreStr(value)
}

trait ProblemCell {
  def fullSolution: Boolean
}

case class SchoolCell(attempt: Int, score: Rational, fullSolution: Boolean) extends ProblemCell {
  override def toString =
    if (attempt == 0) ""
    else RationalToScoreStr(score)
}

object ACMCell {
  def empty = ACMCell(0, 0, fullSolution = false)
}

object SecondsToTimeStr {
  def apply(time: Int) = "%02d:%02d".format(time / 3600, (time / 60) % 60)
}

case class ACMScore(value: Int) extends Score {
  override def toString: String = value.toString
}

object ACMScorer extends SubmitScorer[ACMCell] {
  def apply(cell: ACMCell, submit: Submit) =
    if (!submit.compiled || submit.failedOnFirst || cell.fullSolution)
      (cell, None)
    else {
      val result = ACMCell(cell.attempt + 1, submit.submitId.arrived.seconds, submit.success)
      (result, if (submit.success) Some(ACMScore(result.score)) else None)
    }
}

case class ACMCell(attempt: Int, arrivedSeconds: Int, fullSolution: Boolean) extends ProblemCell {
  def score = if (fullSolution) (arrivedSeconds / 60 + (attempt - 1) * 20) else 0

  override def toString =
    if (attempt == 0) ""
    else if (fullSolution) ("+" + (if (attempt == 1) "" else (attempt - 1).toString))
    else s"-$attempt"
}

case class ResultEntry(testingID: Long, test: Int, result: Int, time: TimeMs, memory: Memory, info: Long, testerExitCode: Long,
                       testerOutput: Array[Byte], testerError: Array[Byte]) {
  def resultString = SubmitResult.message(result)

  def testerOutputStr = new String(testerOutput, StandardCharsets.UTF_8)

  def testerErrorStr = new String(testerError, StandardCharsets.UTF_8)

  def timeLimitExceeded = result == 11

  def success =
    if (test == 0) result == 1
    else result == 10
}

case class SubmitDetails(fsub: FullyDescribedSubmit, source: Array[Byte]) {
  def sourceStr = new String(source, StandardCharsets.UTF_8)
}

case class FullyDescribedSubmit(submit: Submit, index: Int, score: Option[Score], result: SubmitResult,
                                stats: SubmitStats, details: Seq[ResultEntry])

object Submits {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel
  import org.stingray.contester.dbmodel.SlickModel.UpliftedSubmit

  case class ScoredSubmit[Sc](submit: Submit, score: Score, index: Int)

  def groupByTP(submits: Seq[Submit]) =
    submits.groupBy(x => (x.submitId.teamId, x.submitId.problem.id))
      .mapValues(_.sortBy(_.submitId.arrived.seconds))

  def indexGrouped[S](values: Seq[S]) =
    values.zipWithIndex.map(x => x._1 -> (x._2 + 1))

  def justIndexSubmits(submits: Seq[Submit]): Seq[(Submit, Int)] =
    groupByTP(submits)
      .mapValues(indexGrouped)
      .values.flatten.toSeq

  def scoreGrouped[Cell](submits: Seq[Submit], empty: Cell, scorer: SubmitScorer[Cell]): (Cell, Seq[(Submit, Option[Score])]) =
    submits.foldLeft((empty, Seq[(Submit, Option[Score])]())) {
      case (state, submit) =>
        val next = scorer(state._1, submit)
        (next._1, state._2 :+ ((submit, next._2)))
    }

  def indexAndScoreGrouped[Cell](submits: Seq[Submit], empty: Cell, scorer: SubmitScorer[Cell]) = {
    val scored = scoreGrouped(submits, empty, scorer)
    val indexed = indexGrouped(scored._2).map {
      case ((submit, score), index) => (submit, score, index)
    }
    scored._1 -> indexed
  }

  def upliftSub(s: UpliftedSubmit): Submit =
    Submit(
      SubmitId(s.id.toInt, Arrived(s.arrived, s.arrivedSeconds.toStandardSeconds.getSeconds, s.afterFreeze),
        s.teamID, s.contestID, RatedProblem(s.problemID, 30), s.ext), s.finished,
      s.compiled, s.passed, s.taken,
      if (s.testingID == 0) None else Some(s.testingID.toInt))

  def getContestSubmits(contest: Long) = {
    SlickModel.submits0.filter(_.contestID === contest.toInt).result
  }

  def getContestTeamSubmits(contest: Int, team: Int) = {
    SlickModel.submits0.filter(x => (x.contestID === contest && x.teamID === team)).result
  }

  def getContestTeamProblemSubmits(contest: Int, team: Int, problem: String) = {
    SlickModel.submits0.filter(x => (x.contestID === contest && x.teamID === team && x.problemID === problem)).result
  }

  private def trOption(items: Seq[ResultEntry]): Option[Seq[ResultEntry]] =
    if (items.nonEmpty)
      Some(items)
    else
      None

  def getTestingStats(details: Seq[ResultEntry]) =
    trOption(details.filter(_.test != 0)).map { detlist =>
      SubmitStats(detlist.map(_.time).max, detlist.map(_.memory).max, detlist.exists(_.timeLimitExceeded))
    }.getOrElse(SubmitStats(TimeMs(0), Memory(0)))

  def getTestingLastResult(details: Seq[ResultEntry]) =
    trOption(details).map(_.maxBy(_.test))

  private[this] def annotateGrouped(db: JdbcBackend#DatabaseDef, schoolMode: Boolean, submits: Seq[Submit])(implicit ec: ExecutionContext) = {
    val scored = if (schoolMode) scoreGrouped[SchoolCell](submits, SchoolCell.empty, SchoolScorer)
    else scoreGrouped(submits, ACMCell.empty, ACMScorer)
    val indexed = indexGrouped(scored._2)

    Future.sequence(
      indexed.map {
        case ((submit, optScore), index) =>
          SubmitResult.annotate(db, schoolMode, submit).map {
            case (submitResult, submitDetails, submitStats) =>
              FullyDescribedSubmit(submit, index, optScore, submitResult, submitStats, submitDetails)
          }
      })
  }

  def groupAndAnnotate(db: JdbcBackend#DatabaseDef, schoolMode: Boolean, submits: Seq[Submit])(implicit ec: ExecutionContext) =
    Future.sequence(groupByTP(submits).values.map { grouped =>
      annotateGrouped(db, schoolMode, grouped)
    }).map(_.flatten.toSeq.sortBy(-_.submit.submitId.arrived.seconds))


  import scala.language.implicitConversions

  implicit def o2f[A](o: Option[Future[A]])(implicit ec: ExecutionContext): Future[Option[A]] =
    o.map(_.map(Some(_))).getOrElse(Future.successful(None))

  def getSubmitById(db: JdbcBackend#DatabaseDef, submitId: Int)(implicit ec: ExecutionContext): Future[Option[SubmitDetails]] = {
    val pq = SlickModel.submits.filter(_.id === submitId.toLong).map(x => (x.contest, x.team, x.problem, x.source)).take(1).result.headOption
    db.run(pq)
      .flatMap { maybeSubmit =>
        maybeSubmit map { short =>
          db.run(getContestTeamProblemSubmits(short._1, short._2, short._3))
            .flatMap(submits =>
              groupAndAnnotate(db, false, submits.map(upliftSub(_)))).map { submits =>
            submits.find(_.submit.submitId.id == submitId).map(SubmitDetails(_, short._4))
          }
        }
      }.map(_.flatten)
  }

  def loadSubmitDetails(db: JdbcBackend#DatabaseDef, testingId: Int)(implicit ec: ExecutionContext) =
    db.run(SlickModel.results0.filter(_.testingID === testingId.toLong).result).map(_.map(x =>
      ResultEntry(x.testingID, x.testID, x.resultCode, x.timeMs, x.memoryBytes,
        x.returnCode, x.testerReturnCode, x.testerOutput, x.testerError)
    ))

  def dbsub2sub(s: DatabaseSubmitRow): Submit = {
    Submit(SubmitId(s.submit.id.toInt, s.submit.arrived, s.submit.team.id.toInt, s.submit.contest.toInt, RatedProblem(s.submit.problem.id, s.submit.problem.rating), s.submit.ext),
      s.testingData.exists(_.finished), s.testingData.exists(_.compiled),
      s.testingData.map(_.passed).getOrElse(0), s.testingData.map(_.taken).getOrElse(0),
      s.testingData.map(_.testingId.toInt))
  }
}
