package models

import org.joda.time.DateTime
import slick.jdbc.{JdbcBackend, GetResult}
import spire.math.{FixedScale, FixedPoint, Rational}
import scala.async.Async.{async, await}

import scala.concurrent.{ExecutionContext, Future}


case class Arrived(timestamp: DateTime, seconds: Int, afterFreeze: Boolean) {
  def asString = SecondsToTimeStr(seconds)
}
case class RatedProblem(id: String, rating: Int)

case class SubmitId(id: Int, arrived: Arrived, teamId: Int, contestId: Int, problem: RatedProblem, ext: String)

case class SubmitTopLevel(finished: Boolean, compiled: Boolean, passed: Int, taken: Int) {
  def success = finished && compiled && taken > 0 && passed == taken
}

case class Submit(submitId: SubmitId, status: SubmitTopLevel, testingId: Option[Int]) {
  def success = status.success
  def afterFreeze = submitId.arrived.afterFreeze
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

  def empty = SchoolCell(0, 0, false)
}

object RationalToScoreStr {
  implicit private val scale = FixedScale(100)

  def apply(r: Rational): String =
    if (r.isWhole()) r.toString
    else FixedPoint(r).toString(scale)
}

object SchoolScorer extends SubmitScorer[SchoolCell] {
  def apply(cell: SchoolCell, submit: Submit) =
    if (!submit.status.compiled)
      (cell, None)
    else {
      val newScore = SchoolCell.calculate(submit.submitId.problem.rating,
        Rational(submit.status.passed, submit.status.taken), cell.attempt + 1)

      (SchoolCell(cell.attempt + 1, cell.score.max(newScore), cell.fullSolution || submit.success),
        if (newScore > cell.score) Some(SchoolScore(newScore))
        else None)
    }
}

case class SchoolScore(value: Rational) extends Score {
  override def toString: String = RationalToScoreStr(value)
}

case class SchoolCell(attempt: Int, score: Rational, fullSolution: Boolean) {
  override def toString =
    if (attempt == 0) ""
    else RationalToScoreStr(score)
}

object ACMCell {
  def empty = ACMCell(0, 0, false)
}

object SecondsToTimeStr {
  def apply(time: Int) = "%02d:%02d".format(time / 3600, (time / 60) % 60)
}

case class ACMScore(value: Int) extends Score

object ACMScorer extends SubmitScorer[ACMCell] {
  def apply(cell: ACMCell, submit: Submit) =
    if (!submit.status.compiled || cell.fullSolution)
      (cell, None)
    else
      (ACMCell(cell.attempt + 1, submit.submitId.arrived.seconds, submit.success),
        Some(ACMScore(cell.arrivedSeconds / 60 + (cell.attempt - 1) * 20)))

}

case class ACMCell(attempt: Int, arrivedSeconds: Int, fullSolution: Boolean) {
  def score = if (fullSolution) (arrivedSeconds / 60 + (attempt - 1) * 20) else 0

  override def toString =
    if (attempt == 0) ""
    else if (fullSolution) ("+" + (if (attempt == 1) "" else (attempt - 1).toString))
    else s"-$attempt"
}

case class ResultEntry(test: Int, result: Int, time: Int, memory: Long, info: Int, testerExitCode: Int,
                       testerOutput: String, testerError: String) {
  def resultString = SubmitResult.message(result)
}

object ResultEntry {
  import slick.driver.MySQLDriver.api._

  implicit val getResultEntry = GetResult(r =>
    ResultEntry(r.nextInt(), r.nextInt(), r.nextInt(), r.nextLong(), r.nextInt(), r.nextInt(), r.nextString(), r.nextString())
  )
}

case class SubmitDetails(submit: Submit, details: Seq[ResultEntry])

object Submits {

  import slick.driver.MySQLDriver.api._

  case class ScoredSubmit[Sc](submit: Submit, score: Score, index: Int)

  //implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

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
        (next._1, state._2 :+(submit, next._2))
    }

  def indexAndScoreGrouped[Cell](submits: Seq[Submit], empty: Cell, scorer: SubmitScorer[Cell]) = {
    val scored = scoreGrouped(submits, empty, scorer)
    val indexed = indexGrouped(scored._2).map {
      case ((submit, score), index) => (submit, score, index)
    }
    scored._1 -> indexed
  }

  implicit val getSubmitResult = GetResult(r => Submit(
    SubmitId(r.nextInt(),
      Arrived(new DateTime(r.nextTimestamp()), r.nextInt(), r.nextBoolean()),
      r.nextInt(), r.nextInt(),
      RatedProblem(r.nextString(), r.nextInt()),
      r.nextString()),
    SubmitTopLevel(
      r.nextBoolean(),
      r.nextBoolean(),
      r.nextInt(), r.nextInt()), r.nextIntOption()
  ))

  def getContestSubmits(contest: Int) = {
    sql"""select NewSubmits.ID,
          NewSubmits.Arrived,
          unix_timestamp(NewSubmits.Arrived) - unix_timestamp(Contests.Start) as ArrivedSeconds,
          NewSubmits.Arrived > Contests.Finish as AfterFreeze,
          NewSubmits.Team, NewSubmits.Contest,
          NewSubmits.Problem, Problems.Rating,
          Languages.Ext, Submits.Finished, Submits.Compiled,
          Submits.Passed, Submits.Taken,

          Submits.TestingID
          from Contests, Problems, Languages, NewSubmits, Submits where NewSubmits.ID = Submits.ID and
          Contests.ID = $contest and NewSubmits.Arrived < Contests.End and NewSubmits.Arrived >= Contests.Start and
          Contests.ID = NewSubmits.Contest and Problems.Contest = Contests.ID and
          Languages.ID = NewSubmits.SrcLang and Languages.Contest = Contests.ID and
          Problems.ID = NewSubmits.Problem order by ArrivedSeconds""".as[Submit]
  }

  def getContestTeamSubmits(contest: Int, team: Int) = {
    sql"""select NewSubmits.ID,
          NewSubmits.Arrived,
          unix_timestamp(NewSubmits.Arrived) - unix_timestamp(Contests.Start) as ArrivedSeconds,
          NewSubmits.Arrived > Contests.Finish as AfterFreeze,
          NewSubmits.Team, NewSubmits.Contest,
          NewSubmits.Problem, Problems.Rating,
          Languages.Ext, Submits.Finished, Submits.Compiled,
          Submits.Passed, Submits.Taken,

          Submits.TestingID
          from Contests, Problems, Languages, NewSubmits LEFT join Submits on NewSubmits.ID = Submits.ID where
          NewSubmits.Team = $team and
          Contests.ID = $contest and NewSubmits.Arrived < Contests.End and NewSubmits.Arrived >= Contests.Start and
          Contests.ID = NewSubmits.Contest and Problems.Contest = Contests.ID and
          Languages.ID = NewSubmits.SrcLang and Languages.Contest = Contests.ID and
          Problems.ID = NewSubmits.Problem order by ArrivedSeconds""".as[Submit]
  }

  private def trOption(items: Seq[ResultEntry]): Option[Seq[ResultEntry]] =
    if (items.nonEmpty)
      Some(items)
    else
      None

  def getTestingMaxTime(details: Seq[ResultEntry]) =
    trOption(details).map(_.map(_.time).max).getOrElse(0)

  def getTestingLastResult(details: Seq[ResultEntry]) =
    trOption(details).map(_.maxBy(_.test))

  def annotateGrouped(db: JdbcBackend#DatabaseDef, schoolMode: Boolean, submits: Seq[Submit])(implicit ec: ExecutionContext) = {
    val scored = if (schoolMode) scoreGrouped[SchoolCell](submits, SchoolCell.empty, SchoolScorer)
      else scoreGrouped(submits, ACMCell.empty, ACMScorer)
    val indexed = indexGrouped(scored._2)

    Future.sequence(
      indexed.map {
        case ((submit, optScore), index) =>
          SubmitResult.annotate(db, schoolMode, submit).map { submitResult =>
            (submit, index, optScore, submitResult)
          }
      })
  }

  def groupAndAnnotate(db: JdbcBackend#DatabaseDef, schoolMode: Boolean, submits: Seq[Submit])(implicit ec: ExecutionContext) =
    Future.sequence(groupByTP(submits).values.map { grouped =>
      annotateGrouped(db, schoolMode, grouped)
    }).map(_.flatten.toSeq.sortBy(-_._1.submitId.arrived.seconds))


  def loadSubmitByID(db: JdbcBackend#DatabaseDef, submitId: Int)(implicit ec: ExecutionContext) = {
    db.run(sql"""select NewSubmits.ID,
          NewSubmits.Arrived,
          unix_timestamp(NewSubmits.Arrived) - unix_timestamp(Contests.Start) as ArrivedSeconds,
          NewSubmits.Arrived > Contests.Finish as AfterFreeze,
          NewSubmits.Team, NewSubmits.Contest,
          NewSubmits.Problem, Problems.Rating,
          Languages.Ext, Submits.Finished, Submits.Compiled,
          Submits.Passed, Submits.Taken,

          Submits.TestingID
          from Contests, Problems, Languages, NewSubmits LEFT join Submits on NewSubmits.ID = Submits.ID where
          NewSubmits.ID = $submitId and NewSubmits.Arrived < Contests.End and NewSubmits.Arrived >= Contests.Start and
          Contests.ID = NewSubmits.Contest and Problems.Contest = Contests.ID and
          Languages.ID = NewSubmits.SrcLang and Languages.Contest = Contests.ID and
          Problems.ID = NewSubmits.Problem order by ArrivedSeconds""".as[Submit]).map(_.headOption)
  }

  def loadSubmitDetails(db: JdbcBackend#DatabaseDef, testingId: Int)(implicit ec: ExecutionContext) =
    db.run(
      sql"""select Test, Result, Timex, Memory, Info, TesterExitCode, TesterOutput, TesterError
           from Results where UID = $testingId order by Test""".as[ResultEntry])

  def loadSubmitAndDetails(db: JdbcBackend#DatabaseDef, submitId: Int)(implicit ec: ExecutionContext): Future[Option[SubmitDetails]] =
    loadSubmitByID(db, submitId)
      .flatMap { submitOption =>
      submitOption.map { submit =>
        submit.testingId.map(loadSubmitDetails(db, _))
          .getOrElse(Future.successful(Nil))
          .map(details => Some(SubmitDetails(submit, details)))
      }.getOrElse(Future.successful(None))
    }
}