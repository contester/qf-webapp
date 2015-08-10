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

case class Submit(submitId: SubmitId, finished: Boolean,
                  compiled: Boolean, passed: Int, taken: Int, testingId: Option[Int]) {
  def success = finished && compiled && taken > 0 && passed == taken
  def afterFreeze = submitId.arrived.afterFreeze
}

trait SubmitScore[S] {
  def withSubmit(submit: Submit): S
}

trait CellScore[S] {
  def withCell(cell: S): CellScore[S]
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

case class SchoolCell(attempt: Int, score: Rational, fullSolution: Boolean) extends SubmitScore[SchoolCell] {
  override def toString =
    if (attempt == 0) ""
    else RationalToScoreStr(score)

  override def withSubmit(submit: Submit): SchoolCell =
    if (!submit.compiled)
      this
    else
      SchoolCell(attempt + 1, score.max(SchoolCell.calculate(submit.submitId.problem.rating,
        Rational(submit.passed, submit.taken), attempt + 1)), fullSolution || submit.success)
}

object ACMCell {
  def empty = ACMCell(0, 0, false)
}

object SecondsToTimeStr {
  def apply(time: Int) = "%02d:%02d".format(time / 3600, (time / 60) % 60)
}

case class ACMCell(attempt: Int, arrivedSeconds: Int, fullSolution: Boolean) extends SubmitScore[ACMCell] {
  def score = if (fullSolution) (arrivedSeconds / 60 + (attempt - 1) * 20) else 0

  override def toString =
    if (attempt == 0) ""
    else if (fullSolution) ("+" + (if (attempt == 1) "" else (attempt - 1).toString))
    else s"-$attempt"

  override def withSubmit(submit: Submit): ACMCell =
    if (!submit.compiled || fullSolution)
      this
    else
      ACMCell(attempt + 1, submit.submitId.arrived.seconds, submit.success)
}

trait AnyScoreAndStatus {
}

trait AnyStatusSubmit {
  def submit: Submit

  def arrived: Int = submit.submitId.arrived.seconds
  def problem: String = submit.submitId.problem.id
  def attempt: Int
  def ext: String = submit.submitId.ext
  def timeMs: Int
  def finished: Boolean = submit.finished
  def compiled: Boolean = submit.compiled
  def passed: Int = submit.passed
  def taken: Int = submit.taken

  def anyScoreAndStatus: AnyScoreAndStatus

  def arrivedStr = SecondsToTimeStr(arrived)

  def success = finished && compiled && taken > 0 && passed == taken

  def asSchool =
    if (!finished) "..."
    else if (!compiled) "Compilation failed"
    else if (passed == taken) "Полное решение"
    else s"${passed} из ${taken}"
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

  type SubmitScorerFunc[Sc] = Seq[Submit] => Sc

  def score2[Sc](submits: Seq[Submit], scorer: SubmitScorerFunc[Sc]) =
    groupByTP(submits)
      .mapValues(scorer)

  case class ScoredSubmit[Sc <: SubmitScore[Sc]](submit: Submit, score: Sc, index: Int)

  implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

  def indexSubmits[Sc <: SubmitScore[Sc]](submits: Seq[Submit], scorer: => Sc): Seq[ScoredSubmit[Sc]] =
    groupByTP(submits)
      .mapValues(x => scoreSubmits(x, scorer)
      .zipWithIndex.map(x => ScoredSubmit(x._1._2, x._1._1, x._2 + 1))).values.toSeq.flatten

  private def groupByTP(submits: Seq[Submit]) =
    submits.groupBy(x => (x.submitId.teamId, x.submitId.problem.id))
      .mapValues(_.sortBy(_.submitId.arrived.seconds))

  def justIndexSubmits(submits: Seq[Submit]): Seq[(Submit, Int)] =
    groupByTP(submits)
      .mapValues(_.zipWithIndex.map(x => x._1 -> (x._2 + 1)))
      .values.flatten.toSeq

  def scoreSubmits[Sc <: SubmitScore[Sc]](submits: Seq[Submit], empty: Sc): Seq[(Sc, Submit)] =
    submits.foldLeft((empty, Seq[(Sc, Submit)]())) {
      case (state, submit) =>
        val newLeft = state._1.withSubmit(submit)
        (newLeft, state._2 :+(newLeft, submit))
    }._2

  def getContestSubmits(contest: Int) = {
    implicit val getSubmitResult = GetResult(r => Submit(
      SubmitId(r.nextInt(),
        Arrived(new DateTime(r.nextTimestamp()), r.nextInt(), r.nextBoolean()),
        r.nextInt(), r.nextInt(),
        RatedProblem(r.nextString(), r.nextInt()),
        r.nextString()),
      r.nextBoolean(),
      r.nextBoolean(),
      r.nextInt(), r.nextInt(), r.nextIntOption()
    ))

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
    implicit val getSubmitResult = GetResult(r => Submit(
      SubmitId(r.nextInt(),
        Arrived(new DateTime(r.nextTimestamp()), r.nextInt(), r.nextBoolean()),
        r.nextInt(), r.nextInt(),
        RatedProblem(r.nextString(), r.nextInt()),
        r.nextString()),
      r.nextBoolean(),
      r.nextBoolean(),
      r.nextInt(), r.nextInt(), r.nextIntOption()
    ))

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

  case class SchoolScoreAndStatus(score: Rational) extends AnyScoreAndStatus
  case class StatusSubmit[X <: AnyScoreAndStatus](submit: Submit, attempt: Int,
    timeMs: Int, scoreAndStatus: X) extends AnyStatusSubmit {
    override def anyScoreAndStatus: AnyScoreAndStatus = scoreAndStatus
  }

  case class ACMScoreAndStatus(message: String, test: Option[Int]) extends AnyScoreAndStatus {
    def toStatus =
      s"${message}${test.map(x => s" on test $x").getOrElse("")}"
  }

  def trOption(items: Seq[ResultEntry]): Option[Seq[ResultEntry]] =
    if (items.nonEmpty)
      Some(items)
    else
      None

  def getTestingMaxTime(details: Seq[ResultEntry]) =
    trOption(details).map(_.map(_.time).max).getOrElse(0)

  def getTestingLastResult(details: Seq[ResultEntry]) =
    trOption(details).map(_.maxBy(_.test))

  def annotateSchoolSubmit(db: JdbcBackend#DatabaseDef,
                           sub: ScoredSubmit[SchoolCell])(implicit ec: ExecutionContext): Future[StatusSubmit[SchoolScoreAndStatus]] =
    sub.submit.testingId.map(loadSubmitDetails(db, _)).getOrElse(Future.successful(Nil)).map { details =>
      val timeMs = getTestingMaxTime(details)
      StatusSubmit(sub.submit, sub.index, timeMs, SchoolScoreAndStatus(sub.score.score))
    }

  def annotateSchoolSubmits(db: JdbcBackend#DatabaseDef, submits: Seq[Submit])(implicit ec: ExecutionContext): Future[Seq[StatusSubmit[SchoolScoreAndStatus]]] =
    Future.sequence(
      indexSubmits[SchoolCell](submits, SchoolCell.empty).sortBy(_.submit.submitId.arrived.seconds)
        .reverse.map(annotateSchoolSubmit(db, _))
    )

  def annotateACMSubmit(db: JdbcBackend#DatabaseDef,
                       sub: (Submit, Int))(implicit ec: ExecutionContext) =
    if (!sub._1.finished)
      Future.successful(StatusSubmit(sub._1, sub._2, 0, ACMScoreAndStatus("Waiting", None)))
    else sub._1.testingId.map { testingId =>
      loadSubmitDetails(db, testingId).map { details =>
        val timeMs = getTestingMaxTime(details)
        val resultOption = getTestingLastResult(details)
          val testIdOption = resultOption.flatMap { res =>
            if (res.test != 0 && !SubmitResult.success(res.result))
              Some(res.test)
            else
              None
          }
          StatusSubmit(sub._1, sub._2, timeMs, ACMScoreAndStatus(resultOption.map(x => SubmitResult.message(x.result)).getOrElse("..."), testIdOption))
        }

    }.getOrElse(Future.successful(StatusSubmit(sub._1, sub._2, 0, ACMScoreAndStatus("...", None))))

  def annotateACMSubmits(db: JdbcBackend#DatabaseDef, submits: Seq[Submit])(implicit ec: ExecutionContext) =
    Future.sequence(
      justIndexSubmits(submits).sortBy(_._1.submitId.arrived.seconds).reverse.map(annotateACMSubmit(db, _))
    )

  def loadSubmitByID(db: JdbcBackend#DatabaseDef, submitId: Int)(implicit ec: ExecutionContext) = {
    implicit val getSubmitResult = GetResult(r => Submit(
      SubmitId(r.nextInt(),
        Arrived(new DateTime(r.nextTimestamp()), r.nextInt(), r.nextBoolean()),
        r.nextInt(), r.nextInt(),
        RatedProblem(r.nextString(), r.nextInt()),
        r.nextString()),
      r.nextBoolean(),
      r.nextBoolean(),
      r.nextInt(), r.nextInt(), r.nextIntOption()
    ))

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