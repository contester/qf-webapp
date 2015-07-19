package models

import org.joda.time.DateTime
import slick.jdbc.{JdbcBackend, GetResult}
import spire.math.{FixedScale, FixedPoint, Rational}

import scala.concurrent.{ExecutionContext, Future}

trait SubmitId {
  def submitId: Int
}

trait ContestSubmit {
  def arrivedTimestamp: DateTime
  def teamId: Int
  def problem: String
  def ext: String
  def finished: Boolean
  def compiled: Boolean
  def passed: Int
  def taken: Int

  def success = finished && compiled && taken > 0 && passed == taken

  def asSchool =
    if (!finished) "..."
    else if (!compiled) "Compilation failed"
    else if (passed == taken) "Полное решение"
    else s"$passed из $taken"

  def arrivedSeconds: Int
  def afterFreeze: Boolean
  def problemRating: Int

  def arrivedStr = SecondsToTimeStr(arrivedSeconds)
}

trait Indexed {
  def index: Int
}

case class Submit(submitId: Int, arrivedTimestamp: DateTime, teamId: Int,
                  problem: String, ext: String, finished: Boolean,
                   compiled: Boolean, passed: Int, taken: Int, arrivedSeconds: Int,
                  afterFreeze: Boolean, problemRating: Int, testingId: Option[Int]) extends ContestSubmit with SubmitId

trait SubmitScore[S] {
  def withSubmit(submit: ContestSubmit): S
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

  override def withSubmit(submit: ContestSubmit): SchoolCell =
    if (!submit.compiled)
      this
    else
      SchoolCell(attempt + 1, score.max(SchoolCell.calculate(submit.problemRating,
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

  override def withSubmit(submit: ContestSubmit): ACMCell =
    if (!submit.compiled || fullSolution)
      this
    else
      ACMCell(attempt + 1, submit.arrivedSeconds, submit.success)
}

trait AnyScoreAndStatus {
}

trait AnyStatusSubmit {
  def arrived: Int
  def problem: String
  def attempt: Int
  def ext: String
  def timeMs: Int
  def finished: Boolean
  def compiled: Boolean
  def passed: Int
  def taken: Int

  def anyScoreAndStatus: AnyScoreAndStatus

  def arrivedStr = SecondsToTimeStr(arrived)

  def success = finished && compiled && taken > 0 && passed == taken

  def asSchool =
    if (!finished) "..."
    else if (!compiled) "Compilation failed"
    else if (passed == taken) "Полное решение"
    else s"${passed} из ${taken}"
}

trait SubmitScorer2[S] {
  def withSubmit(submit: ContestSubmit): SubmitScorer2[S]
  def score: S
}

object Submits {

  import slick.driver.MySQLDriver.api._

  type SubmitScorerFunc[Sc] = Seq[Submit] => Sc

  def score2[Sc](submits: Seq[Submit], scorer: SubmitScorerFunc[Sc]) =
    submits.groupBy(x => (x.teamId, x.problem))
      .mapValues(x => scorer(x.sortBy(_.arrivedSeconds)))

  case class ScoredSubmit[S <: ContestSubmit, Sc <: SubmitScore[Sc]](submit: S, score: Sc, index: Int)

  implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

  def indexSubmits[S <: ContestSubmit, Sc <: SubmitScore[Sc]](submits: Seq[S], scorer: => Sc): Seq[ScoredSubmit[S, Sc]] =
    submits.groupBy(x => (x.teamId, x.problem))
      .mapValues(x => scoreSubmits(x.sortBy(_.arrivedTimestamp), scorer)
      .zipWithIndex.map(x => ScoredSubmit(x._1._2, x._1._1, x._2 + 1))).values.toSeq.flatten

  def justIndexSubmits(submits: Seq[Submit]): Seq[(Submit, Int)] =
    submits.groupBy(x => (x.teamId, x.problem))
      .mapValues(_.sortBy(_.arrivedSeconds).zipWithIndex.map(x => x._1 -> (x._2 + 1)))
      .values.flatten.toSeq

  def scoreSubmits[S <: ContestSubmit, Sc <: SubmitScore[Sc]](submits: Seq[S], empty: Sc): Seq[(Sc, S)] =
    submits.foldLeft((empty, Seq[(Sc, S)]())) {
      case (state, submit) =>
        val newLeft = state._1.withSubmit(submit)
        (newLeft, state._2 :+(newLeft, submit))
    }._2

  def getContestSubmits(contest: Int) = {
    implicit val getSubmitResult = GetResult(r => Submit(
      r.nextInt(), new DateTime(r.nextTimestamp()), r.nextInt(), r.nextString(), r.nextString(), r.nextBoolean(),
      r.nextBoolean(),
      r.nextInt(), r.nextInt(), r.nextInt(), r.nextBoolean(), r.nextInt(), r.nextIntOption()
    ))

    sql"""select Submits.ID, Submits.Arrived,
          Team, Task, Ext, Finished, Compiled,
          Passed, Taken,
          unix_timestamp(Submits.Arrived) - unix_timestamp(Contests.Start) as Arrived0,
          Submits.Arrived > Contests.Finish, Problems.Rating, Submits.TestingID
          from Contests, Submits, Problems where
          Contests.ID = $contest and Submits.Arrived < Contests.End and Submits.Arrived >= Contests.Start and
          Contests.ID = Submits.Contest and Submits.Finished and Problems.Contest = Contests.ID and
          Problems.ID = Submits.Task order by Arrived0""".as[Submit]
  }

  def getContestTeamSubmits(contest: Int, team: Int) = {
    implicit val getSubmitResult = GetResult(r => Submit(
      r.nextInt(), new DateTime(r.nextTimestamp()), r.nextInt(), r.nextString(), r.nextString(), r.nextBoolean(),
      r.nextBoolean(),
      r.nextInt(), r.nextInt(), r.nextInt(), r.nextBoolean(), r.nextInt(), r.nextIntOption()
    ))

    sql"""select NewSubmits.ID, NewSubmits.Arrived,
          NewSubmits.Team, NewSubmits.Problem, Languages.Ext, Submits.Finished, Submits.Compiled,
          Submits.Passed, Submits.Taken,
          unix_timestamp(NewSubmits.Arrived) - unix_timestamp(Contests.Start) as Arrived0,

          NewSubmits.Arrived > Contests.Finish, Problems.Rating, Submits.TestingID
          from Contests, Problems, Languages, NewSubmits LEFT join Submits on NewSubmits.ID = Submits.ID where
          NewSubmits.Team = $team and
          Contests.ID = $contest and NewSubmits.Arrived < Contests.End and NewSubmits.Arrived >= Contests.Start and
          Contests.ID = NewSubmits.Contest and Problems.Contest = Contests.ID and
          Languages.ID = NewSubmits.SrcLang and Languages.Contest = Contests.ID and
          Problems.ID = NewSubmits.Problem order by Arrived0""".as[Submit]
  }

  case class SchoolScoreAndStatus(score: Rational) extends AnyScoreAndStatus
  case class StatusSubmit[X <: AnyScoreAndStatus](arrived: Int, problem: String, attempt: Int, ext: String,
    timeMs: Int, finished: Boolean, compiled: Boolean, passed: Int, taken: Int, scoreAndStatus: X) extends AnyStatusSubmit {
    override def anyScoreAndStatus: AnyScoreAndStatus = scoreAndStatus
  }

  case class ACMScoreAndStatus(message: String, test: Option[Int]) extends AnyScoreAndStatus {
    def toStatus =
      s"${message}${test.map(x => s" on test $x").getOrElse("")}"
  }

  def getTestingMaxTimeQuery(testingId: Int) =
    sql"""select max(Timex) from Results where UID = $testingId""".as[Int]

  def getTestingLastFailureQuery(testingId: Int) =
    sql"""select ResultDesc.Description, Results.Test, ResultDesc.Success from Results, ResultDesc
         where Results.UID = $testingId and ResultDesc.ID = Results.Result
         order by Results.Test desc limit 1
      """.as[(String, Option[Int], Boolean)]

  def getTestingMaxTime(db: JdbcBackend#DatabaseDef, testingId: Int)(implicit ec: ExecutionContext): Future[Int] =
    db.run(getTestingMaxTimeQuery(testingId)).map(_.headOption.getOrElse(0))

  def getTestingLastResult(db: JdbcBackend#DatabaseDef, testingId: Int)(implicit ec: ExecutionContext) =
    db.run(getTestingLastFailureQuery(testingId)).map(_.headOption)

  def annotateSchoolSubmit(db: JdbcBackend#DatabaseDef,
                           sub: ScoredSubmit[Submit, SchoolCell])(implicit ec: ExecutionContext): Future[StatusSubmit[SchoolScoreAndStatus]] =
    sub.submit.testingId.map(getTestingMaxTime(db, _)).getOrElse(Future.successful(0)).map { timeMs =>
      StatusSubmit(sub.submit.arrivedSeconds, sub.submit.problem, sub.index, sub.submit.ext, timeMs, sub.submit.finished,
        sub.submit.compiled, sub.submit.passed, sub.submit.taken, SchoolScoreAndStatus(sub.score.score))
    }

  def annotateSchoolSubmits(db: JdbcBackend#DatabaseDef, submits: Seq[Submit])(implicit ec: ExecutionContext): Future[Seq[StatusSubmit[SchoolScoreAndStatus]]] =
    Future.sequence(
      indexSubmits[Submit, SchoolCell](submits, SchoolCell.empty).sortBy(_.submit.arrivedSeconds).reverse.map(annotateSchoolSubmit(db, _))
    )

  def annotateACMSubmit(db: JdbcBackend#DatabaseDef,
                       sub: (Submit, Int))(implicit ec: ExecutionContext) =
    if (!sub._1.finished)
      Future.successful(StatusSubmit(sub._1.arrivedSeconds, sub._1.problem, sub._2, sub._1.ext, 0, sub._1.finished,
        sub._1.compiled, sub._1.passed, sub._1.taken, ACMScoreAndStatus("Waiting", None)))
    else sub._1.testingId.map { testingId =>
      getTestingMaxTime(db, testingId).flatMap { timeMs =>
        getTestingLastResult(db, testingId).map { resultOption =>
          val testIdOption = resultOption.flatMap { res =>
            if (!res._3)
              res._2
            else
              None
          }
          StatusSubmit(sub._1.arrivedSeconds, sub._1.problem, sub._2, sub._1.ext, timeMs, sub._1.finished,
            sub._1.compiled, sub._1.passed, sub._1.taken, ACMScoreAndStatus(resultOption.map(_._1).getOrElse("..."), testIdOption))
        }
      }
    }.getOrElse(Future.successful(StatusSubmit(sub._1.arrivedSeconds, sub._1.problem, sub._2, sub._1.ext, 0, sub._1.finished,
      sub._1.compiled, sub._1.passed, sub._1.taken, ACMScoreAndStatus("...", None))))

  def annotateACMSubmits(db: JdbcBackend#DatabaseDef, submits: Seq[Submit])(implicit ec: ExecutionContext) =
    Future.sequence(
      justIndexSubmits(submits).sortBy(_._1.arrivedSeconds).reverse.map(annotateACMSubmit(db, _))
    )
}