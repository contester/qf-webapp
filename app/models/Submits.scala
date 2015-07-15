package models

import org.joda.time.DateTime
import slick.jdbc.GetResult
import spire.math.Rational

trait AbstractSubmit {
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
}

trait SubmitId {
  def submitId: Int
}

trait ContestSubmit extends AbstractSubmit {
  def arrivedSeconds: Int
  def afterFreeze: Boolean
  def problemRating: Int

  def arrivedStr = "%02d:%02d".format(arrivedSeconds / 3600, (arrivedSeconds / 60) % 60)
}

trait Indexed {
  def index: Int
}

case class Submit(submitId: Int, arrivedTimestamp: DateTime, teamId: Int,
                  problem: String, ext: String, finished: Boolean,
                   compiled: Boolean, passed: Int, taken: Int, arrivedSeconds: Int,
                  afterFreeze: Boolean, problemRating: Int) extends ContestSubmit with SubmitId

trait SubmitScore[S] {
  def withSubmit(submit: ContestSubmit): S
}

trait CellScore[S] {
  def withCell(cell: S): CellScore[S]
}

case class SchoolScore(score: Rational) extends CellScore[SchoolCell] {
  override def withCell(cell: SchoolCell): CellScore[SchoolCell] =
    if (cell.score > 0)
      SchoolScore(score + cell.score)
    else
      this
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
}

case class SchoolCell(attempt: Int, score: Rational) extends SubmitScore[SchoolCell] {
  override def withSubmit(submit: ContestSubmit): SchoolCell =
    if (!submit.compiled)
      this
    else
      SchoolCell(attempt + 1, score.max(SchoolCell.calculate(submit.problemRating,
        Rational(submit.passed, submit.taken), attempt + 1)))
}

object Submits {
  import slick.driver.MySQLDriver.api._

  case class ScoredSubmit[S <: AbstractSubmit, Sc <: SubmitScore[Sc]](submit: S, score: Sc, index: Int)

  implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

  def indexSubmits[S <: ContestSubmit, Sc <: SubmitScore[Sc]](submits: Seq[S], scorer: => Sc): Seq[ScoredSubmit[S, Sc]] =
    submits.groupBy(x => (x.teamId, x.problem))
      .mapValues(x => scoreSubmits(x.sortBy(_.arrivedTimestamp), scorer)
      .zipWithIndex.map(x => ScoredSubmit(x._1._2, x._1._1, x._2 + 1))).values.toSeq.flatten

  def scoreSubmits[S <: ContestSubmit, Sc <: SubmitScore[Sc]](submits: Seq[S], empty: Sc): Seq[(Sc, S)] =
    submits.foldLeft((empty, Seq[(Sc, S)]())) {
      case (state, submit) =>
        val newLeft = state._1.withSubmit(submit)
        (newLeft, state._2 :+ (newLeft, submit))
    }._2

  def getContestSubmits(contest: Int) = {
    implicit val getSubmitResult = GetResult(r => Submit(
      r.nextInt(), new DateTime(r.nextTimestamp()), r.nextInt(), r.nextString(), r.nextString(), r.nextBoolean(),
      r.nextBoolean(),
      r.nextInt(), r.nextInt(), r.nextInt(), r.nextBoolean(), r.nextInt()
    ))

    sql"""select Submits.ID, Submits.Arrived,
          Team, Task, Ext, Finished, Compiled,
          Passed, Taken,
          unix_timestamp(Submits.Arrived) - unix_timestamp(Contests.Start) as Arrived0,
          Submits.Arrived > Contests.Finish, Problems.Rating from Contests, Submits, Problems where
          Contests.ID = $contest and Submits.Arrived < Contests.End and Submits.Arrived >= Contests.Start and
          Contests.ID = Submits.Contest and Submits.Finished and Problems.Contest = Contests.ID and
          Problems.ID = Submits.Task order by Arrived0""".as[Submit]
  }

  def getContestTeamSubmits(contest: Int, team: Int) = {
    implicit val getSubmitResult = GetResult(r => Submit(
      r.nextInt(), new DateTime(r.nextTimestamp()), r.nextInt(), r.nextString(), r.nextString(), r.nextBoolean(),
      r.nextBoolean(),
      r.nextInt(), r.nextInt(), r.nextInt(), r.nextBoolean(), r.nextInt()
    ))

    sql"""select Submits.ID, Submits.Arrived,
          Team, Task, Ext, Finished, Compiled,
          Passed, Taken,
          unix_timestamp(Submits.Arrived) - unix_timestamp(Contests.Start) as Arrived0,
          Submits.Arrived > Contests.Finish, Problems.Rating from Contests, Submits, Problems where
          Submits.Team = $team and
          Contests.ID = $contest and Submits.Arrived < Contests.End and Submits.Arrived >= Contests.Start and
          Contests.ID = Submits.Contest and Problems.Contest = Contests.ID and
          Problems.ID = Submits.Task order by Arrived0""".as[Submit]
  }
}