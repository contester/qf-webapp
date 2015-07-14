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
}

trait SubmitId {
  def submitId: Int
}

trait ContestSubmit {
  def arrivedSeconds: Int
  def afterFreeze: Boolean

  def arrivedStr = "%02d:%02d".format(arrivedSeconds / 3600, (arrivedSeconds / 60) % 60)
}

trait Indexed {
  def index: Int
}

trait IndexedSubmit extends AbstractSubmit with Indexed

trait SchoolScoredSubmit extends ContestSubmit with IndexedSubmit {
  def score: Rational
}

trait SubmitList[S <: IndexedSubmit] {
  def submits: Iterable[S]
}

case class Submit(submitId: Int, arrivedTimestamp: DateTime, teamId: Int,
                  problem: String, ext: String, finished: Boolean,
                   compiled: Boolean, passed: Int, taken: Int, arrivedSeconds: Int,
                  afterFreeze: Boolean) extends AbstractSubmit with ContestSubmit with SubmitId

object Submits {
  import slick.driver.MySQLDriver.api._

  def getContestSubmits(contest: Int) = {
    implicit val getSubmitResult = GetResult(r => Submit(
      r.nextInt(), new DateTime(r.nextTimestamp()), r.nextInt(), r.nextString(), r.nextString(), r.nextBoolean(),
      r.nextBoolean(),
      r.nextInt(), r.nextInt(), r.nextInt(), r.nextBoolean()
    ))

    sql"""select Submits.ID, Submits.Arrived,
          Team, Task, Ext, Finished, Compiled,
          Passed, Taken,
          unix_timestamp(Submits.Arrived) - unix_timestamp(Contests.Start) as Arrived0,
          Submits.Arrived > Contests.Finish from Contests, Submits where
          Contests.ID = $contest and Submits.Arrived < Contests.End and Submits.Arrived >= Contests.Start and
          Contests.ID = Submits.Contest and Submits.Finished order by Arrived0""".as[Submit]
  }
}