package models

import com.github.nscala_time.time.Imports._
import org.joda.time.DateTime
import slick.jdbc.GetResult

case class Contest(id: Int, name: String, schoolMode: Boolean, startTime: DateTime, endTime: DateTime,
                   freezeTime: DateTime, exposeTime: DateTime) {
  def frozen = (DateTime.now >= freezeTime) && (DateTime.now < exposeTime)
  def finished = DateTime.now >= endTime
  def started = DateTime.now >= startTime

  def getProblems = Contests.getProblems(id)
  def getCompilers = Contests.getCompilers(id)
  def getClarifications = Contests.getClarifications(id)
}

case class Problem(id: String, name: String, tests: Int, rating: Int)

case class Compiler(id: Int, name: String, ext: String)

object Compilers {
  def toSelect(compilers: Seq[Compiler]) =
    compilers.map(x => x.id.toString -> x.name)
}

object Contests {
  import slick.driver.MySQLDriver.api._

  implicit private val getProblem = GetResult(r => Problem(r.nextString().toUpperCase, r.nextString(),
    r.nextInt(), r.nextInt()))

  def getProblems(contest: Int) =
    sql"""select ID, Name, Tests, Rating from Problems where Contest = $contest order by ID""".as[Problem]

  implicit private val getCompiler = GetResult(
    r => Compiler(r.nextInt(), r.nextString(), r.nextString())
  )

  def getCompilers(contest: Int) =
    sql"""select ID, Name, Ext from Languages where Contest = $contest order by ID""".as[Compiler]

  implicit private val getClarification = GetResult(
    r => Clarification(new DateTime(r.nextTimestamp()), r.nextString().toUpperCase, r.nextString()))

  def getClarifications(contestId: Int) =
    sql"""select cl_date, cl_task, cl_text from clarifications where cl_is_hidden = '0' and
         cl_contest_idf = $contestId""".as[Clarification]
}

object Problems {
  def toSelect(problems: Seq[Problem]) =
    problems.map(x => x.id -> s"${x.id}. ${x.name}")
}

case class Clarification(time: DateTime, problem: String, text: String)
case class ClarificationRequest(time: DateTime, problem: String, text: String, answer: String)

