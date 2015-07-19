package models

import com.github.nscala_time.time.Imports._
import slick.jdbc.GetResult

case class Contest(id: Int, name: String, schoolMode: Boolean, startTime: DateTime, endTime: DateTime,
                   freezeTime: DateTime, exposeTime: DateTime) {
  def frozen = (DateTime.now >= freezeTime) && (DateTime.now < exposeTime)
  def finished = DateTime.now >= endTime
  def started = DateTime.now >= startTime

  def getProblems = Contests.getProblems(id)
  def getCompilers = Contests.getCompilers(id)
}

case class Problem(id: String, name: String, tests: Int, rating: Int)

case class Compiler(id: Int, name: String, ext: String)

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
}

object Problems {
  def toSelect(problems: Seq[Problem]) =
    problems.map(x => x.id -> s"${x.id}. ${x.name}")
}