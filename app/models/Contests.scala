package models

import com.github.nscala_time.time.Imports._
import play.api.libs.EventSource.{EventNameExtractor, EventIdExtractor, EventDataExtractor}
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.{JsValue, Json, Writes}
import slick.jdbc.GetResult

case class Contest(id: Int, name: String, schoolMode: Boolean, startTime: DateTime, endTime: DateTime,
                   freezeTime: DateTime, exposeTime: DateTime) {
  def frozen = (DateTime.now >= freezeTime) && (DateTime.now < exposeTime)
  def finished = DateTime.now >= endTime
  def started = DateTime.now >= startTime

  def timevalHMS =
    Contests.formatHMS(if (started) {
      if (!finished)
        (DateTime.now to endTime).toDurationMillis
      else
        0L
    } else
      (DateTime.now to startTime).toDurationMillis
      )

  def getProblems = Contests.getProblems(id)
  def getCompilers = Contests.getCompilers(id)
  def getClarifications = Contests.getClarifications(id)
}

object Contest {
  implicit val writes = new Writes[Contest] {
    def writes(c: Contest) = {
      import com.github.nscala_time.time.Imports._
      val now = DateTime.now

      def ntm(x: DateTime) =
        if (x >= now)
          (now to x).toDurationMillis
        else
          -((x to now).toDurationMillis)

      Json.obj(
        "id" -> c.id,
        "name" -> c.name,
        "schoolMode" -> c.schoolMode,
        "frozen" -> c.frozen,
        "startTime" -> c.startTime,
        "startTimeDelta" -> ntm(c.startTime),
        "endTime" -> c.endTime,
        "endTimeDelta" -> ntm(c.endTime),
        "freezeTime" -> c.freezeTime,
        "freezeTimeDelta" -> ntm(c.freezeTime),
        "exposeTime" -> c.exposeTime,
        "exposeTimeDelta" -> ntm(c.exposeTime)
      )
    }
  }
}

case class UserEvent(contest: Option[Int], team: Option[Int], event: Option[String], data: JsValue)
object UserEvent {
  implicit val eventDataExtractor = EventDataExtractor[UserEvent](x => Json.stringify(x.data))
  implicit val eventIdExtractor = EventNameExtractor[UserEvent](_.event)
}

case class SelectedContest(contest: Contest, contests: Seq[(Int, String)])

case class Problem(id: String, name: String, tests: Int, rating: Int)

case class Compiler(id: Int, name: String, ext: String)

object Compilers {
  def toSelect(compilers: Seq[Compiler]) =
    compilers.map(x => x.id.toString -> x.name)
}

object Contests {
  import slick.driver.MySQLDriver.api._

  def formatHMS(ms: Long) = {
    val s = ms / 1000
    val seconds = s % 60
    val m = s / 60
    val minutes = m % 60
    val hours = m / 60

    f"$hours%02d:$minutes%02d:$seconds%02d"
  }

  implicit val convertContests = GetResult(r => Contest(r.nextInt(), r.nextString(), r.nextBoolean(),
    new DateTime(r.nextTimestamp()), new DateTime(r.nextTimestamp()), new DateTime(r.nextTimestamp()),
    new DateTime(r.nextTimestamp())))

  val getContests =
    sql"""select ID, Name, SchoolMode, Start, End, Finish, Expose from Contests""".as[Contest]

  def getContest(contestId: Int) =
    sql"""select ID, Name, SchoolMode, Start, End, Finish, Expose from Contests where ID = $contestId""".as[Contest]

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

  implicit val getLocalTeam = GetResult(r =>
    LocalTeam(r.nextInt(), r.nextString(), r.nextIntOption(), r.nextString(), r.nextBoolean(),
      r.nextBoolean(), r.nextBoolean()))


  def getTeams(contest: Int) =
    sql"""select Participants.LocalID, Schools.Name, Teams.Num, Teams.Name, Participants.NotRated,
           Participants.NoPrint, Participants.Disabled from Participants, Schools, Teams where
         Participants.Contest = $contest and Teams.ID = Participants.Team and Schools.ID = Teams.School
       """.as[LocalTeam]
}

object Problems {
  def toSelect(problems: Seq[Problem]) =
    problems.map(x => x.id -> s"${x.id}. ${x.name}")
}

case class Clarification(time: DateTime, problem: String, text: String)
case class ClarificationRequest(time: DateTime, problem: String, text: String, answer: String)

