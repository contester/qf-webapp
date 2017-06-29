package models

import akka.actor.ActorRef
import com.github.nscala_time.time.Imports._
import org.stingray.qf.actors.ProblemStateActor
import play.api.libs.EventSource.{EventDataExtractor, EventIdExtractor, EventNameExtractor}
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.{JsValue, Json, Writes}
import slick.jdbc.GetResult

import scala.concurrent.Future

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

  implicit val eventNameExtractor = EventNameExtractor[Contest](_ => Some("contest"))
  implicit val eventDataExtractor = EventDataExtractor[Contest] { c =>
    Json.stringify(Json.toJson(c))
  }
}

case class SelectedContest(contest: Contest, contests: Seq[(Int, String)])

case class Problem(id: String, name: String, tests: Int, rating: Int)

class ProblemClient(problemStateActor: ActorRef) {
  import akka.pattern.ask
  def getProblems(contest: Int)(implicit timeout: akka.util.Timeout): Future[Map[Int, Problem]] =
    (problemStateActor ? ProblemStateActor.GetProblems(contest)).mapTo[Map[Int, Problem]]
}

case class Compiler(id: Int, name: String, ext: String)

object Compilers {
  def toSelect(compilers: Seq[Compiler]) =
    Seq(("", "Выберите компилятор")) ++ compilers.sortBy(_.name).map(x => x.id.toString -> x.name)
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
}

object Problems {
  def toSelect(problems: Seq[Problem]) =
    problems.map(x => x.id -> s"${x.id}. ${x.name}")
}

