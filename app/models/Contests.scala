package models

import akka.actor.ActorRef
import com.github.nscala_time.time.Imports._
import org.stingray.qf.actors.ProblemStateActor
import play.api.libs.EventSource.{EventDataExtractor, EventIdExtractor, EventNameExtractor}
import play.api.libs.json.{JsValue, Json, Writes}
import slick.jdbc.GetResult
import utils.Selectable

import scala.concurrent.Future

case class Contest(id: Int, name: String, startTime: DateTime, freezeTime: DateTime,
                   endTime: DateTime, exposeTime: DateTime, polygonID: String, language: String, schoolMode: Boolean) {
  def frozen = {
    val now = DateTime.now
    now >= freezeTime && now < exposeTime
  }
  def finished = DateTime.now >= endTime
  def started = DateTime.now >= startTime
  def running = started && !finished

  def timevalHMS =
    Contests.formatHMS(if (started) {
      if (!finished)
        (DateTime.now to endTime).toDurationMillis
      else
        0L
    } else
      (DateTime.now to startTime).toDurationMillis
      )

  def getCompilers = Contests.getCompilers(id)
}

object Contest {
  def tupled = (Contest.apply _).tupled
  implicit val writes = new Writes[Contest] {
    override def writes(c: Contest): JsValue = {
      val now = DateTime.now

      def ntm(x: DateTime) =
        if (x >= now)
          (now to x).toDurationMillis
        else
          -((x to now).toDurationMillis)

      import play.api.libs.json.JodaWrites._

      Json.obj(
        "id" -> c.id,
        "name" -> c.name,
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

case class Problem(contest: Int, id: String, name: String, tests: Int)

class ProblemClient(problemStateActor: ActorRef) {
  import akka.pattern.ask
  def getProblems(contest: Int)(implicit timeout: akka.util.Timeout): Future[Seq[Problem]] =
    (problemStateActor ? ProblemStateActor.GetProblems(contest)).mapTo[Seq[Problem]]
}

object Compilers {
  def toSelect(compilers: Seq[Compiler]) =
    compilers.sortBy(_.name).map(x => x.id.toString -> x.name)

  def forForm(compilers: Seq[Compiler]) =
    Selectable.forSelect(toSelect(compilers), "Select compiler")
}

object Contests {
  import utils.MyPostgresProfile.api._

  def formatHMS(ms: Long) = {
    val s = ms / 1000
    val seconds = s % 60
    val m = s / 60
    val minutes = m % 60
    val hours = m / 60

    f"$hours%02d:$minutes%02d:$seconds%02d"
  }

  def getCompilers(contest: Int) =
    SlickModel.sortedCompilers
    //SlickModel.compilers.filter(_.contest === contest).sortBy(_.id)
}

object Problems {
  def toSelect(problems: Seq[Problem]) =
    problems.map(x => x.id -> s"${x.id}. ${x.name}")

  def forForm(problems: Seq[Problem]) =
    Selectable.forSelect(toSelect(problems), "Select problem")
}

