package models

import akka.actor.ActorRef
import com.github.nscala_time.time.Imports._
import org.stingray.contester.dbmodel.{Contest, Problem, SlickModel, Team}
import org.stingray.qf.actors.ProblemStateActor
import play.api.libs.EventSource.{EventDataExtractor, EventIdExtractor, EventNameExtractor}
import play.api.libs.json.{JsValue, Json, Writes}
import slick.jdbc.JdbcBackend
import utils.Selectable

import scala.concurrent.{ExecutionContext, Future}

object ContestWrites {
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
        "startTime" -> c.startTime,
        "startTimeDelta" -> ntm(c.startTime),
        "endTime" -> c.endTime,
        "endTimeDelta" -> ntm(c.endTime)
      )
    }
  }

  implicit val eventNameExtractor = EventNameExtractor[Contest](_ => Some("contest"))
  implicit val eventDataExtractor = EventDataExtractor[Contest] { c =>
    Json.stringify(Json.toJson(c))
  }
}

case class SelectedContest(contest: Contest, contests: Seq[(Int, String)], canModify: Boolean = true, isSuperAdmin: Boolean = false)

/** There is no current round to select: the admin pages say so instead of failing. */
case object NoCurrentRounds extends Exception("there are no current rounds") with scala.util.control.NoStackTrace

object SelectedContest {
  /**
    * The round `contestId` among the rounds the admin may spectate, or the first of them when it is not one; None when
    * there are none at all.
    */
  def choose(all: Seq[Contest], contestId: Int, account: Admin): Option[SelectedContest] = {
    val contests = all.filter(c => account.canSpectate(c.id)).sortBy(_.id)
    contests.find(_.id == contestId).orElse(contests.headOption).map { selected =>
      SelectedContest(selected, contests.map(x => x.id -> x.name), account.canModify(selected.id), account.isSuperAdmin)
    }
  }
}

class ProblemClient(problemStateActor: ActorRef) {
  import akka.pattern.ask
  def getProblems(contest: Int)(implicit timeout: akka.util.Timeout): Future[Seq[Problem]] =
    (problemStateActor ? ProblemStateActor.GetProblems(contest)).mapTo[Seq[Problem]]
}

object Compilers {
  import org.stingray.contester.dbmodel.Compiler

  def toSelect(compilers: Seq[Compiler]) =
    compilers.sortBy(_.name).map(x => x.id.toString -> x.name)

  def forForm(compilers: Seq[Compiler])(implicit messages: play.api.i18n.Messages) =
    Selectable.forSelect(toSelect(compilers), messages("label.selectCompiler"))
}

/** Rounds of the current season (qf's contests) and their participants (qf's teams). */
object Rounds {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._

  def roundName(contestName: String, roundName: String): String = SlickModel.roundName(contestName, roundName)

  /** Rounds of the current season with both start and end set, by id. */
  def currentRounds(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Seq[Contest]] =
    db.run(SlickModel.currentRounds.result).map(_.map(SlickModel.toContest))

  /** A round of the current season with both times set. */
  def currentRound(db: JdbcBackend#DatabaseDef, roundId: Int)(implicit ec: ExecutionContext): Future[Option[Contest]] =
    db.run(SlickModel.currentRoundByID(roundId).result.headOption).map(_.map(SlickModel.toContest))

  /**
    * The participants of the round: those with an active credential for it, not soft-deleted, with their school.
    */
  def teamsOfRound(db: JdbcBackend#DatabaseDef, roundId: Int)(implicit ec: ExecutionContext): Future[Seq[Team]] =
    db.run(SlickModel.teamsOfRound(roundId).result)

  /** The participants of every current round, by round id. */
  def teamsOfCurrentRounds(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Map[Int, Seq[Team]]] =
    db.run(SlickModel.teamsOfCurrentRounds.result).map(_.groupBy(_._1).map { case (k, v) => k -> v.map(_._2) })
}

object Contests {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._

  def formatHMS(ms: Long) = {
    val s = ms / 1000
    val seconds = s % 60
    val m = s / 60
    val minutes = m % 60
    val hours = m / 60

    f"$hours%02d:$minutes%02d:$seconds%02d"
  }

  /** The languages of the round's season. */
  def getCompilers(contest: Contest) =
    SlickModel.compilersOfSeason(contest.seasonId)
}

object Problems {
  def toSelect(problems: Seq[Problem]) =
    problems.map(x => x.id -> s"${x.id}. ${x.name}")

  def forForm(problems: Seq[Problem])(implicit messages: play.api.i18n.Messages) =
    Selectable.forSelect(toSelect(problems), messages("label.selectProblem"))
}

