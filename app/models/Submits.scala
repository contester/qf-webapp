package models


import java.nio.charset.StandardCharsets
import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{Memory, TimeMs}
import play.api.Logging
import slick.jdbc.{GetResult, JdbcBackend}
import slick.lifted.MappedTo
import spire.math.Rational
import spire.math.extras.{FixedPoint, FixedScale}

import scala.concurrent.{ExecutionContext, Future}

case class Arrived(timestamp: DateTime, seconds: Int) {
  def asString = SecondsToTimeStr(seconds)
}

case class RatedProblem(id: String, rating: Int)

case class SubmitId(id: Int, arrived: Arrived, teamId: Int, contestId: Int, problem: RatedProblem, ext: String)

case class Submit(submitId: SubmitId, finished: Boolean, compiled: Boolean, passed: Int, taken: Int, testingId: Option[Int],
                  points: Option[Double] = None, maxPoints: Double = 0) {
  val success = finished && compiled && taken > 0 && passed == taken
  val failedOnFirst = taken == 1 && passed == 0
}

object SecondsToTimeStr {
  def apply(time: Int) = "%02d:%02d".format(time / 3600, (time / 60) % 60)
}

case class ResultEntry(testingID: Long, test: Int, result: Int, time: TimeMs, memory: Memory, info: Long, testerExitCode: Long,
                       group: Option[String] = None, points: Double = 0) {
  def resultString = SubmitResult.message(result)

  def resultString(messages: play.api.i18n.Messages): String = SubmitResult.statusMessage(result)(messages)

  def timeLimitExceeded = result == 11

  def success =
    if (test == 0) result == 1
    else result == 10
}

case class SubmitDetails(fsub: FullyDescribedSubmit, source: Array[Byte]) {
  def sourceStr = new String(source, StandardCharsets.UTF_8)
}

case class FullyDescribedSubmit(submit: Submit, index: Int, result: SubmitResult,
                                stats: SubmitStats, details: Seq[ResultEntry])

sealed trait SubmitError
case object RoundNotRunning extends SubmitError
case object UnknownProblem extends SubmitError
case object UnknownLanguage extends SubmitError
case object NotAdmitted extends SubmitError

object SubmitErrors {
  def key(e: SubmitError): String = e match {
    case RoundNotRunning => "submit.error.roundNotRunning"
    case UnknownProblem => "submit.error.unknownProblem"
    case UnknownLanguage => "submit.error.unknownLanguage"
    case NotAdmitted => "submit.error.notAdmitted"
  }

  def withError[T](form: play.api.data.Form[T], e: SubmitError): play.api.data.Form[T] = form.withGlobalError(key(e))
}

/** What a foreign key violation (SQLState 23503) on submits means to the contestant. */
object SubmitConstraints {
  def errorOf(constraint: String): Option[SubmitError] =
    if (constraint == "fk_submits_credential" || constraint == "fk_custom_tests_credential") Some(NotAdmitted)
    else if (constraint.startsWith("fk_submits_problem")) Some(UnknownProblem)
    else if (constraint.startsWith("fk_submits_language") || constraint.startsWith("fk_custom_tests_language")) Some(UnknownLanguage)
    else None

  def fromException(e: Throwable): Option[SubmitError] = e match {
    case p: org.postgresql.util.PSQLException if p.getSQLState == "23503" =>
      Option(p.getServerErrorMessage).flatMap(m => Option(m.getConstraint)).flatMap(errorOf)
    case _ => Option(e.getCause).filter(_ ne e).flatMap(fromException)
  }
}

object Submits {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel
  import org.stingray.contester.dbmodel.SlickModel.UpliftedSubmit

  def groupByTP(submits: Seq[Submit]) =
    submits.groupBy(x => (x.submitId.teamId, x.submitId.problem.id))
      .mapValues(_.sortBy(_.submitId.arrived.seconds))

  def indexGrouped[S](values: Seq[S]) =
    values.zipWithIndex.map(x => x._1 -> (x._2 + 1))

  def justIndexSubmits(submits: Seq[Submit]): Seq[(Submit, Int)] =
    groupByTP(submits)
      .mapValues(indexGrouped)
      .values.flatten.toSeq

  def upliftSub(s: UpliftedSubmit): Submit =
    Submit(
      SubmitId(s.id.toInt, Arrived(s.arrived, s.arrivedSeconds.toStandardSeconds.getSeconds),
        s.teamID, s.contestID, RatedProblem(s.problemID, 30), s.ext), s.finished,
      s.compiled, s.passed, s.taken,
      s.testingID.map(_.toInt), s.points, s.maxPoints)

  def getContestSubmits(contest: Int) = {
    SlickModel.submits0ByContest(contest).result
  }

  /** The contest of a submit; None if there is no such submit. */
  def contestOfSubmit(submitId: Int) =
    SlickModel.submits.filter(_.id === submitId.toLong).map(_.contest).result.headOption

  def getContestTeamSubmits(contest: Int, team: Int) = {
    SlickModel.submits0ByContestTeam(contest, team).result
  }

  def getContestTeamProblemSubmits(contest: Int, team: Int, problem: String) = {
    SlickModel.submits0ByContestTeamProblem(contest, team, problem).result
  }

  private def trOption(items: Seq[ResultEntry]): Option[Seq[ResultEntry]] =
    if (items.nonEmpty)
      Some(items)
    else
      None

  def getTestingStats(details: Seq[ResultEntry]) =
    trOption(details.filter(_.test != 0)).map { detlist =>
      SubmitStats(detlist.map(_.time).max, detlist.map(_.memory).max, detlist.exists(_.timeLimitExceeded))
    }.getOrElse(SubmitStats(TimeMs(0), Memory(0)))

  private[this] def annotateGrouped(db: JdbcBackend#DatabaseDef, submits: Seq[Submit])(implicit ec: ExecutionContext) = {
    val indexed = indexGrouped(submits)

    Future.sequence(
      indexed.map {
        case (submit, index) =>
          SubmitResult.annotate(db, submit).map {
            case (submitResult, submitDetails, submitStats) =>
              FullyDescribedSubmit(submit, index, submitResult, submitStats, submitDetails)
          }
      })
  }

  def groupAndAnnotate(db: JdbcBackend#DatabaseDef, submits: Seq[Submit])(implicit ec: ExecutionContext) =
    Future.sequence(groupByTP(submits).values.map { grouped =>
      annotateGrouped(db, grouped)
    }).map(_.flatten.toSeq.sortBy(-_.submit.submitId.arrived.seconds))


  /**
    * One team's attempts on one problem, scored in arrival order and numbered, with the results of their testings.
    * Pure, so a change to an earlier attempt (a rejudge) can be seen to change the scores of the later ones.
    */
  def describeAttempts(attempts: Seq[Submit], resultsByTesting: Map[Long, Seq[ResultEntry]]): Seq[FullyDescribedSubmit] = {
    val submits = attempts.sortBy(_.submitId.arrived.seconds)
    indexGrouped(submits).map {
      case (submit, index) =>
        val details = submit.testingId.flatMap(t => resultsByTesting.get(t.toLong)).getOrElse(Nil)
        FullyDescribedSubmit(submit, index, SubmitResult.annotate2(submit),
          getTestingStats(details), details)
    }
  }

  /**
    * A team's attempts on one problem, described like the rows of the team's submit list. Two queries however many
    * attempts there are: the submits, and the per-test results of all of them in one go (without the tester output).
    */
  def teamProblemSubmits(db: JdbcBackend#DatabaseDef, contest: Int, team: Int, problem: String)
                        (implicit ec: ExecutionContext): Future[Seq[FullyDescribedSubmit]] =
    db.run(getContestTeamProblemSubmits(contest, team, problem)).flatMap { rows =>
      val submits = rows.map(upliftSub)
      val testingIds = submits.flatMap(_.testingId.map(_.toLong)).distinct
      val results =
        if (testingIds.isEmpty) Future.successful(Map.empty[Long, Seq[ResultEntry]])
        else db.run(SlickModel.results.filter(_.testingID inSet testingIds).sortBy(_.testID)
          .map(x => (x.testingID, x.testID, x.resultCode, x.timeMs, x.memoryBytes)).result).map(_.map {
          case (tid, test, code, time, memory) => ResultEntry(tid, test, code, time, memory, 0, 0)
        }.groupBy(_.testingID))
      results.map(describeAttempts(submits, _))
    }

  import scala.language.implicitConversions

  implicit def o2f[A](o: Option[Future[A]])(implicit ec: ExecutionContext): Future[Option[A]] =
    o.map(_.map(Some(_))).getOrElse(Future.successful(None))

  def getSubmitById(db: JdbcBackend#DatabaseDef, submitId: Int)(implicit ec: ExecutionContext): Future[Option[SubmitDetails]] = {
    val pq = SlickModel.shortSubmitByID(submitId.toLong).result.headOption
    db.run(pq)
      .flatMap { maybeSubmit =>
        maybeSubmit map { short =>
          db.run(getContestTeamProblemSubmits(short._1, short._2, short._3))
            .flatMap(submits =>
              groupAndAnnotate(db, submits.map(upliftSub(_)))).map { submits =>
            submits.find(_.submit.submitId.id == submitId).map(SubmitDetails(_, short._4))
          }
        }
      }.map(_.flatten)
  }

  def loadSubmitDetails(db: JdbcBackend#DatabaseDef, testingId: Int)(implicit ec: ExecutionContext) =
    db.run(SlickModel.results0ByTesting(testingId.toLong).result).map(_.map(x =>
      ResultEntry(x.testingID, x.testID, x.resultCode, x.timeMs, x.memoryBytes,
        x.returnCode, x.testerReturnCode, x.groupName, x.points)
    ))

  /** The groups of a testing, as the per-test table wants them. */
  def loadGroups(db: JdbcBackend#DatabaseDef, testingId: Long)(implicit ec: ExecutionContext): Future[Seq[GroupRow]] =
    db.run(SlickModel.groupsByTesting(testingId).result).map(_.map(g =>
      GroupRow(g.groupName, g.policy, g.pointsEarned, g.pointsMax, g.passed, g.skipped)))

  /** The per-test table of a testing: its results and groups. What the submit pages call. */
  def loadTestRows(db: JdbcBackend#DatabaseDef, testingId: Long)(implicit ec: ExecutionContext): Future[Seq[TestRowView]] =
    loadSubmitDetails(db, testingId.toInt).zip(loadGroups(db, testingId)).map { case (details, groups) =>
      TestRows.describe(details.map(TestRows.fromEntry), groups)
    }

  /** Localized variant of `loadTestRows` for controllers. */
  def loadTestRows(db: JdbcBackend#DatabaseDef, testingId: Long, messages: play.api.i18n.Messages)
                  (implicit ec: ExecutionContext): Future[Seq[TestRowView]] =
    loadSubmitDetails(db, testingId.toInt).zip(loadGroups(db, testingId)).map { case (details, groups) =>
      TestRows.describe(details.map(TestRows.fromEntry), groups, messages)
    }

  /**
    * Inserts a submit for the team, after checking in this order: the round runs at `now` (read from the database,
    * not from the session), the problem is the round's, the language is the round season's, the credential is active.
    * A foreign key violation that gets past the checks (a race) maps to the matching error too. The director learns
    * of the row from the database trigger, so nothing is published here.
    */
  def insert(db: JdbcBackend#DatabaseDef, team: LoggedInTeam, problemId: String, languageId: Int, source: Array[Byte], remoteAddr: Option[String],
             now: DateTime)(implicit ec: ExecutionContext): Future[Either[SubmitError, Long]] = {
    val roundId = team.contest.id
    val participantId = team.team.id
    val addr = remoteAddr.flatMap { a =>
      val parsed = scala.util.Try(com.github.tminglei.slickpg.InetString(a)).toOption
      if (parsed.isEmpty) play.api.Logger("models.Submits").warn(s"unparsable remote address '$a' of team $participantId stored as NULL")
      parsed
    }
    val action: DBIO[Either[SubmitError, Long]] =
      admitted(team, languageId, now, Some(problemId)) { seasonId =>
        ((SlickModel.submits.map(x => (x.seasonId, x.contest, x.team, x.problem, x.language, x.source, x.arrived,
          x.remoteAddr, x.tested, x.compiled, x.passed, x.taken)) returning SlickModel.submits.map(_.id)) +=
          ((seasonId, roundId, participantId, problemId, languageId, source, now, addr, false, false, 0, 0)))
      }
    db.run(action).recover {
      case e => SubmitConstraints.fromException(e).map(x => Left(x): Either[SubmitError, Long]).getOrElse(throw e)
    }
  }

  /**
    * Runs `insert` (given the round's season) only if the round runs at `now`, the problem (when given) is the round's,
    * the language is the round season's and the team's credential is active; otherwise the first failed check.
    */
  private def admitted(team: LoggedInTeam, languageId: Int, now: DateTime, problemId: Option[String])(insert: Int => DBIO[Long])
                      (implicit ec: ExecutionContext): DBIO[Either[SubmitError, Long]] = {
    val roundId = team.contest.id
    val participantId = team.team.id
    def refuse(e: SubmitError): DBIO[Either[SubmitError, Long]] = DBIO.successful(Left(e))
    SlickModel.contestRounds.filter(_.id === roundId).map(r => (r.seasonId, r.startsAt, r.endsAt, r.isFinished)).result.headOption.flatMap {
      case Some((seasonId, Some(start), Some(end), false)) if !now.isBefore(start) && now.isBefore(end) =>
        for {
          problem <- problemId.fold[DBIO[Boolean]](DBIO.successful(true))(pid =>
            SlickModel.problems.filter(p => p.contestID === roundId && p.id === pid).exists.result)
          language <- SlickModel.compilers.filter(l => l.id === languageId && l.seasonId === seasonId).exists.result
          admitted <- SlickModel.roundCredentials.filter(c => c.roundId === roundId && c.participantId === participantId && c.isActive).exists.result
          r <- if (!problem) refuse(UnknownProblem)
            else if (!language) refuse(UnknownLanguage)
            else if (!admitted) refuse(NotAdmitted)
            else insert(seasonId).map(Right(_): Either[SubmitError, Long])
        } yield r
      case _ => refuse(RoundNotRunning)
    }
  }

  /**
    * Inserts a custom test (its insert trigger notifies the bridge) with the same checks as a submit, except the
    * problem: the round runs at `now`, the language is the round season's, the credential is active.
    */
  def insertCustomTest(db: JdbcBackend#DatabaseDef, team: LoggedInTeam, languageId: Int, source: Array[Byte], input: Array[Byte],
                       now: DateTime)(implicit ec: ExecutionContext): Future[Either[SubmitError, Long]] = {
    val action = admitted(team, languageId, now, None) { seasonId =>
      (SlickModel.customTests.map(x => (x.seasonId, x.contest, x.team, x.language, x.source, x.input)) returning SlickModel.customTests.map(_.id)) +=
        ((seasonId, team.contest.id, team.team.id, languageId, source, input))
    }
    db.run(action).recover {
      case e => SubmitConstraints.fromException(e).map(x => Left(x): Either[SubmitError, Long]).getOrElse(throw e)
    }
  }
}
