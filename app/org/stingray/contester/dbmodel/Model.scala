package org.stingray.contester.dbmodel

import java.nio.charset.Charset

import com.github.nscala_time.time.Imports._
import com.github.tminglei.slickpg.InetString
import play.api.libs.json.{JsValue, Json}
import slick.lifted.MappedTo

// The tables of the teacher-web schema (database `public`) that qf reads and writes. A qf "contest" is a round
// (contest_rounds.id); a team is a participant. Everything per round also has the round's season_id.

case class Clarification(id: Option[Long], contest: Int, problem: String, text: String, arrived: DateTime, hidden: Boolean)

case class MaxSeen(contest: Int, team: Int, timestamp: DateTime)

case class ClarificationRequest(id: Long, contest: Int, team: Int, problem: String, request: String,
                                answer: String, arrived: DateTime, answered: Boolean) {
  def getAnswer = if (answered) {
    if (answer.isEmpty) "No comments" else answer
  } else "..."
}

// TODO(task11): qf's own admin table; admins move to teacher-web's `admins`.
case class AdminEntry(username: String, password: String, spectator: String, administrator: String,
                      locations: String, unrestricted: String)

/** A language of the season (season_languages); `ext` is the module id. */
case class Compiler(id: Int, name: String, ext: String)

case class Message2(id: Option[Long], contest: Int, team: Int, kind: String, data: JsValue, seen: Boolean)

case class Extrainfo(contest: Int, num: Int, heading: String, data: String)

/**
  * A round of the current season, with both times set. `id` is contest_rounds.id, the "contest id" of qf's URLs.
  * Start and end are also the freeze and expose times: there is no freeze configuration.
  */
case class Contest(id: Int, name: String, startTime: DateTime, endTime: DateTime, polygonId: Option[String], seasonId: Int) {
  // Kept so that the legacy board code compiles; the scoreboard rewrite drops them.
  def freezeTime: DateTime = startTime
  def exposeTime: DateTime = endTime
  def language: String = "russian"

  def frozen = {
    val now = DateTime.now
    freezeTime.isBefore(endTime) && now >= freezeTime && now < exposeTime
  }
  def finished = DateTime.now >= endTime
  def started = DateTime.now >= startTime
  def running = started && !finished

  private[this] def timeval = if (started) {
    if (!finished)
      (DateTime.now to endTime).toDurationMillis
    else
      0L
  } else
    (DateTime.now to startTime).toDurationMillis

  private[this] def formatHMS(ms: Long) = {
    val s = ms / 1000
    val seconds = s % 60
    val m = s / 60
    val minutes = m % 60
    val hours = m / 60

    f"$hours%02d:$minutes%02d:$seconds%02d"
  }

  def timevalHMS =
    formatHMS(timeval)
}

/** round_credentials.unrated: None is a rated participant. */
sealed trait Unrated
/** Listed on the board, but without a place. */
case object NotRated extends Unrated
/** Hidden from the board. */
case object Disqualified extends Unrated

object Unrated {
  def toDb(u: Unrated): String = u match {
    case NotRated => "not_rated"
    case Disqualified => "disqualified"
  }

  def fromDb(s: String): Unrated = s match {
    case "not_rated" => NotRated
    case "disqualified" => Disqualified
    case other => throw new IllegalArgumentException(s"unknown credential_unrated value $other")
  }
}

/**
  * A participant of a round. `number` is participants.season_seq, `name` the full name; school and city come from
  * the participant's school.
  */
case class Team(participantId: Int, number: Int, name: String, school: String, city: String, unrated: Option[Unrated]) {
  def id: Int = participantId

  /** Out of competition. Always true for a disqualified participant. */
  def notRated: Boolean = unrated.isDefined
  def disqualified: Boolean = unrated.contains(Disqualified)
  /** Not on any board, and scores nothing. */
  def disabled: Boolean = disqualified

  def schoolWithCity: String = s"$school, $city"
  def teamFullName: String = s"$number. $name ($schoolWithCity)"

  // Legacy accessors for the board views, until the scoreboard rewrite.
  def teamNum: Int = number
  def teamName: String = name
  def schoolFullName: String = school
  def schoolShortName: String = school
  def schoolFullNameWithNum: String = s"$number. $schoolWithCity"
}

case class ContestTeamIds(contestId: Int, teamId: Int)

case class TimeMs(underlying: Long) extends AnyVal with MappedTo[Long] {
  override def toString: String = s"${underlying}"

  override def value: Long = underlying
}

object TimeMs {
  implicit val ordering: Ordering[TimeMs] = Ordering.by(_.underlying)
}

case class Memory(underlying: Long) extends AnyVal with MappedTo[Long] {
  override def toString: String = s"${underlying / 1024}"

  override def value: Long = underlying
}

object Memory {
  implicit val ordering: Ordering[Memory] = Ordering.by(_.underlying)
}

case class Problem(contest: Int, id: String, name: String, tests: Int, maxPoints: Double = 0)

case class EvalEntry(id: Long, contest: Int, team: Int, languageName: String, source: Array[Byte], input: Array[Byte],
                     output: Array[Byte], arrived: DateTime, finishTime: Option[DateTime], resultCode: Int,
                     timex: TimeMs, memory: Memory, returnCode: Long) {
  private[this] def trimStr(s: Array[Byte]) = {
    val x = new String(s, Charset.forName("CP1251"))
    if (x.length > 1024) {
      x.substring(0, 1024) + "\n..."
    } else x
  }

  def sourceStr = new String(source, "CP1251")
  def inputStr = trimStr(input)
  def outputStr = trimStr(output)

}

// TODO(task19): printing.
case class Area(id: Int, name: String, printer: String)

/** One group of a testing (testing_groups). `policy` is `complete-group` or `each-test`. */
case class TestingGroup(testingId: Long, groupName: String, policy: String, pointsEarned: Double, pointsMax: Double,
                        passed: Boolean, skipped: Boolean)

object SlickModel {
  import MyPostgresProfile.api._

  // ---- teacher-web's own tables, read only ----

  case class Seasons(tag: Tag) extends Table[(Int, Boolean)](tag, "seasons") {
    def id = column[Int]("id", O.PrimaryKey)
    def isCurrent = column[Boolean]("is_current")

    override def * = (id, isCurrent)
  }

  val seasons = TableQuery[Seasons]

  case class TwContests(tag: Tag) extends Table[(Int, Int, String)](tag, "contests") {
    def id = column[Int]("id", O.PrimaryKey)
    def seasonId = column[Int]("season_id")
    def name = column[String]("name")

    override def * = (id, seasonId, name)
  }

  val twContests = TableQuery[TwContests]

  case class ContestRound(id: Int, contestId: Int, seasonId: Int, name: String, startsAt: Option[DateTime],
                          endsAt: Option[DateTime], isFinished: Boolean, testingSystemId: Option[Int])

  case class ContestRounds(tag: Tag) extends Table[ContestRound](tag, "contest_rounds") {
    def id = column[Int]("id", O.PrimaryKey)
    def contestId = column[Int]("contest_id")
    def seasonId = column[Int]("season_id")
    def name = column[String]("name")
    def startsAt = column[Option[DateTime]]("starts_at")
    def endsAt = column[Option[DateTime]]("ends_at")
    def isFinished = column[Boolean]("is_finished")
    def testingSystemId = column[Option[Int]]("testing_system_id")

    override def * = (id, contestId, seasonId, name, startsAt, endsAt, isFinished, testingSystemId) <>
      (ContestRound.tupled, ContestRound.unapply)
  }

  val contestRounds = TableQuery[ContestRounds]

  case class Schools(tag: Tag) extends Table[(Int, String, String)](tag, "schools") {
    def id = column[Int]("id", O.PrimaryKey)
    def name = column[String]("name")
    def city = column[String]("city")

    override def * = (id, name, city)
  }

  val schools = TableQuery[Schools]

  case class Participants(tag: Tag) extends Table[(Int, Int, Int, Int, String, Int, Boolean)](tag, "participants") {
    def id = column[Int]("id", O.PrimaryKey)
    def seasonId = column[Int]("season_id")
    def contestId = column[Int]("contest_id")
    def seasonSeq = column[Int]("season_seq")
    def fullName = column[String]("full_name")
    def schoolId = column[Int]("school_id")
    def isDeleted = column[Boolean]("is_deleted")

    override def * = (id, seasonId, contestId, seasonSeq, fullName, schoolId, isDeleted)
  }

  val participants = TableQuery[Participants]

  /** Read only, except `unrated`, which qf admins write. */
  case class RoundCredentials(tag: Tag) extends Table[(Int, Int, Int, Int, Boolean, String, String, Option[Unrated])](tag, "round_credentials") {
    def id = column[Int]("id", O.PrimaryKey)
    def roundId = column[Int]("round_id")
    def contestId = column[Int]("contest_id")
    def participantId = column[Int]("participant_id")
    def isActive = column[Boolean]("is_active")
    def login = column[String]("login")
    def password = column[String]("password")
    def unrated = column[Option[Unrated]]("unrated")

    override def * = (id, roundId, contestId, participantId, isActive, login, password, unrated)
  }

  val roundCredentials = TableQuery[RoundCredentials]

  // ---- rounds as qf contests ----

  /** The name of a round in qf: the contest's name, and the round's unless it adds nothing. */
  def roundName(contestName: String, roundName: String): String = {
    val r = roundName.trim
    if (r.isEmpty || r == contestName.trim) contestName else s"$contestName — $r"
  }

  /** The times are never None: the query keeps only rounds that have both. */
  type RoundRow = (Int, String, String, Option[DateTime], Option[DateTime], Option[String], Int)

  def toContest(r: RoundRow): Contest = r match {
    case (id, contestName, name, start, end, polygonId, seasonId) =>
      Contest(id, roundName(contestName, name), start.get, end.get, polygonId, seasonId)
  }

  /** Rounds of the current season with both times set: the only rounds qf knows. */
  private[this] val currentRoundRows = for {
    r <- contestRounds if r.startsAt.isDefined && r.endsAt.isDefined
    c <- twContests if c.id === r.contestId
    s <- seasons if s.id === r.seasonId && s.isCurrent
  } yield (r.id, c.name, r.name, r.startsAt, r.endsAt, r.testingSystemId.asColumnOf[Option[String]], r.seasonId)

  val currentRounds = Compiled(currentRoundRows.sortBy(_._1))

  val currentRoundByID = Compiled((roundID: Rep[Int]) => currentRoundRows.filter(_._1 === roundID).take(1))

  /** The season of a round, for the season_id of the rows qf inserts. Fails if there is no such round. */
  def seasonOfRound(roundID: Int): DBIO[Int] =
    contestRounds.filter(_.id === roundID).map(_.seasonId).result.head

  // ---- participants as qf teams ----

  case class LiftedTeam(participantId: Rep[Int], number: Rep[Int], name: Rep[String], school: Rep[String],
                        city: Rep[String], unrated: Rep[Option[Unrated]])

  implicit object TeamShape extends CaseClassShape(LiftedTeam.tupled, (Team.apply _).tupled)

  /** Participants with an active credential for a round, not soft-deleted, with the credential. */
  val roundTeams = for {
    rc <- roundCredentials if rc.isActive
    p <- participants if p.id === rc.participantId && !p.isDeleted
    s <- schools if s.id === p.schoolId
  } yield (rc, LiftedTeam(p.id, p.seasonSeq, p.fullName, s.name, s.city, rc.unrated))

  val teamsOfRound = Compiled((roundID: Rep[Int]) => roundTeams.filter(_._1.roundId === roundID).map(_._2))

  /** The teams of every current round, by round. */
  val teamsOfCurrentRounds = for {
    (rc, t) <- roundTeams
    r <- currentRoundRows if r._1 === rc.roundId
  } yield (rc.roundId, t)

  case class LoggedInTeam0(username: String, password: String, contest: Contest, team: Team)

  /** (login, password, round, team) of the active credentials of current rounds. */
  val loginQuery = for {
    (rc, t) <- roundTeams
    r <- currentRoundRows if r._1 === rc.roundId
  } yield (rc.login, rc.password, r, t)

  def toLoggedInTeam0(x: (String, String, RoundRow, Team)): LoggedInTeam0 =
    LoggedInTeam0(x._1, x._2, toContest(x._3), x._4)

  // ---- contest data, per round ----

  case class Clarifications(tag: Tag) extends Table[Clarification](tag, "clarifications") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def seasonId = column[Int]("season_id")
    def contest = column[Int]("round_id")
    def problem = column[String]("problem")
    def text = column[String]("text")
    def arrived = column[DateTime]("posted")
    def hidden = column[Boolean]("hidden")

    override def * = (id.?, contest, problem, text, arrived, hidden) <> (Clarification.tupled, Clarification.unapply)
  }

  val clarifications = TableQuery[Clarifications]

  /** Inserts a new clarification (its id is ignored) with its round's season; returns it with the new id. */
  def insertClarification(cl: Clarification)(implicit ec: scala.concurrent.ExecutionContext): DBIO[Clarification] =
    seasonOfRound(cl.contest).flatMap { season =>
      (clarifications.map(x => (x.seasonId, x.contest, x.problem, x.text, x.arrived, x.hidden))
        returning clarifications.map(_.id)) += ((season, cl.contest, cl.problem, cl.text, cl.arrived, cl.hidden))
    }.map(id => cl.copy(id = Some(id)))

  val getClarificationsForContest = Compiled((contestId: Rep[Int]) =>
    clarifications.filter(_.contest === contestId).sortBy(_.arrived.desc))

  val clarificationByID = Compiled((clarificationID: Rep[Long]) =>
    clarifications.filter(_.id === clarificationID).take(1)
  )

  case class ClarificationRequests(tag: Tag) extends Table[ClarificationRequest](tag, "clarification_requests") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def seasonId = column[Int]("season_id")
    def contest = column[Int]("round_id")
    def team = column[Int]("participant_id")
    def problem = column[String]("problem")
    def request = column[String]("request")
    def answer = column[String]("answer")
    def arrived = column[DateTime]("arrived")
    def answered = column[Boolean]("answered")

    override def * = (id, contest, team, problem, request, answer, arrived, answered) <>
      (ClarificationRequest.tupled, ClarificationRequest.unapply)
  }

  val clarificationRequests = TableQuery[ClarificationRequests]

  val clarificationRequestsUnanswered = for {
    c <- clarificationRequests.filter(!_.answered)
  } yield (c.contest, c.id)

  case class ClrSeen2(tag: Tag) extends Table[MaxSeen](tag, "clarifications_seen") {
    def seasonId = column[Int]("season_id")
    def contest = column[Int]("round_id", O.PrimaryKey)
    def team = column[Int]("participant_id", O.PrimaryKey)
    def timestamp = column[DateTime]("max_seen")

    override def * = (contest, team, timestamp) <> (MaxSeen.tupled, MaxSeen.unapply)
  }

  val clrSeen2 = TableQuery[ClrSeen2]

  /** Records the newest clarification a participant has seen, with the round's season. */
  def upsertMaxSeen(m: MaxSeen): DBIO[Int] = {
    val ts = new java.sql.Timestamp(m.timestamp.getMillis)
    sqlu"""insert into clarifications_seen (season_id, round_id, participant_id, max_seen)
           select season_id, id, ${m.team}, $ts from contest_rounds where id = ${m.contest}
           on conflict (round_id, participant_id) do update set max_seen = excluded.max_seen"""
  }

  case class Compilers(tag: Tag) extends Table[Compiler](tag, "season_languages") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def seasonId = column[Int]("season_id")
    def name = column[String]("name")
    def moduleID = column[String]("module_id")

    def * = (id, name, moduleID) <> (Compiler.tupled, Compiler.unapply)
  }

  val compilers = TableQuery[Compilers]

  /** The languages of a season, by name. */
  val compilersOfSeason = Compiled((seasonId: Rep[Int]) => compilers.filter(_.seasonId === seasonId).sortBy(_.name))

  case class ExtraInfos(tag: Tag) extends Table[Extrainfo](tag, "extra_info") {
    def seasonId = column[Int]("season_id")
    def contest = column[Int]("round_id")
    def num = column[Int]("num")
    def heading = column[String]("heading")
    def data = column[String]("data")

    override def * = (contest, num, heading, data) <> (Extrainfo.tupled, Extrainfo.unapply)
  }

  val extraInfos = TableQuery[ExtraInfos]

  def insertExtraInfo(e: Extrainfo)(implicit ec: scala.concurrent.ExecutionContext): DBIO[Int] =
    seasonOfRound(e.contest).flatMap { season =>
      extraInfos.map(x => (x.seasonId, x.contest, x.num, x.heading, x.data)) += ((season, e.contest, e.num, e.heading, e.data))
    }

  case class Messages2(tag: Tag) extends Table[Message2](tag, "messages") {
    def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
    def seasonId = column[Int]("season_id")
    def contest = column[Int]("round_id")
    def team = column[Int]("participant_id")
    def kind = column[String]("kind")
    def value = column[String]("value")
    def seen = column[Boolean]("seen")

    override def * = (id.?, contest, team, kind, value, seen) <> (
      (x: (Option[Long], Int, Int, String, String, Boolean)) => Message2(x._1, x._2, x._3, x._4, Json.parse(x._5), x._6),
      (m: Message2) => Some((m.id, m.contest, m.team, m.kind, Json.stringify(m.data), m.seen)))
  }

  private[this] val messages2 = TableQuery[Messages2]

  val allMessages = Compiled(messages2.map(identity))

  /** Inserts a message (its id is ignored) with its round's season; returns it with the new id. */
  def addMessage(m: Message2)(implicit ec: scala.concurrent.ExecutionContext): DBIO[Message2] =
    seasonOfRound(m.contest).flatMap { season =>
      (messages2.map(x => (x.seasonId, x.contest, x.team, x.kind, x.value, x.seen)) returning messages2.map(_.id)) +=
        ((season, m.contest, m.team, m.kind, Json.stringify(m.data), m.seen))
    }.map(id => m.copy(id = Some(id)))

  val messages2Unseen = Compiled(messages2.filterNot(_.seen))

  val messageSeenByID = Compiled((id: Rep[Long]) => messages2.filter(_.id === id).map(_.seen))

  case class Submits(tag: Tag) extends Table[(Long, Int, Int, String, Int, Array[Byte], DateTime, Boolean, Int, Option[Long], Int, Boolean, Option[Double])](tag, "submits") {
    def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
    def seasonId = column[Int]("season_id")
    def contest = column[Int]("round_id")
    def team = column[Int]("participant_id")
    def problem = column[String]("problem_id")
    def language = column[Int]("language_id")
    def source = column[Array[Byte]]("source")
    def arrived = column[DateTime]("submitted_at")
    def remoteAddr = column[Option[InetString]]("remote_addr")
    def tested = column[Boolean]("tested")
    def passed = column[Int]("passed")
    def testingID = column[Option[Long]]("testing_id")
    def taken = column[Int]("taken")
    def compiled = column[Boolean]("compiled")
    def points = column[Option[Double]]("points")

    override def * = (id, contest, team, problem, language, source, arrived, tested, passed, testingID, taken, compiled, points)
  }

  val submits = TableQuery[Submits]

  case class LiftedContestTeamID(contest: Rep[Int], team: Rep[Int])

  implicit object ContestTeamIDShape extends CaseClassShape(LiftedContestTeamID.tupled, ContestTeamIds.tupled)

  def getContestTeamIDs(submitID: Long) =
    submits.filter(_.id === submitID).map(x => LiftedContestTeamID(x.contest, x.team)).take(1)

  case class Problems(tag: Tag) extends Table[Problem](tag, "problems") {
    def seasonId = column[Int]("season_id")
    def contestID = column[Int]("round_id")
    def id = column[String]("problem_id")
    def tests = column[Int]("tests")
    def name = column[String]("name")
    def maxPoints = column[Double]("max_points")

    def pk = primaryKey("problems_pkey", (contestID, id))

    def * = (contestID, id, name, tests, maxPoints) <> (Problem.tupled, Problem.unapply)
  }

  val problems = TableQuery[Problems]

  case class LiftedSubmit(id: Rep[Long], arrived: Rep[DateTime], arrivedSeconds: Rep[Period], afterFreeze: Rep[Boolean],
                          teamID: Rep[Int], contestID: Rep[Int], problemID: Rep[String], ext: Rep[String],
                          finished: Rep[Boolean], compiled: Rep[Boolean], passed: Rep[Int], taken: Rep[Int],
                          testingID: Rep[Option[Long]])

  case class UpliftedSubmit(id: Long, arrived: DateTime, arrivedSeconds: Period, afterFreeze: Boolean, teamID: Int,
                            contestID: Int, problemID: String, ext: String, finished: Boolean, compiled: Boolean,
                            passed: Int, taken: Int, testingID: Option[Long])

  implicit object SubmitShape extends CaseClassShape(LiftedSubmit.tupled, UpliftedSubmit.tupled)

  /** Submits with their round's start (arrival and "after freeze" are relative to it) and language, unsorted. */
  val submitsUnsorted = for {
    s <- submits
    round <- contestRounds if s.contest === round.id
    problem <- problems if s.problem === problem.id && s.contest === problem.contestID
    lang <- compilers if s.language === lang.id
  } yield LiftedSubmit(s.id, s.arrived, s.arrived - round.startsAt.ifNull(s.arrived), s.arrived > round.startsAt.ifNull(s.arrived),
    s.team, s.contest, s.problem, lang.moduleID, s.tested, s.compiled, s.passed, s.taken, s.testingID)

  val submits0 = submitsUnsorted.sortBy(_.arrived)

  val submits0ByContest= Compiled((contestID: Rep[Int]) =>
    submits0.filter(_.contestID === contestID))

  val submits0ByContestTeam = Compiled((contestID: Rep[Int], teamID: Rep[Int]) =>
    submits0.filter(x => (x.contestID === contestID && x.teamID === teamID)))

  val submits0ByContestTeamProblem = Compiled((contestID: Rep[Int], teamID: Rep[Int], problemID: Rep[String]) =>
    submits0.filter(x => (x.contestID === contestID && x.teamID === teamID && x.problemID === problemID)))

  val shortSubmitByID = Compiled((submitID: Rep[Long]) =>
    submits.filter(_.id === submitID).map(x => (x.contest, x.team, x.problem, x.source)).take(1)
  )

  val results0ByTesting = Compiled((testingID: Rep[Long]) =>
    results0.filter(_.testingID === testingID)
  )

  val testingByID = Compiled((testingID: Rep[Long]) =>
    testings.filter(_.id === testingID).take(1)
  )

  /** (id, submit, started, problem revision, finished). */
  case class Testings(tag: Tag) extends Table[(Long, Long, DateTime, Option[Int], Option[DateTime])](tag, "testings") {
    def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
    def submit = column[Long]("submit_id")
    def startTime = column[DateTime]("started_at")
    def problemRevision = column[Option[Int]]("problem_revision")
    def finishTime = column[Option[DateTime]]("finished_at")

    override def * = (id, submit, startTime, problemRevision, finishTime)
  }

  val testings = TableQuery[Testings]

  case class TestingGroups(tag: Tag) extends Table[TestingGroup](tag, "testing_groups") {
    def testingId = column[Long]("testing_id")
    def groupName = column[String]("group_name")
    def policy = column[String]("policy")
    def pointsEarned = column[Double]("points_earned")
    def pointsMax = column[Double]("points_max")
    def passed = column[Boolean]("passed")
    def skipped = column[Boolean]("skipped")

    def pk = primaryKey("testing_groups_pkey", (testingId, groupName))

    override def * = (testingId, groupName, policy, pointsEarned, pointsMax, passed, skipped) <>
      (TestingGroup.tupled, TestingGroup.unapply)
  }

  val testingGroups = TableQuery[TestingGroups]

  case class CustomTests(tag: Tag) extends Table[(Long, Int, Int, Boolean, Int, Array[Byte], Array[Byte], Array[Byte], DateTime,
    Option[DateTime], Int, TimeMs, Memory, Long)](tag, "custom_tests") {
    def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
    def seasonId = column[Int]("season_id")
    def contest = column[Int]("round_id")
    def team = column[Int]("participant_id")
    def tested = column[Boolean]("tested")
    def language = column[Int]("language_id")
    def source = column[Array[Byte]]("source")
    def input = column[Array[Byte]]("input")
    def output = column[Array[Byte]]("output")
    def arrived = column[DateTime]("submit_time_absolute")
    // finished_at, not the legacy finish_time column (which nothing writes any more).
    def finishTime = column[Option[DateTime]]("finished_at")
    def resultCode = column[Int]("result_code")
    def timeMs = column[TimeMs]("time_ms")
    def memoryBytes = column[Memory]("memory_bytes")
    def returnCode = column[Long]("return_code")

    override def * = (id, contest, team, tested, language, source, input, output, arrived,
      finishTime, resultCode, timeMs, memoryBytes, returnCode)
  }

  val customTests = TableQuery[CustomTests]

  case class LiftedCustomTest(id: Rep[Long], contest: Rep[Int], team: Rep[Int], languageName: Rep[String],
                              source: Rep[Array[Byte]], input: Rep[Array[Byte]], output: Rep[Array[Byte]],
                              arrived: Rep[DateTime], finishTime: Rep[Option[DateTime]], resultCode: Rep[Int],
                              timeMs: Rep[TimeMs], memoryBytes: Rep[Memory], returnCode: Rep[Long])

  implicit object CustomEntryShape extends CaseClassShape(LiftedCustomTest.tupled, EvalEntry.tupled)

  val customTestWithLang = (for {
    c <- customTests
    l <- compilers if l.id === c.language
  } yield LiftedCustomTest(c.id, c.contest, c.team, l.name, c.source, c.input, c.output, c.arrived, c.finishTime,
    c.resultCode, c.timeMs, c.memoryBytes, c.returnCode)).sortBy(_.arrived.desc)

  case class Results(tag: Tag) extends Table[DBResultEntry](tag, "results") {
    def testingID = column[Long]("testing_id")
    def testID = column[Int]("test_no")
    def groupName = column[Option[String]]("group_name")
    def resultCode = column[Int]("result_code")
    def points = column[Double]("points")
    def recordTime = column[DateTime]("recorded_at")
    def timeMs = column[TimeMs]("time_ms")
    def memoryBytes = column[Memory]("memory_bytes")
    def returnCode = column[Long]("return_code")
    def testerReturnCode = column[Long]("tester_return_code")

    override def * = (testingID, testID, resultCode, recordTime, timeMs, memoryBytes, returnCode,
      testerReturnCode, groupName, points) <> (DBResultEntry.tupled, DBResultEntry.unapply)
  }

  val results = TableQuery[Results]

  val headResultByTesting = Compiled((testingID: Rep[Long]) => results.filter(_.testingID === testingID).sortBy(_.testID.desc).take(1))

  case class DBResultEntry(testingID: Long, testID: Int, resultCode: Int, recordTime: DateTime, timeMs: TimeMs, memoryBytes:Memory,
                           returnCode: Long, testerReturnCode: Long, groupName: Option[String], points: Double)

  val results0 = results.sortBy(_.testID)

  // ---- TODO(task19): printing, rooms and waiter tasks. These tables do not exist in the teacher-web schema; the
  // definitions stay only so that the printing code compiles until it is deleted. ----

  case class DBPrintJob(id: Option[Long], contest: Int, team: Int, filename: String, data: Array[Byte],
                        computer: InetString, arrived: DateTime)

  case class DBPrintJobs(tag: Tag) extends Table[DBPrintJob](tag, "print_jobs") {
    def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
    def contest = column[Int]("contest")
    def team = column[Int]("team")
    def filename = column[String]("filename")
    def data = column[Array[Byte]]("data")
    def computer = column[InetString]("computer_id")
    def arrived = column[DateTime]("arrived")

    override def * = (id.?, contest, team, filename, data, computer, arrived) <> (DBPrintJob.tupled, DBPrintJob.unapply)
  }

  val dbPrintJobs = TableQuery[DBPrintJobs]

  case class DBPrintRender(id: Option[Long], jobId: Long, format: String, pages: Int, data: Array[Byte],
                           created: DateTime)

  case class DBPrintRenders(tag: Tag) extends Table[DBPrintRender](tag, "print_renders") {
    def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
    def jobId = column[Long]("job_id")
    def format = column[String]("format")
    def pages = column[Int]("pages")
    def data = column[Array[Byte]]("data")
    def created = column[DateTime]("created")

    override def * = (id.?, jobId, format, pages, data, created) <> (DBPrintRender.tupled, DBPrintRender.unapply)
  }

  val dbPrintRenders = TableQuery[DBPrintRenders]

  case class DBPrintAttempt(id: Option[Long], jobId: Long, kind: String, created: DateTime, requestedBy: Option[String],
                            printer: String, renderId: Option[Long], status: String, finished: Option[DateTime],
                            pagesPrinted: Int, spoolerJobId: Option[Long], error: String, detail: String)

  case class DBPrintAttempts(tag: Tag) extends Table[DBPrintAttempt](tag, "print_attempts") {
    def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
    def jobId = column[Long]("job_id")
    def kind = column[String]("kind")
    def created = column[DateTime]("created")
    def requestedBy = column[Option[String]]("requested_by")
    def printer = column[String]("printer")
    def renderId = column[Option[Long]]("render_id")
    def status = column[String]("status")
    def finished = column[Option[DateTime]]("finished")
    def pagesPrinted = column[Int]("pages_printed")
    def spoolerJobId = column[Option[Long]]("spooler_job_id")
    def error = column[String]("error")
    def detail = column[String]("detail")

    override def * = (id.?, jobId, kind, created, requestedBy, printer, renderId, status, finished, pagesPrinted,
      spoolerJobId, error, detail) <> (DBPrintAttempt.tupled, DBPrintAttempt.unapply)
  }

  val dbPrintAttempts = TableQuery[DBPrintAttempts]

  /** Row of the print_job_state view: the state of a job is its latest attempt's (read-only). */
  case class DBPrintJobState(jobId: Long, attemptId: Option[Long], status: String, finished: Option[DateTime],
                             error: String, pages: Int)

  case class DBPrintJobStates(tag: Tag) extends Table[DBPrintJobState](tag, "print_job_state") {
    def jobId = column[Long]("job_id")
    def attemptId = column[Option[Long]]("attempt_id")
    def status = column[String]("status")
    def finished = column[Option[DateTime]]("finished")
    def error = column[String]("error")
    def pages = column[Int]("pages")

    override def * = (jobId, attemptId, status, finished, error, pages) <> (DBPrintJobState.tupled, DBPrintJobState.unapply)
  }

  val dbPrintJobStates = TableQuery[DBPrintJobStates]

  case class LiftedLocatedPrintJob(id: Rep[Long], contest: Rep[Int], team: Rep[Int], filename: Rep[String],
                                   arrived: Rep[DateTime], printed: Rep[Option[DateTime]], computerName: Rep[String],
                                   pages: Rep[Int], error: Rep[String])

  case class LocatedPrintJob(id: Long, contest: Int, team: Int, filename: String, arrived: DateTime,
                             printed: Option[DateTime], computerName: String, pages: Int, error: String)

  implicit object CustomPrintJobShape extends CaseClassShape(LiftedLocatedPrintJob.tupled, LocatedPrintJob.tupled)

  case class CompLocation(id: InetString, location: Int, name: String)

  case class CompLocations(tag: Tag) extends Table[CompLocation](tag, "computer_locations") {
    def id = column[InetString]("id", O.PrimaryKey)
    def location = column[Int]("location")
    def name = column[String]("name")

    override def * = (id, location, name) <> (CompLocation.tupled, CompLocation.unapply)
  }

  val compLocations = TableQuery[CompLocations]

  val locatedPrintJobs = (for {
    p <- dbPrintJobs
    l <- compLocations if p.computer === l.id
    s <- dbPrintJobStates if s.jobId === p.id
  } yield LiftedLocatedPrintJob(p.id, p.contest, p.team, p.filename, p.arrived, s.finished, l.name, s.pages, s.error)).sortBy(_.arrived.desc)

  val locatedPrintJobsByContest = Compiled((contestID: Rep[Int]) =>
    locatedPrintJobs.filter(_.contest === contestID)
  )

  case class Areas(tag: Tag) extends Table[Area](tag, "areas") {
    def id = column[Int]("id", O.AutoInc, O.PrimaryKey)
    def name = column[String]("name")
    def printer = column[String]("printer")

    override def * = (id, name, printer) <> (Area.tupled, Area.unapply)
  }

  val areas = TableQuery[Areas]

  /** A network printer that a print agent reported it can print to. */
  case class AgentPrinter(agent: String, unc: String, server: String, share: String, seen: DateTime)

  case class AgentPrinters(tag: Tag) extends Table[AgentPrinter](tag, "agent_printers") {
    def agent = column[String]("agent")
    def unc = column[String]("unc")
    def server = column[String]("server")
    def share = column[String]("share")
    def seen = column[DateTime]("seen")

    def pk = primaryKey("agent_printers_pkey", (agent, unc))

    override def * = (agent, unc, server, share, seen) <> (AgentPrinter.tupled, AgentPrinter.unapply)
  }

  val agentPrinters = TableQuery[AgentPrinters]

  case class WaiterTasks(tag: Tag) extends Table[(Long, DateTime, String, String)](tag, "waiter_tasks") {
    def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
    def created = column[DateTime]("created")
    def message = column[String]("message")
    def rooms = column[String]("rooms")

    override def * = (id, created, message, rooms)
  }

  val waiterTasks = TableQuery[WaiterTasks]

  class WaiterTaskRecords(tag: Tag) extends Table[(Long, String, DateTime)](tag, "waiter_task_record") {
    def id = column[Long]("id")
    def room = column[String]("room")
    def ts = column[DateTime]("ts")

    def pk = primaryKey("waiter_task_record_pkey", (id, room))
    override def * = (id, room, ts)
  }

  val waiterTaskRecords = TableQuery[WaiterTaskRecords]

  // TODO(task11): qf's own admin table; teacher-web's `admins` (email + bcrypt) replaces it.
  case class Admins(tag: Tag) extends Table[AdminEntry](tag, "admins") {
    def username = column[String]("username", O.PrimaryKey)
    def password = column[String]("password")
    def spectator = column[String]("spectator")
    def administrator = column[String]("administrator")
    def locations = column[String]("locations")
    def unrestricted = column[String]("unrestricted_view")

    override def * = (username, password, spectator, administrator, locations, unrestricted) <> (AdminEntry.tupled, AdminEntry.unapply)
  }

  val admins = TableQuery[Admins]
}
