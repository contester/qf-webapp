package models

import com.github.nscala_time.time.Imports._
import com.github.tminglei.slickpg.InetString
import com.google.protobuf.ByteString
import com.spingo.op_rabbit.Message
import play.api.{Logger, Logging}
import play.api.libs.json.JsValue
import protos.Tickets.{Computer, IdName, PrintJob, PrintJobReport}
import slick.basic.DatabaseConfig
import slick.jdbc.{GetResult, JdbcBackend, JdbcProfile}

import scala.concurrent.{ExecutionContext, Future}

case class Clarification(id: Option[Long], contest: Int, problem: String, text: String, arrived: DateTime, hidden: Boolean)

case class MaxSeen(contest: Int, team: Int, timestamp: DateTime)

case class ClarificationRequest(id: Long, contest: Int, team: Int, problem: String, request: String,
                                answer: String, arrived: DateTime, answered: Boolean) {
  def getAnswer = if (answered) {
    if (answer.isEmpty) "No comments" else answer
  } else "..."
}

case class Compiler(id: Int, name: String, ext: String)

case class School(id: Int, name: String, fullName: String)

case class Message2(id: Option[Int], contest: Int, team: Int, kind: String, data: JsValue, seen: Boolean)

object SlickModel {
  import com.github.tototoshi.slick.PostgresJodaSupport._
  import utils.MyPostgresProfile.api._

  case class Team(id: Int, school: Int, num: Int, name: String)

  case class Participant(contest: Int, team: Int, disabled: Boolean, noPrint: Boolean, notRated: Boolean)

  case class Assignment(contest: Int, teamId: Int, username: String, password: String)

  case class Clarifications(tag: Tag) extends Table[Clarification](tag, "clarifications") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def contest = column[Int]("contest")
    def problem = column[String]("problem")
    def text = column[String]("text")
    def arrived = column[DateTime]("posted")
    def hidden = column[Boolean]("hidden")

    override def * = (id.?, contest, problem, text, arrived, hidden) <> (Clarification.tupled, Clarification.unapply)
  }

  val clarifications = TableQuery[Clarifications]

  def getClarificationsForContest(contestId: Int) =
    clarifications.filter(_.contest === contestId).sortBy(_.arrived.desc)

  case class ClarificationRequests(tag: Tag) extends Table[ClarificationRequest](tag, "clarification_requests") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def contest = column[Int]("contest")
    def team = column[Int]("team")
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
    def contest = column[Int]("contest", O.PrimaryKey)
    def team = column[Int]("team", O.PrimaryKey)
    def timestamp = column[DateTime]("max_seen")

    override def * = (contest, team, timestamp) <> (MaxSeen.tupled, MaxSeen.unapply)
  }

  val clrSeen2 = TableQuery[ClrSeen2]

  case class Compilers(tag: Tag) extends Table[Compiler](tag, "languages") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def moduleID = column[String]("module_id")

    def * = (id, name, moduleID) <> (Compiler.tupled, Compiler.unapply)
  }

  val compilers = TableQuery[Compilers]

  val sortedCompilers = compilers.sortBy(_.name)

  case class LiftedContest(id: Rep[Int], name: Rep[String], startTime: Rep[DateTime],
                           freezeTime: Rep[DateTime], endTime: Rep[DateTime], exposeTime: Rep[DateTime],
                           polygonId: Rep[String], language: Rep[String], schoolMode: Rep[Boolean])

  implicit object ContestShape extends CaseClassShape(LiftedContest.tupled, Contest.tupled)

  case class Contests(tag: Tag) extends Table[Contest](tag, "contests") {
    def id = column[Int]("id", O.AutoInc, O.PrimaryKey)
    def name = column[String]("name")
    def startTime = column[DateTime]("start_time")
    def freezeTime = column[DateTime]("freeze_time")
    def endTime = column[DateTime]("end_time")
    def exposeTime = column[DateTime]("expose_time")
    def polygonId = column[String]("polygon_id")
    def language = column[String]("language")
    def schoolMode = column[Boolean]("school_mode")

    override def * = (id, name, startTime, freezeTime, endTime, exposeTime,
      polygonId, language, schoolMode) <> (Contest.tupled, Contest.unapply)
  }

  val contests = TableQuery[Contests]

  val contests0 = for {
    c <- contests
  } yield LiftedContest(c.id, c.name, c.startTime, c.freezeTime, c.endTime, c.exposeTime, c.polygonId, c.language, c.schoolMode)

  case class Schools(tag: Tag) extends Table[School](tag, "schools") {
    def id = column[Int]("id", O.AutoInc, O.PrimaryKey)
    def name = column[String]("short_name")
    def fullName = column[String]("full_name")

    override def * = (id, name, fullName) <> (School.tupled, School.unapply)
  }

  val schools = TableQuery[Schools]

  case class Teams(tag: Tag) extends Table[Team](tag, "teams") {
    def id = column[Int]("id", O.AutoInc, O.PrimaryKey)
    def school = column[Int]("school")
    def num = column[Int]("num")
    def name = column[String]("name")

    override def * = (id, school, num, name) <> (Team.tupled, Team.unapply)
  }

  val teams = TableQuery[Teams]

  case class Participants(tag: Tag) extends Table[Participant](tag, "participants") {
    def contest = column[Int]("contest")
    def team = column[Int]("team")
    def disabled = column[Boolean]("disabled")
    def noPrint = column[Boolean]("no_print")
    def notRated = column[Boolean]("not_rated")

    override def * = (contest, team, disabled, noPrint, notRated) <> (Participant.tupled, Participant.unapply)
  }

  val participants = TableQuery[Participants]


  case class Assignments(tag: Tag) extends Table[Assignment](tag, "logins") {
    def contest = column[Int]("contest")
    def team = column[Int]("team")
    def username = column[String]("username")
    def password = column[String]("password")

    override def * = (contest, team, username, password) <> (Assignment.tupled, Assignment.unapply)
  }

  val assignments = TableQuery[Assignments]

  case class ExtraInfos(tag: Tag) extends Table[Extrainfo](tag, "extra_info") {
    def contest = column[Int]("contest")
    def num = column[Int]("num")
    def heading = column[String]("heading")
    def data = column[String]("data")

    override def * = (contest, num, heading, data) <> (Extrainfo.tupled, Extrainfo.unapply)
  }

  val extraInfos = TableQuery[ExtraInfos]

  case class Messages2(tag: Tag) extends Table[Message2](tag, "messages") {
    def id = column[Int]("id", O.AutoInc)
    def contest = column[Int]("contest")
    def team = column[Int]("team")
    def kind = column[String]("kind")
    def value = column[JsValue]("value")
    def seen = column[Boolean]("seen")

    override def * = (id.?, contest, team, kind, value, seen) <> (Message2.tupled, Message2.unapply)
  }

  val messages2 = TableQuery[Messages2]

  case class LiftedLocalTeam(teamId: Rep[Int], contest: Rep[Int], schoolName: Rep[String], teamNum: Rep[Int],
                             teamName: Rep[String], notRated: Rep[Boolean], noPrint: Rep[Boolean],
                             disabled: Rep[Boolean])

  implicit object LocalTeamShape extends CaseClassShape(LiftedLocalTeam.tupled, LocalTeam.tupled)

  val localTeamQuery = for {
    ((p, t), s) <- participants join teams on (_.team === _.id) join schools on (_._2.school === _.id)
  } yield LiftedLocalTeam(p.team, p.contest, s.name, t.num, t.name, p.notRated, p.noPrint, p.disabled)

  case class LoggedInTeam0(username: String, password: String,  contest: Contest, team: LocalTeam)

  case class LiftedLoggedInTeam0(username: Rep[String], password: Rep[String], contest: LiftedContest, team: LiftedLocalTeam)

  implicit object LoggedInTeam0Shape extends CaseClassShape(LiftedLoggedInTeam0.tupled, LoggedInTeam0.tupled)

  val joinedLoginQuery = for {
    ((assignment, team), contest) <- assignments join
      localTeamQuery on { case (a, lt) => (a.team === lt.teamId) && (a.contest === lt.contest) } join
      contests0 on (_._1.contest === _.id)
  } yield LiftedLoggedInTeam0(assignment.username, assignment.password, contest, team)

  case class Submits(tag: Tag) extends Table[(Long, Int, Int, String, Int, Array[Byte], DateTime, Boolean, Boolean, Int, Long, Int, Boolean)](tag, "submits") {
    def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
    def contest = column[Int]("contest")
    def team = column[Int]("team_id")
    def problem = column[String]("problem")
    def language = column[Int]("language_id")
    def source = column[Array[Byte]]("source")
    def arrived = column[DateTime]("submit_time_absolute")
    def tested = column[Boolean]("tested")
    def success = column[Boolean]("success")
    def passed = column[Int]("passed")
    def testingID = column[Long]("testing_id")
    def taken = column[Int]("taken")
    def compiled = column[Boolean]("compiled")

    override def * = (id, contest, team, problem, language, source, arrived, tested, success, passed, testingID, taken, compiled)
  }

  val submits = TableQuery[Submits]

  case class LiftedContestTeamID(contest: Rep[Int], team: Rep[Int])

  implicit object ContestTeamIDShape extends CaseClassShape(LiftedContestTeamID.tupled, ContestTeamIds.tupled)

  def getContestTeamIDs(submitID: Long) =
    submits.filter(_.id === submitID).map(x => LiftedContestTeamID(x.contest, x.team)).take(1)

  case class Problems(tag: Tag) extends Table[Problem](tag, "problems") {
    def contestID = column[Int]("contest_id")
    def id = column[String]("id")
    def tests = column[Int]("tests")
    def name = column[String]("name")

    def * = (contestID, id, name, tests) <> (Problem.tupled, Problem.unapply)
  }

  val problems = TableQuery[Problems]

  case class LiftedSubmit(id: Rep[Long], arrived: Rep[DateTime], arrivedSeconds: Rep[Period], afterFreeze: Rep[Boolean],
                          teamID: Rep[Int], contestID: Rep[Int], problemID: Rep[String], ext: Rep[String],
                          finished: Rep[Boolean], compiled: Rep[Boolean], passed: Rep[Int], taken: Rep[Int],
                          testingID: Rep[Long])

  case class UpliftedSubmit(id: Long, arrived: DateTime, arrivedSeconds: Period, afterFreeze: Boolean, teamID: Int,
                            contestID: Int, problemID: String, ext: String, finished: Boolean, compiled: Boolean,
                            passed: Int, taken: Int, testingID: Long)

  implicit object SubmitShape extends CaseClassShape(LiftedSubmit.tupled, UpliftedSubmit.tupled)

  val submits0 = (for {
    s <- submits
    contest <- contests if s.contest === contest.id
    problem <- problems if s.problem === problem.id && s.contest === problem.contestID
    lang <- compilers if s.language === lang.id
  } yield LiftedSubmit(s.id, s.arrived, s.arrived - contest.startTime, s.arrived > contest.freezeTime,
    s.team, s.contest, s.problem, lang.moduleID, s.tested, s.compiled, s.passed, s.taken, s.testingID)).sortBy(_.arrived)

  case class Testing(id: Int, submit: Int, start: DateTime, finish: Option[DateTime], problemId: Option[String])

  case class Testings(tag: Tag) extends Table[(Long, Long, DateTime, String, Option[DateTime])](tag, "testings") {
    def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
    def submit = column[Long]("submit")
    def startTime = column[DateTime]("start_time")
    def problemURL = column[String]("problem_id")
    def finishTime = column[Option[DateTime]]("finish_time")

    override def * = (id, submit, startTime, problemURL, finishTime)
  }

  val testings = TableQuery[Testings]

  case class DBPrintJob(id: Option[Long], contest: Int, team: Int, filename: String, data: Array[Byte],
                        computer: InetString, arrived: DateTime, printed: Option[DateTime],
                        pages: Int, error: String)

  case class DBPrintJobs(tag: Tag) extends Table[DBPrintJob](tag, "print_jobs") {
    def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
    def contest = column[Int]("contest")
    def team = column[Int]("team")
    def filename = column[String]("filename")
    def data = column[Array[Byte]]("data")
    def computer = column[InetString]("computer_id")
    def arrived = column[DateTime]("arrived")
    def printed = column[Option[DateTime]]("printed")
    def pages = column[Int]("pages")
    def error = column[String]("error")

    override def * = (id.?, contest, team, filename, data, computer, arrived, printed, pages, error) <> (DBPrintJob.tupled, DBPrintJob.unapply)
  }

  val dbPrintJobs = TableQuery[DBPrintJobs]

  case class LiftedLocatedPrintJob(id: Rep[Long], contest: Rep[Int], team: Rep[Int], filename: Rep[String],
                                   arrived: Rep[DateTime], printed: Rep[Option[DateTime]], computerName: Rep[String],
                                   error: Rep[String])

  case class LocatedPrintJob(id: Long, contest: Int, team: Int, filename: String, arrived: DateTime,
                             printed: Option[DateTime], computerName: String, error: String)

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
  } yield LiftedLocatedPrintJob(p.id, p.contest, p.team, p.filename, p.arrived, p.printed, l.name, p.error)).sortBy(_.arrived.desc)

  case class CustomTests(tag: Tag) extends Table[(Long, Int, Int, Int, Array[Byte], Array[Byte], Array[Byte], DateTime,
    Option[DateTime], Int, TimeMs, Memory, Long)](tag, "custom_test") {
    def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
    def contest = column[Int]("contest")
    def team = column[Int]("team_id")
    def language = column[Int]("language_id")
    def source = column[Array[Byte]]("source")
    def input = column[Array[Byte]]("input")
    def output = column[Array[Byte]]("output")
    def arrived = column[DateTime]("submit_time_absolute")
    def finishTime = column[Option[DateTime]]("finish_time")
    def resultCode = column[Int]("result_code")
    def timeMs = column[TimeMs]("time_ms")
    def memoryBytes = column[Memory]("memory_bytes")
    def returnCode = column[Long]("return_code")

    override def * = (id, contest, team, language, source, input, output, arrived,
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
    def testID = column[Int]("test_id")
    def resultCode = column[Int]("result_code")
    def recordTime = column[DateTime]("record_time")
    def timeMs = column[TimeMs]("time_ms")
    def memoryBytes = column[Memory]("memory_bytes")
    def returnCode = column[Long]("return_code")
    def testerOutput = column[Array[Byte]]("tester_output")
    def testerError = column[Array[Byte]]("tester_error")
    def testerReturnCode = column[Long]("tester_return_code")

    override def * = (testingID, testID, resultCode, recordTime, timeMs, memoryBytes, returnCode, testerOutput,
      testerError, testerReturnCode) <> (DBResultEntry.tupled, DBResultEntry.unapply)
  }

  val results = TableQuery[Results]

  case class DBResultEntry(testingID: Long, testID: Int, resultCode: Int, recordTime: DateTime, timeMs: TimeMs, memoryBytes:Memory,
                           returnCode: Long, testerOutput: Array[Byte], testerError: Array[Byte], testerReturnCode: Long)

  val results0 = results.sortBy(_.testID)

  case class Areas(tag: Tag) extends Table[Area](tag, "areas") {
    def id = column[Int]("id", O.AutoInc, O.PrimaryKey)
    def name = column[String]("name")
    def printer = column[String]("printer")

    override def * = (id, name, printer) <> (Area.tupled, Area.unapply)
  }

  val areas = TableQuery[Areas]

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

    override def * = (id, room, ts)
  }

  val waiterTaskRecords = TableQuery[WaiterTaskRecords]
}

case class ShortenedPrintJob(id: Int, filename: String, contest: Int, team: Int, computerID: String,
                             computerName: String, areaID: Int, areaName: String, printer: String,
                             data: Array[Byte], arrived: DateTime)

case class PrintEntry(filename: String, arrived: DateTime, printed: Boolean)

class PrintingModel(dbConfig: DatabaseConfig[JdbcProfile], statusActorModel: StatusActorModel, rabbitMqModel: RabbitMqModel)(implicit ec: ExecutionContext) extends Logging {
  import com.github.tototoshi.slick.PostgresJodaSupport._
  import utils.MyPostgresProfile.api._
  import SlickModel._

  private[this] val db = dbConfig.db


  def insertAndPrintJob(contest:Int, team:Int, filename: String, data: Array[Byte], computerAsString: String) = {
    db.run((dbPrintJobs.map(x => (x.contest, x.team, x.filename, x.data, x.computer)) returning(dbPrintJobs.map(_.id))) += (
      contest, team, filename, data, InetString(computerAsString)
    )).flatMap { id =>
      printJobByID(id)
    }
  }

  private implicit val standardTimeout: akka.util.Timeout = {
    import scala.concurrent.duration._
    Duration(5, SECONDS)
  }

  private[this] def buildProtoFromID(id: Long) = {
    val q = for {
      pj <- dbPrintJobs.filter(_.id === id)
      cl <- compLocations if cl.id === pj.computer
      area <- areas if area.id === cl.location
    } yield (pj.id, pj.filename, pj.contest, pj.team, pj.computer, cl.name, area.id, area.name, area.printer, pj.data, pj.arrived)
    db.run(q.result.headOption).flatMap( pjOpt =>
      pjOpt.map { x =>
        val pj = ShortenedPrintJob(x._1.toInt, x._2, x._3, x._4, x._5.toString, x._6, x._7, x._8, x._9, x._10, x._11)
        statusActorModel.teamClient.getTeam(pj.contest, pj.team).zip(statusActorModel.getContest(pj.contest)).map {
          case (team, contestOpt) =>
            Some(PrintJob(
              filename=pj.filename,
              contest=Some(IdName(pj.contest, contestOpt.get.name)),
              team=Some(IdName(pj.team, team.get.teamFullName)),
              computer=Some(Computer(id=pj.computerID, name=pj.computerName)),
              area=Some(IdName(id=pj.areaID, name=pj.areaName)),
              data = ByteString.copyFrom(pj.data),
              timestampSeconds = pj.arrived.getMillis / 1000,
              printer=pj.printer,
              jobId = s"s-$id"
            ))
        }
      }.getOrElse(Future.successful(None))
    )
  }

  import utils.ProtoRabbitSupport._

  def printJobByID(id: Long) =
    buildProtoFromID(id).map{ optJob =>
      optJob.map { job =>
        rabbitMqModel.rabbitMq ! Message.queue(job, queue = "contester.protoprintjobs")
      }
    }

  def processPrintJobReport(r: PrintJobReport): Future[Int] = {
    if (r.jobExpandedId.startsWith("s-")) {
      val jobId = r.jobExpandedId.substring(2).toLong
      val procTime = new DateTime(r.timestampSeconds * 1000)
      db.run(dbPrintJobs.filter(_.id === jobId).map(x => (x.printed, x.error)).update((Some(procTime), r.errorMessage)))
    } else Future.successful(0)
  }

  def printJobsForTeam(contest:Int, team: Int) =
    db.run(dbPrintJobs.filter(x => ((x.contest === contest) && (x.team === team)))
      .map(x => (x.filename, x.arrived, x.printed.isDefined)).sortBy(_._2.desc).result).map(_.map(PrintEntry.tupled))
}

object ClarificationModel extends Logging {
  import com.github.tototoshi.slick.PostgresJodaSupport._
  import utils.MyPostgresProfile.api._

  def getClarificationReqs(db: JdbcBackend#DatabaseDef, contestId: Int)(implicit ec: ExecutionContext) =
    db.run(SlickModel.clarificationRequests.filter(_.contest === contestId).result).map(_.sortBy(_.arrived).reverse)

  def getTeamClarificationReqs(db: JdbcBackend#DatabaseDef, contestId: Int, teamId: Int)(implicit ec: ExecutionContext) =
    db.run(SlickModel.clarificationRequests.filter(x => x.contest === contestId && x.team === teamId).result)
      .map(_.sortBy(_.arrived).reverse)
}