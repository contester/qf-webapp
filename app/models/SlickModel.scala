package models

import com.github.nscala_time.time.Imports._
import play.api.{Logger, Logging}
import play.api.libs.json.JsValue
import slick.jdbc.JdbcBackend

import scala.concurrent.{ExecutionContext, Future}

case class Clarification(id: Option[Int], contest: Int, problem: String, text: String, arrived: DateTime, hidden: Boolean)

case class MaxSeen(contest: Int, team: Int, timestamp: DateTime)

case class ClarificationRequest(id: Int, contest: Int, team: Int, problem: String, request: String,
                                answer: Option[String], arrived: DateTime, answered: Boolean) {
  def getAnswer = if (answered)
    answer.getOrElse("No comments")
  else "..."
}

case class Compiler(id: Int, contest: Int, name: String, ext: String)

case class School(id: Int, name: String)

case class Message2(id: Option[Int], contest: Int, team: Int, kind: String, data: JsValue, seen: Boolean)

object SlickModel {
  import slick.jdbc.MySQLProfile.api._
  import utils.Db._
  import com.github.tototoshi.slick.MySQLJodaSupport._

  case class Team(id: Int, school: Int, num: Option[Int], name: String)

  case class Participant(contest: Int, team: Int, localId: Int, disabled: Boolean, noPrint: Boolean, notRated: Boolean)

  case class Assignment(contest: Int, localId: Int, username: String, password: String)

  case class Clarifications(tag: Tag) extends Table[Clarification](tag, "clarifications") {
    def id = column[Int]("cl_id", O.PrimaryKey, O.AutoInc)
    def contest = column[Int]("cl_contest_idf")
    def problem = column[String]("cl_task")
    def text = column[String]("cl_text")
    def arrived = column[DateTime]("cl_date")
    def hidden = column[Boolean]("cl_is_hidden")

    override def * = (id.?, contest, problem, text, arrived, hidden) <> (Clarification.tupled, Clarification.unapply)
  }

  val clarifications = TableQuery[Clarifications]

  def getClarificationsForContest(contestId: Int) =
    clarifications.filter(_.contest === contestId).sortBy(_.arrived.desc)

  case class ClarificationRequests(tag: Tag) extends Table[ClarificationRequest](tag, "ClarificationRequests") {
    def id = column[Int]("ID", O.PrimaryKey)
    def contest = column[Int]("Contest")
    def team = column[Int]("Team")
    def problem = column[String]("Problem")
    def request = column[String]("Request")
    def answer = column[Option[String]]("Answer")
    def arrived = column[DateTime]("Arrived")
    def answered = column[Boolean]("Status")

    override def * = (id, contest, team, problem, request, answer, arrived, answered) <>
      (ClarificationRequest.tupled, ClarificationRequest.unapply)
  }

  val clarificationRequests = TableQuery[ClarificationRequests]

  val clarificationRequestsUnanswered = for {
    c <- clarificationRequests.filter(!_.answered)
  } yield (c.id, c.contest)

  case class ClrSeen2(tag: Tag) extends Table[MaxSeen](tag, "ClrSeen2") {
    def contest = column[Int]("Contest")
    def team = column[Int]("Team")
    def timestamp = column[DateTime]("MaxSeen")

    override def * = (contest, team, timestamp) <> (MaxSeen.tupled, MaxSeen.unapply)
  }

  val clrSeen2 = TableQuery[ClrSeen2]

  case class Compilers(tag: Tag) extends Table[Compiler](tag, "Languages") {
    def id = column[Int]("ID")
    def contest = column[Int]("Contest")
    def name = column[String]("Name")
    def ext = column[String]("Ext")

    override def * = (id, contest, name, ext) <> (Compiler.tupled, Compiler.unapply)
  }

  val compilers = TableQuery[Compilers]

  case class LiftedContest(id: Rep[Int], name: Rep[String], schoolMode: Rep[Boolean], startTime: Rep[DateTime],
                           freezeTime: Rep[DateTime], endTime: Rep[DateTime], exposeTime: Rep[DateTime],
                           printTickets: Rep[Boolean], paused: Rep[Boolean], polygonId: Rep[String],
                           language: Rep[String])

  implicit object ContestShape extends CaseClassShape(LiftedContest.tupled, Contest.tupled)

  case class Contests(tag: Tag) extends Table[Contest](tag, "Contests") {
    def id = column[Int]("ID")
    def name = column[String]("Name")
    def schoolMode = column[Boolean]("SchoolMode")
    def startTime = column[DateTime]("Start")
    def freezeTime = column[DateTime]("Finish")
    def endTime = column[DateTime]("End")
    def exposeTime = column[DateTime]("Expose")
    def printTickets = column[Boolean]("PrintTickets")
    def paused = column[Boolean]("Paused")
    def polygonId = column[String]("PolygonID")
    def language = column[String]("Language")

    override def * = (id, name, schoolMode, startTime, freezeTime, endTime, exposeTime,
      printTickets, paused, polygonId, language) <> (Contest.tupled, Contest.unapply)
  }

  val contests = TableQuery[Contests]

  val contests0 = for {
    c <- contests
  } yield LiftedContest(c.id, c.name, c.schoolMode, c.startTime, c.freezeTime, c.endTime, c.exposeTime, c.printTickets,
    c.paused, c.polygonId, c.language)

  case class Schools(tag: Tag) extends Table[School](tag, "Schools") {
    def id = column[Int]("ID")
    def name = column[String]("Name")

    override def * = (id, name) <> (School.tupled, School.unapply)
  }

  val schools = TableQuery[Schools]

  case class Teams(tag: Tag) extends Table[Team](tag, "Teams") {
    def id = column[Int]("ID")
    def school = column[Int]("School")
    def num = column[Option[Int]]("Num")
    def name = column[String]("Name")

    override def * = (id, school, num, name) <> (Team.tupled, Team.unapply)
  }

  val teams = TableQuery[Teams]

  case class Participants(tag: Tag) extends Table[Participant](tag, "Participants") {
    def contest = column[Int]("Contest")
    def team = column[Int]("Team")
    def localId = column[Int]("LocalID")
    def disabled = column[Boolean]("Disabled")
    def noPrint = column[Boolean]("NoPrint")
    def notRated = column[Boolean]("NotRated")

    override def * = (contest, team, localId, disabled, noPrint, notRated) <> (Participant.tupled, Participant.unapply)
  }

  val participants = TableQuery[Participants]


  case class Assignments(tag: Tag) extends Table[Assignment](tag, "Assignments") {
    def contest = column[Int]("Contest")
    def localId = column[Int]("LocalID")
    def username = column[String]("Username")
    def password = column[String]("Password")

    override def * = (contest, localId, username, password) <> (Assignment.tupled, Assignment.unapply)
  }

  val assignments = TableQuery[Assignments]

  case class ExtraInfos(tag: Tag) extends Table[Extrainfo](tag, "Extrainfo") {
    def contest = column[Int]("Contest")
    def num = column[Int]("Num")
    def heading = column[String]("Heading")
    def data = column[String]("Data")

    override def * = (contest, num, heading, data) <> (Extrainfo.tupled, Extrainfo.unapply)
  }

  val extraInfos = TableQuery[ExtraInfos]

  case class Messages2(tag: Tag) extends Table[Message2](tag, "Messages2") {
    def id = column[Int]("ID", O.AutoInc)
    def contest = column[Int]("Contest")
    def team = column[Int]("Team")
    def kind = column[String]("Kind")
    def value = column[JsValue]("Value")
    def seen = column[Boolean]("Seen")

    override def * = (id.?, contest, team, kind, value, seen) <> (Message2.tupled, Message2.unapply)
  }

  val messages2 = TableQuery[Messages2]

  case class LiftedLocalTeam(teamId: Rep[Int], contest: Rep[Int], localId: Rep[Int], schoolName: Rep[String], teamNum: Rep[Option[Int]],
                             teamName: Rep[String], notRated: Rep[Boolean], noPrint: Rep[Boolean],
                             disabled: Rep[Boolean])

  implicit object LocalTeamShape extends CaseClassShape(LiftedLocalTeam.tupled, LocalTeam.tupled)

  val localTeamQuery = for {
    ((p, t), s) <- participants join teams on (_.team === _.id) join schools on (_._2.school === _.id)
  } yield LiftedLocalTeam(p.team, p.contest, p.localId, s.name, t.num, t.name, p.notRated, p.noPrint, p.disabled)

  case class LoggedInTeam0(username: String, password: String,  contest: Contest, team: LocalTeam)

  case class LiftedLoggedInTeam0(username: Rep[String], password: Rep[String], contest: LiftedContest, team: LiftedLocalTeam)

  implicit object LoggedInTeam0Shape extends CaseClassShape(LiftedLoggedInTeam0.tupled, LoggedInTeam0.tupled)

  val joinedLoginQuery = for {
    ((assignment, team), contest) <- assignments join
      localTeamQuery on { case (a, lt) => (a.localId === lt.localId) && (a.contest === lt.contest) } join
      contests0 on (_._1.contest === _.id)
  } yield LiftedLoggedInTeam0(assignment.username, assignment.password, contest, team)

  case class NewSubmit(id: Option[Int], contest: Int, team: Int, problem: String, srcLang: Int, source: Array[Byte],
                       arrived: DateTime, computer: Option[Long], processed: Option[Int])

  case class NewSubmits(tag: Tag) extends Table[NewSubmit](tag, "NewSubmits") {
    def id = column[Int]("ID", O.AutoInc)
    def contest = column[Int]("Contest")
    def team = column[Int]("Team")
    def problem = column[String]("Problem")
    def srcLang = column[Int]("SrcLang")
    def source = column[Array[Byte]]("Source")
    def arrived = column[DateTime]("Arrived")
    def computer = column[Option[Long]]("Computer")
    def processed = column[Option[Int]]("Processed")

    override def * = (id.?, contest, team, problem, srcLang, source, arrived, computer, processed) <> (NewSubmit.tupled, NewSubmit.unapply)
  }

  val newSubmits = TableQuery[NewSubmits]

  case class Testing(id: Int, submit: Int, start: DateTime, finish: Option[DateTime], problemId: Option[String])

  case class Testings(tag: Tag) extends Table[Testing](tag, "Testings") {
    def id = column[Int]("ID", O.AutoInc)
    def submit = column[Int]("Submit")
    def start = column[DateTime]("Start")
    def finish = column[Option[DateTime]]("Finish")
    def problemId = column[Option[String]]("ProblemID")

    override def * = (id, submit, start, finish, problemId) <> (Testing.tupled, Testing.unapply)
  }

  val testings = TableQuery[Testings]

  case class PrintJob(id: Option[Long], contest: Int, team: Int, filename: String, data: Array[Byte], computer: Long, arrived: DateTime, printed: Option[Int])

  case class PrintJobs(tag: Tag) extends Table[PrintJob](tag, "PrintJobs") {
    def id = column[Long]("ID", O.AutoInc)
    def contest = column[Int]("Contest")
    def team = column[Int]("Team")
    def filename = column[String]("Filename")
    def data = column[Array[Byte]]("DATA")
    def computer = column[Long]("Computer")
    def arrived = column[DateTime]("Arrived")
    def printed = column[Option[Int]]("Printed")

    override def * = (id.?, contest, team, filename, data, computer, arrived, printed) <> (PrintJob.tupled, PrintJob.unapply)
  }

  val printJobs = TableQuery[PrintJobs]

  case class CompLocation(id: Long, location: Int, name: String)

  case class CompLocations(tag: Tag) extends Table[CompLocation](tag, "CompLocations") {
    def id = column[Long]("ID", O.PrimaryKey)
    def location = column[Int]("Location")
    def name = column[String]("Name")

    override def * = (id, location, name) <> (CompLocation.tupled, CompLocation.unapply)
  }

  val compLocations = TableQuery[CompLocations]

  case class EvalDBEntry(id: Option[Long], touched: DateTime, ext: String, source: Array[Byte], input: Array[Byte],
                         output:Option[Array[Byte]], timex: Option[Long], memory: Option[Long], info: Option[Long],
                         result: Int, contest: Int, team: Int, processed: Option[Int], arrived: DateTime)

  case class Eval(tag: Tag) extends Table[EvalDBEntry](tag, "Eval") {
    def id = column[Long]("ID", O.AutoInc)
    def touched = column[DateTime]("Touched")
    def ext = column[String]("Ext")
    def source = column[Array[Byte]]("Source")
    def input = column[Array[Byte]]("Input")
    def output = column[Option[Array[Byte]]]("Output")
    def timex = column[Option[Long]]("Timex")
    def memory = column[Option[Long]]("Memory")
    def info = column[Option[Long]]("Info")
    def result = column[Int]("Result")
    def contest = column[Int]("Contest")
    def team = column[Int]("Team")
    def processed = column[Option[Int]]("Processed")
    def arrived = column[DateTime]("Arrived")

    override def * = (id.?, touched, ext, source, input, output, timex, memory, info, result, contest, team, processed, arrived) <> (EvalDBEntry.tupled, EvalDBEntry.unapply)
  }

  val eval = TableQuery[Eval]

  case class DBResult(uid: Long, submit: Int, processed: DateTime, result: Int, test: Option[Int], timex: Option[Long],
                      memory: Option[Long], info: Option[Long], testerOutput: Option[String],
                      testerError: Option[String], testerExitCode:Option[Int])
  case class Results(tag: Tag) extends Table[DBResult](tag, "Results") {
    def uid = column[Long]("UID")
    def submit = column[Int]("Submit")
    def processed = column[DateTime]("Processed")
    def result = column[Int]("Result")
    def test = column[Option[Int]]("Test")
    def timex = column[Option[Long]]("Timex")
    def memory = column[Option[Long]]("Memory")
    def info = column[Option[Long]]("Info")
    def testerOutput = column[Option[String]]("TesterOutput")
    def testerError = column[Option[String]]("TesterError")
    def testerExitCode = column[Option[Int]]("TesterExitCode")

    override def * = (uid, submit, processed, result, test, timex, memory, info, testerOutput, testerError, testerExitCode) <> (DBResult.tupled, DBResult.unapply)
  }

  val results = TableQuery[Results]
}

object ClarificationModel extends Logging {
  import slick.jdbc.MySQLProfile.api._

  def getClarifications(db: JdbcBackend#DatabaseDef, contestId: Int)(implicit ec: ExecutionContext) =
    db.run(SlickModel.getClarificationsForContest(contestId).result)

  def getClarification(db: JdbcBackend#DatabaseDef, id: Int)(implicit ec: ExecutionContext) =
    db.run(SlickModel.clarifications.filter(_.id === id).result).map(_.headOption)

  def updateClarification(db: JdbcBackend#DatabaseDef, cl: Clarification)(implicit ec: ExecutionContext) = {
    val f = db.run(SlickModel.clarifications.returning(SlickModel.clarifications.map(_.id)).insertOrUpdate(cl)).map { opt =>
      opt.map(x => cl.copy(id = Some(x)))
    }
    f.onComplete(x => logger.info(s"updated $x"))
    f
  }

  def deleteClarification(db: JdbcBackend#DatabaseDef, id: Int)(implicit ec: ExecutionContext) =
    db.run(SlickModel.clarifications.filter(_.id === id).delete)

  def getClarificationReqs(db: JdbcBackend#DatabaseDef, contestId: Int)(implicit ec: ExecutionContext) =
    db.run(SlickModel.clarificationRequests.filter(_.contest === contestId).result).map(_.sortBy(_.arrived).reverse)

  def getTeamClarificationReqs(db: JdbcBackend#DatabaseDef, contestId: Int, teamId: Int)(implicit ec: ExecutionContext) =
    db.run(SlickModel.clarificationRequests.filter(x => x.contest === contestId && x.team === teamId).result)
      .map(_.sortBy(_.arrived).reverse)

  def getClarificationReq(db: JdbcBackend#DatabaseDef, id: Int)(implicit ec: ExecutionContext) =
    db.run(SlickModel.clarificationRequests.filter(_.id === id).result).map(_.headOption)

  def updateClarificationRequest(db: JdbcBackend#DatabaseDef, clr: ClarificationRequest)(implicit ec: ExecutionContext): Future[Option[Int]] =
    db.run(SlickModel.clarificationRequests.returning(SlickModel.clarificationRequests.map(_.id)).insertOrUpdate(clr))
}