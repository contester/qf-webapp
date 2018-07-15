package models

import com.github.nscala_time.time.Imports._
import play.api.Logger
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

object SlickModel {
  import slick.jdbc.MySQLProfile.api._
  import utils.Db._

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
}

object ClarificationModel {
  import slick.jdbc.MySQLProfile.api._

  def loadAll(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext) =
    db.run(SlickModel.clarifications.result).zip(db.run(SlickModel.clrSeen2.result))

  def getClarifications(db: JdbcBackend#DatabaseDef, contestId: Int)(implicit ec: ExecutionContext) =
    db.run(SlickModel.clarifications.filter(_.contest === contestId).result).map(_.sortBy(_.arrived).reverse)

  def getClarification(db: JdbcBackend#DatabaseDef, id: Int)(implicit ec: ExecutionContext) =
    db.run(SlickModel.clarifications.filter(_.id === id).result).map(_.headOption)

  def toggleClarification(db: JdbcBackend#DatabaseDef, id: Int)(implicit ec: ExecutionContext) =
    getClarification(db, id).flatMap { clrs =>
      Future.sequence(clrs.toSeq.map { clr =>
        db.run(SlickModel.clarifications.filter(_.id === id).map(_.hidden).update(!clr.hidden))
      })
    }.map(_ => ())

  def updateClarification(db: JdbcBackend#DatabaseDef, cl: Clarification)(implicit ec: ExecutionContext) = {
    val f = db.run(SlickModel.clarifications.returning(SlickModel.clarifications.map(_.id)).insertOrUpdate(cl)).map { opt =>
      opt.map(x => cl.copy(id = Some(x)))
    }
    f.onComplete(x => Logger.info(s"updated $x"))
    f
  }

  def getVisibleClarifications(db: JdbcBackend#DatabaseDef, contestId: Int)(implicit ec: ExecutionContext) =
    db.run(SlickModel.clarifications.filter(x => x.contest === contestId && !x.hidden).result).map(_.sortBy(_.arrived).reverse)

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