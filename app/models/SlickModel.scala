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

object SlickModel {
  import slick.jdbc.MySQLProfile.api._
  import utils.Db._

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