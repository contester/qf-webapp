package models

object SlickModel {
  import com.github.nscala_time.time.Imports._
  import slick.driver.MySQLDriver.api._
  import utils.Db._

  case class Clarification(id: Int, contest: Int, problem: String, text: String, arrived: DateTime, hidden: Boolean)

  case class Clarifications(tag: Tag) extends Table[Clarification](tag, "clarifications") {
    def id = column[Int]("cl_id", O.PrimaryKey, O.AutoInc)
    def contest = column[Int]("cl_contest_idf")
    def problem = column[String]("cl_task")
    def text = column[String]("cl_text")
    def arrived = column[DateTime]("cl_date")
    def hidden = column[Boolean]("cl_is_hidden")

    override def * = (id, contest, problem, text, arrived, hidden) <> (Clarification.tupled, Clarification.unapply)
  }

  val clarifications = TableQuery[Clarifications]

  case class ClarificationRequest(id: Int, contest: Int, team: Int, problem: String, request: String,
                                  answer: String, arrived: DateTime, answered: Boolean)

  case class ClarificationRequests(tag: Tag) extends Table[ClarificationRequest](tag, "ClarificationRequests") {
    def id = column[Int]("ID", O.PrimaryKey)
    def contest = column[Int]("Contest")
    def team = column[Int]("Team")
    def problem = column[String]("Problem")
    def request = column[String]("Request")
    def answer = column[String]("Answer")
    def arrived = column[DateTime]("Arrived")
    def answered = column[Boolean]("Status")

    override def * = (id, contest, team, problem, request, answer, arrived, answered) <>
      (ClarificationRequest.tupled, ClarificationRequest.unapply)
  }

  val clarificationRequests = TableQuery[ClarificationRequests]
}