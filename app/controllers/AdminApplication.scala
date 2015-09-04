package controllers

import javax.inject.{Inject, Singleton}

import akka.actor.{ActorSystem, Props}
import com.google.common.collect.ImmutableRangeSet
import com.spingo.op_rabbit.{QueueMessage, RabbitControl}
import jp.t2v.lab.play2.auth.AuthElement
import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Controller, RequestHeader}
import slick.driver.JdbcProfile
import slick.jdbc.GetResult
import views.html
import com.github.nscala_time.time.Imports._

import scala.concurrent.{ExecutionContext, Future}

case class RejudgeSubmitRange(range: String)
case class PostClarification(id: Option[Int], contest: Int, problem: String, text: String, date: Option[DateTime], hidden: Boolean)

case class Clarification1(id: Int, contest: Int, problem: String, text: String, date: DateTime, hidden: Boolean)
object Clarification1 {
  implicit val getResult = GetResult(r =>
    Clarification1(r.nextInt(), r.nextInt(), r.nextString(), r.nextString(), new DateTime(r.nextTimestamp()),
      r.nextBoolean())
  )
}

case class ClarificationRequest1(id: Int, contest: Int, team: Int, problem: String, text: String, arrived: DateTime,
                                 answer: String, status: Boolean)

object ClarificationRequest1 {
  implicit val getResult = GetResult(r =>
    ClarificationRequest1(r.nextInt(), r.nextInt(), r.nextInt(), r.nextString(), r.nextString(), new DateTime(r.nextTimestamp()),
    r.nextString(), r.nextBoolean())
  )
}

case class ClarificationResponse(answer: String)

@Singleton
class AdminApplication @Inject() (dbConfigProvider: DatabaseConfigProvider,
                             monitorModel: Monitor,
                             system: ActorSystem,
                             val auth: AuthWrapper,
                             val messagesApi: MessagesApi) extends Controller with AuthElement with AdminAuthConfigImpl with I18nSupport{

  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import com.spingo.op_rabbit.PlayJsonSupport._

  val rabbitMq = system.actorOf(Props[RabbitControl])
  def monitor(id: Int) = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    implicit val ec = StackActionExecutionContext

    monitorModel.getMonitor(id, true).map(x => Ok(html.monitor(x.get.contest, x.get.status)))
  }

  private def anyUser(account: Admin): Future[Boolean] = Future.successful(true)

  private def spectator(contestId: Int)(account: Admin): Future[Boolean] =
    Future.successful(account.canSpectate(contestId))

  private def administrator(contestId: Int)(account: Admin): Future[Boolean] =
    Future.successful(account.canModify(contestId))

  import slick.driver.MySQLDriver.api._

  private def getSubmitCid(submitId: Int) =
    db.run(sql"""select Contest from NewSubmits where ID = $submitId""".as[Int])

  private def canSeeSubmit(submitId: Int)(account: Admin): Future[Boolean] = {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext

    getSubmitCid(submitId).map { cids =>
      cids.exists(account.canSpectate(_))
    }
  }

  private def canRejudgeSubmit(submitId: Int)(account: Admin): Future[Boolean] = {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext

    getSubmitCid(submitId).map { cids =>
      cids.exists(account.canModify(_))
    }
  }

  private def showSubs(contestId: Int, limit: Option[Int], account: Admin)(implicit request: RequestHeader, ec: ExecutionContext) =
    db.run(Contests.getContest(contestId)).map(_.headOption).zip(
      db.run(Contests.getTeams(contestId)).map(_.map(x => x.localId -> x).toMap)
    ).zip(db.run(Submits.getContestSubmits(contestId))).flatMap {
      case ((Some(contest), teamMap), submits) =>
        Submits.groupAndAnnotate(db, contest.schoolMode, limit.map(submits.take).getOrElse(submits)).map { fullyDescribedSubmits =>
          Ok(html.admin.submits(fullyDescribedSubmits, teamMap, contest, account))
        }
      case _ =>
        Future.successful(Redirect(routes.AdminApplication.index))
    }

  def index = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    implicit val ec = StackActionExecutionContext
    showSubs(1, Some(20), loggedIn)
  }

  private val rangeRe = "(\\d*)\\.\\.(\\d*)".r

  private def parseItem(item: String): com.google.common.collect.Range[Integer] =
    if (item.contains("..")) {
      item match {
        case rangeRe(left, right) =>
          if (left.isEmpty) {
            if (right.isEmpty) {
              com.google.common.collect.Range.all[Integer]()
            } else {
              com.google.common.collect.Range.atMost[Integer](right.toInt)
            }
          } else {
            if (right.isEmpty) {
              com.google.common.collect.Range.atLeast[Integer](left.toInt)
            } else {
              com.google.common.collect.Range.closed[Integer](left.toInt, right.toInt)
            }
          }
      }
    } else com.google.common.collect.Range.singleton[Integer](item.toInt)


  private def rejudgeRangeEx(range: String, account: Admin)(implicit ec: ExecutionContext): Future[Seq[Int]] = {
    val checks = {
      val builder = ImmutableRangeSet.builder[Integer]()
      range.split(',').map(parseItem).foreach(builder.add)
      builder.build()
    }

    db.run(sql"""select ID, Contest from NewSubmits order by ID""".as[(Int, Int)]).map { submits =>
      val filtered = submits.filter { id =>
        checks.contains(id._1) && account.canModify(id._2)
      }

      for (id <- filtered) {
        rabbitMq ! QueueMessage(SubmitMessage(id._1), queue = "contester.submitrequests")
      }
      filtered.map(_._1)
    }
  }

  def submits(contestId: Int) = AsyncStack(AuthorityKey -> spectator(contestId)) { implicit request =>
    implicit val ec = StackActionExecutionContext
    showSubs(contestId, None, loggedIn)
  }

  val rejudgeSubmitRangeForm = Form {
    mapping("range" -> text)(RejudgeSubmitRange.apply)(RejudgeSubmitRange.unapply)
  }

  def rejudgePage = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    Future.successful(Ok(html.admin.rejudge(rejudgeSubmitRangeForm)))
  }

  def rejudgeRange = AsyncStack(parse.multipartFormData, AuthorityKey -> anyUser) { implicit request =>
    implicit val ec = StackActionExecutionContext
    rejudgeSubmitRangeForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(html.admin.rejudge(formWithErrors))),
      data => rejudgeRangeEx(data.range, loggedIn).map { rejudged =>
        Redirect(routes.AdminApplication.rejudgePage).flashing(
          "success" -> rejudged.mkString(" ")
        )
      }
    )
  }

  def rejudgeSubmit(submitId: Int) = AsyncStack(parse.multipartFormData, AuthorityKey -> canRejudgeSubmit(submitId)) { implicit request =>
    implicit val ec = StackActionExecutionContext
    rabbitMq ! QueueMessage(SubmitMessage(submitId), queue = "contester.submitrequests")
    db.run(sql"select Contest from NewSubmits where ID = $submitId".as[Int]).map { cids =>
      cids.headOption match {
        case Some(contestId) => Redirect(routes.AdminApplication.submits(contestId))
        case None => Redirect(routes.AdminApplication.index)
      }
    }
  }

  def showSubmit(submitId: Int) = AsyncStack(AuthorityKey -> canSeeSubmit(submitId)) { implicit request =>
    implicit val ec = StackActionExecutionContext
    Submits.getSubmitById(db, submitId).map(x => Ok(html.admin.showsubmit(x)))
  }

  def showQandA(contestId: Int) = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    implicit val ec = StackActionExecutionContext
    db.run(
      sql"""select cl_id, cl_contest_idf, cl_task, cl_text, cl_date, cl_is_hidden from clarifications where cl_contest_idf = $contestId"""
        .as[Clarification1]).zip(db.run(
      sql"""select ID, Contest, Team, Problem, Request, Arrived, Answer, Status from ClarificationRequests where Contest = $contestId"""
        .as[ClarificationRequest1])).map {
      case (clarifications, clReqs) =>
        Ok(html.admin.qanda(clarifications, clReqs))
    }
  }

  val postClarificationForm = Form {
    mapping("id" -> optional(number),
      "contest" -> number,
      "problem" -> text,
      "text" -> nonEmptyText,
      "date" -> optional(jodaDate),
      "isHidden" -> boolean
    )(PostClarification.apply)(PostClarification.unapply)
  }

  def postNewClarification(contestId: Int) = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    Future.successful(Ok(html.admin.postclarification(postClarificationForm, contestId)))
  }

  import utils.Db._

  def postUpdateClarification(clarificationId: Int) = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    implicit val ec = StackActionExecutionContext
    db.run(
      sql"""select cl_id, cl_contest_idf, cl_task, cl_text, cl_date, cl_is_hidden from clarifications
            where cl_id = $clarificationId""".as[Clarification1])
      .map(_.headOption).map { clOpt =>
      clOpt.map { cl =>
        val clObj = PostClarification(
          Some(cl.id), cl.contest, cl.problem, cl.text, Some(cl.date), cl.hidden
        )
        Ok(html.admin.postclarification(postClarificationForm.fill(clObj), cl.contest))
      }.getOrElse(Redirect(routes.AdminApplication.postNewClarification(1)))
    }
  }

  def postClarification = AsyncStack(parse.multipartFormData, AuthorityKey -> anyUser) { implicit request =>
    implicit val ec = StackActionExecutionContext
    postClarificationForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(html.admin.postclarification(formWithErrors, 1))),
      data => {
        val cdate = data.date.getOrElse(DateTime.now)

        val cOp = data.id.map { id =>
          db.run(sqlu"""update clarifications set cl_task = ${data.problem}, cl_text = ${data.text}, cl_date = $cdate,
              cl_is_hidden = ${data.hidden} where cl_id = $id""").map(_ => None)
        }.getOrElse(
            db.run(
              sqlu"""insert into clarifications (cl_contest_idf, cl_task, cl_text, cl_date, cl_is_hidden) values
                 (${data.contest}, ${data.problem}, ${data.text}, $cdate, ${data.hidden})
                  """.andThen(sql"select last_insert_id".as[Int]).withPinnedSession).map(_.headOption))

        cOp.map { optId =>
          Redirect(routes.AdminApplication.postNewClarification(1))
        }
      }
    )
  }

  private val clarificationResponseForm = Form {
    mapping("answer" -> text)(ClarificationResponse.apply)(ClarificationResponse.unapply)
  }

  private def getClrById(clrId: Int)(implicit ec: ExecutionContext) =
    db.run(sql"""select ID, Contest, Team, Problem, Request, Arrived, Answer, Status from ClarificationRequests where ID = $clrId"""
      .as[ClarificationRequest1]).map(_.headOption)

  private val answerList = Map(
    "No comments" -> "No comments",
    "Yes" -> "Yes",
    "No" -> "No",
    "Pending" -> "Pending"
  )

  def postAnswerForm(clrId: Int) = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    implicit val ec = StackActionExecutionContext
    getClrById(clrId).map { optClr =>
      optClr.map { clr =>
        BadRequest(html.admin.postanswer(
          clarificationResponseForm.fill(ClarificationResponse(clr.answer)), clr, answerList.toSeq))
      }.getOrElse(Redirect(routes.AdminApplication.showQandA(1)))
    }
  }

  def postAnswer(clrId: Int) = AsyncStack(parse.multipartFormData, AuthorityKey -> anyUser) { implicit request =>
    implicit val ec = StackActionExecutionContext
    getClrById(clrId).flatMap { optClr =>
      optClr.map { clr =>
        clarificationResponseForm.bindFromRequest.fold(
          formWithErrors => Future.successful(BadRequest(html.admin.postanswer(formWithErrors, clr, answerList.toSeq))),
          data => {
            db.run(sqlu"""update ClarificationRequests set Answer = ${data.answer}, Status = 1 where ID = $clrId""").map { _ =>
              Redirect(routes.AdminApplication.showQandA(clr.contest))
            }
          }
        )
      }.getOrElse(Future.successful(Redirect(routes.AdminApplication.showQandA(1))))
    }
  }
}