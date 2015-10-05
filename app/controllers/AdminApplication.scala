package controllers

import javax.inject.{Inject, Singleton}

import actors.StatusActor
import actors.StatusActor.ClarificationAnswered
import akka.actor.{ActorSystem, Props}
import com.google.common.collect.ImmutableRangeSet
import com.spingo.op_rabbit.{Message, RabbitControl}
import jp.t2v.lab.play2.auth.AuthElement
import models._
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.EventSource
import play.api.libs.iteratee.{Enumeratee, Concurrent}
import play.api.libs.json.{Json, JsValue}
import play.api.mvc.{Action, Controller, RequestHeader}
import slick.driver.JdbcProfile
import slick.jdbc.GetResult
import views.html

import scala.concurrent.{ExecutionContext, Future}

import com.github.nscala_time.time.Imports._

case class RejudgeSubmitRange(range: String)
case class PostClarification(id: Option[Int], contest: Int, problem: String, text: String, date: Option[DateTime], hidden: Boolean)

case class ClarificationResponse(answer: String)

case class SubmitIdLite(id: Int)
object SubmitIdLite {
  implicit val format = Json.format[SubmitIdLite]
}

case class SubmitTicketLite(submit: SubmitIdLite)
object SubmitTicketLite {
  implicit val format = Json.format[SubmitTicketLite]
}

@Singleton
class AdminApplication @Inject() (dbConfigProvider: DatabaseConfigProvider,
                             monitorModel: Monitor,
                                 rabbitMqModel: RabbitMqModel,
                             statusActorModel: StatusActorModel,
                             val auth: AuthWrapper,
                             val messagesApi: MessagesApi) extends Controller with AuthElement with AdminAuthConfigImpl with I18nSupport{

  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import com.spingo.op_rabbit.PlayJsonSupport._

  private val rabbitMq = rabbitMqModel.rabbitMq

  def monitor(id: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canSpectate(id)) { implicit request =>
    implicit val ec = StackActionExecutionContext

    getSelectedContests(id, loggedIn).zip(monitorModel.getMonitor(id, true)).map {
      case (contest, status) => Ok(html.admin.monitor(contest, status.get.status))
    }
  }

  import slick.driver.MySQLDriver.api._

  private def getSubmitCid(submitId: Int)(implicit ec: ExecutionContext) =
    db.run(sql"""select Contest from NewSubmits where ID = $submitId""".as[Int]).map(_.headOption)

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
    getSelectedContests(contestId, account).zip(
      db.run(Contests.getTeams(contestId)).map(_.map(x => x.localId -> x).toMap)
    ).zip(db.run(Submits.getContestSubmits(contestId))).flatMap {
      case ((contest, teamMap), submits) =>
        Submits.groupAndAnnotate(db, contest.contest.schoolMode, limit.map(submits.take).getOrElse(submits)).map { fullyDescribedSubmits =>
          Ok(html.admin.submits(fullyDescribedSubmits, teamMap, contest, account))
        }
      case _ =>
        Future.successful(Redirect(routes.AdminApplication.index))
    }

  private def getSelectedContests(contestId: Int, account: Admin)(implicit ec: ExecutionContext): Future[SelectedContest] =
    db.run(Contests.getContests).map(_.filter(c => account.canSpectate(c.id)).sortBy(_.id)).map { contests =>
      val cmap = contests.map(x => x.id -> x).toMap
      SelectedContest(cmap.getOrElse(contestId, contests.head), cmap.mapValues(_.name).toSeq)
    }

  def index = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    implicit val ec = StackActionExecutionContext
    Future.successful(Redirect(routes.AdminApplication.submits(1)))
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
        rabbitMq ! Message.queue(SubmitMessage(id._1), queue = "contester.submitrequests")
      }
      filtered.map(_._1)
    }
  }

  def submits(contestId: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canSpectate(contestId)) { implicit request =>
    implicit val ec = StackActionExecutionContext
    showSubs(contestId, None, loggedIn)
  }

  val rejudgeSubmitRangeForm = Form {
    mapping("range" -> text)(RejudgeSubmitRange.apply)(RejudgeSubmitRange.unapply)
  }

  def rejudgePage(contestId: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canModify(contestId)) { implicit request =>
    implicit val ec = StackActionExecutionContext
    getSelectedContests(contestId, loggedIn).map { contest =>
      Ok(html.admin.rejudge(rejudgeSubmitRangeForm, contest))
    }
  }

  def rejudgeRange(contestId: Int) = AsyncStack(parse.multipartFormData, AuthorityKey -> AdminPermissions.canModify(contestId)) { implicit request =>
    implicit val ec = StackActionExecutionContext
    rejudgeSubmitRangeForm.bindFromRequest.fold(
      formWithErrors => getSelectedContests(contestId, loggedIn).map { contest =>
        BadRequest(html.admin.rejudge(formWithErrors, contest))
      },
      data => rejudgeRangeEx(data.range, loggedIn).map { rejudged =>
        Redirect(routes.AdminApplication.rejudgePage(contestId)).flashing(
          "success" -> rejudged.mkString(" ")
        )
      }
    )
  }

  def rejudgeSubmit(submitId: Int) = AsyncStack(AuthorityKey -> canRejudgeSubmit(submitId)) { implicit request =>
    implicit val ec = StackActionExecutionContext
    rabbitMq ! Message.queue(SubmitMessage(submitId), queue = "contester.submitrequests")
    db.run(sql"select Contest from NewSubmits where ID = $submitId".as[Int]).map { cids =>
      cids.headOption match {
        case Some(contestId) => Redirect(routes.AdminApplication.submits(contestId))
        case None => Redirect(routes.AdminApplication.index)
      }
    }
  }

  def showSubmit(submitId: Int) = AsyncStack(AuthorityKey -> canSeeSubmit(submitId)) { implicit request =>
    implicit val ec = StackActionExecutionContext
    Submits.getSubmitById(db, submitId).flatMap { submit =>
      val cid = submit.map(_.fsub.submit.submitId.contestId).getOrElse(1)
      getSelectedContests(cid, loggedIn).map { contest =>
        Ok(html.admin.showsubmit(submit, contest))
      }
    }
  }

  def reprintSubmit(submitId: Int) = AsyncStack(AuthorityKey -> canRejudgeSubmit(submitId)) { implicit request =>
    implicit val ec = StackActionExecutionContext

    rabbitMq ! Message.queue(SubmitTicketLite(SubmitIdLite(submitId)), queue = "contester.tickets")
    Future.successful(Ok("ok"))
  }

  def showQandA(contestId: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canSpectate(contestId)) { implicit request =>
    implicit val ec = StackActionExecutionContext
    db.run(
      sql"""select cl_id, cl_contest_idf, cl_task, cl_text, cl_date, cl_is_hidden from clarifications
             where cl_contest_idf = $contestId order by cl_date desc"""
        .as[Clarification]).zip(db.run(
      sql"""select ID, Contest, Team, Problem, Request, Answer, Arrived, Status from ClarificationRequests
               where Contest = $contestId order by Arrived desc"""
        .as[ClarificationRequest])).zip(getSelectedContests(contestId, loggedIn)).map {
      case ((clarifications, clReqs), contest) =>
        Ok(html.admin.qanda(clarifications, clReqs, contest))
    }
  }

  private def getClarificationById(clrId: Int) =
    db.run(sql"""select cl_id, cl_contest_idf, cl_task, cl_text, cl_date, cl_is_hidden from clarifications
            where cl_id = $clrId""".as[Clarification])

  def toggleClarification(clrId: Int) = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    implicit val ec = StackActionExecutionContext

    getClarificationById(clrId).flatMap { clrs =>
      Future.sequence(clrs.map { clr =>
        db.run(sqlu"update clarifications set cl_is_hidden = ${!clr.hidden} where cl_id = ${clr.id}")
      })
    }.map { _ =>
      Ok("ok")
    }
  }

  def deleteClarification(clrId: Int) = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    implicit val ec = StackActionExecutionContext

    db.run(sqlu"delete from clarifications where cl_id = ${clrId}").map { _ =>
      Ok("ok")
    }
  }


  def feed(contestId: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canSpectate(contestId)) { implicit request =>
    implicit val ec = StackActionExecutionContext

    import scala.concurrent.duration._
    import akka.pattern.ask

    statusActorModel.statusActor.ask(StatusActor.JoinAdmin(contestId))(Duration(5, SECONDS)).map {
      case StatusActor.AdminJoined(e) => {
        Ok.feed(e).as("text/event-stream")
      }
      case _ => BadRequest("foo")
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

  def postNewClarification(contestId: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canModify(contestId)) { implicit request =>
    implicit val ec = StackActionExecutionContext
    getSelectedContests(contestId, loggedIn).map { contest =>
      Ok(html.admin.postclarification(postClarificationForm, contest))
    }
  }

  import utils.Db._

  def postUpdateClarification(clarificationId: Int) = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    implicit val ec = StackActionExecutionContext
    db.run(
      sql"""select cl_id, cl_contest_idf, cl_task, cl_text, cl_date, cl_is_hidden from clarifications
            where cl_id = $clarificationId""".as[Clarification])
      .map(_.headOption).flatMap { clOpt =>
      clOpt.map { cl =>
        val clObj = PostClarification(
          Some(cl.id), cl.contest, cl.problem, cl.text, Some(cl.arrived), cl.hidden
        )
        getSelectedContests(cl.contest, loggedIn).map { contest =>
          Ok(html.admin.postclarification(postClarificationForm.fill(clObj), contest))
        }
      }.getOrElse(Future.successful(Redirect(routes.AdminApplication.postNewClarification(1))))
    }
  }

  def postClarification(contestId: Int) = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    implicit val ec = StackActionExecutionContext
    postClarificationForm.bindFromRequest.fold(
      formWithErrors => getSelectedContests(1, loggedIn).map { contest =>
        BadRequest(html.admin.postclarification(formWithErrors, contest))
      },
      data => {
        val cdate = data.date.getOrElse(DateTime.now)

        val cOp = data.id.map { id =>
          db.run(sqlu"""update clarifications set cl_task = ${data.problem}, cl_text = ${data.text}, cl_date = $cdate,
              cl_is_hidden = ${data.hidden} where cl_id = $id""").map(_ => None)
        }.getOrElse(
            db.run(
              sqlu"""insert into clarifications (cl_contest_idf, cl_task, cl_text, cl_date, cl_is_hidden) values
                 (${data.contest}, ${data.problem}, ${data.text}, $cdate, ${data.hidden})
                  """.andThen(sql"select last_insert_id()".as[Int]).withPinnedSession).map(_.headOption))

        cOp.map { optId =>
          Redirect(routes.AdminApplication.showQandA(contestId))
        }
      }
    )
  }

  private val clarificationResponseForm = Form {
    mapping("answer" -> text)(ClarificationResponse.apply)(ClarificationResponse.unapply)
  }

  private def getClrById(clrId: Int)(implicit ec: ExecutionContext) =
    db.run(sql"""select ID, Contest, Team, Problem, Request, Answer, Arrived, Status from ClarificationRequests
                where ID = $clrId"""
      .as[ClarificationRequest]).map(_.headOption)

  private val answerList = Map(
    "No comments" -> "No comments",
    "Yes" -> "Yes",
    "No" -> "No",
    "Pending" -> "Pending"
  )

  def postAnswerForm(clrId: Int) = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    implicit val ec = StackActionExecutionContext
    getClrById(clrId).flatMap { optClr =>
      optClr.map { clr =>
        getSelectedContests(clr.contest, loggedIn).map { contest =>
          Ok(html.admin.postanswer(
            clarificationResponseForm.fill(ClarificationResponse(clr.answer)), clr, answerList.toSeq, contest))
        }
      }.getOrElse(Future.successful(Redirect(routes.AdminApplication.showQandA(1))))
    }
  }

  def postAnswer(clrId: Int) = AsyncStack(parse.multipartFormData, AuthorityKey -> Permissions.any) { implicit request =>
    implicit val ec = StackActionExecutionContext
    getClrById(clrId).flatMap { optClr =>
      optClr.map { clr =>
        clarificationResponseForm.bindFromRequest.fold(
          formWithErrors => getSelectedContests(clr.contest, loggedIn).map { contest =>
            BadRequest(html.admin.postanswer(formWithErrors, clr, answerList.toSeq, contest))
          },
          data => {
            db.run(sqlu"""update ClarificationRequests set Answer = ${data.answer}, Status = 1 where ID = $clrId""").map { _ =>
              statusActorModel.statusActor ! ClarificationAnswered(clr.contest, clrId, clr.team, clr.problem, data.answer)
              Redirect(routes.AdminApplication.showQandA(clr.contest))
            }
          }
        )
      }.getOrElse(Future.successful(Redirect(routes.AdminApplication.showQandA(1))))
    }
  }
}