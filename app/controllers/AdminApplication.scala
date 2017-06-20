package controllers

import javax.inject.{Inject, Singleton}

import actors.{StatusActor, WaiterActor}
import actors.StatusActor.ClarificationAnswered
import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import com.github.nscala_time.time.Imports._
import com.google.common.collect.ImmutableRangeSet
import com.spingo.op_rabbit.Message
import jp.t2v.lab.play2.auth.AuthElement
import models._
import play.api.{Configuration, Logger}
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.http.ContentTypes
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.EventSource
import play.api.libs.EventSource.Event
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc.{Action, AnyContent, Controller, RequestHeader}
import slick.driver.JdbcProfile
import utils.{Ask, PolygonURL}
import views.html

import scala.concurrent.duration.{Duration, _}
import scala.concurrent.{ExecutionContext, Future}

case class RejudgeSubmitRange(range: String)
case class PostClarification(problem: String, text: String, hidden: Boolean)
case class ClarificationResponse(answer: String)

case class PostWaiterTask(message: String, rooms: String)

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
                                  waiterActorModel: WaiterActorModel,
                                  configuration: Configuration,
                                  ws: WSClient,
                             val auth: AuthWrapper,
                             val messagesApi: MessagesApi) extends Controller with AuthElement with AdminAuthConfigImpl with I18nSupport{

  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import com.spingo.op_rabbit.PlayJsonSupport._

  private val rabbitMq = rabbitMqModel.rabbitMq

  private val fileserverUrl = configuration.getString("fileserver.url")
  private val shortn = configuration.getString("fileserver.shortn")

  def monitor(id: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canSpectate(id)) { implicit request =>
    import Contexts.adminExecutionContext
    getSelectedContests(id, loggedIn).zip(monitorModel.getMonitor(id, loggedIn.canSeeAll(id))).map {
      case (contest, status) => Ok(html.admin.monitor(contest, status.get.status))
    }
  }

  import slick.driver.MySQLDriver.api._

  private def getSubmitCid(submitId: Int)(implicit ec: ExecutionContext) =
    db.run(sql"""select Contest from NewSubmits where ID = $submitId""".as[Int]).map(_.headOption)

  private def canSeeSubmit(submitId: Int)(account: Admin): Future[Boolean] = {
    import Contexts.adminExecutionContext

    getSubmitCid(submitId).map { cids =>
      cids.exists(account.canSpectate(_))
    }
  }

  private def canRejudgeSubmit(submitId: Int)(account: Admin): Future[Boolean] = {
    import Contexts.adminExecutionContext

    getSubmitCid(submitId).map { cids =>
      cids.exists(account.canModify(_))
    }
  }

  private def showSubs(contestId: Int, limit: Option[Int], account: Admin)(implicit request: RequestHeader, ec: ExecutionContext) =
    getSelectedContests(contestId, account).zip(
      db.run(Contests.getTeams(contestId)).map(_.map(x => x.localId -> x).toMap)
    ).zip(db.run(Submits.getContestSubmits(contestId))).flatMap {
      case ((contest, teamMap), submits0) =>
        val canSeeAll = account.canSeeAll(contestId)
        def canSeeSubmit(s: Submit) =
          if (canSeeAll) true else !s.afterFreeze
        val submits = submits0.filter(canSeeSubmit)
        Submits.groupAndAnnotate(db, contest.contest.schoolMode, limit.map(submits.reverse.take(_).reverse).getOrElse(submits)).map { fullyDescribedSubmits =>
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
    Future.successful(Redirect(routes.AdminApplication.submits(loggedIn.defaultContest)))
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
    import Contexts.adminExecutionContext

    val lim = if (loggedIn.canModify(contestId))
      None
    else
      Some(100)
    showSubs(contestId, lim, loggedIn)
  }

  val rejudgeSubmitRangeForm = Form {
    mapping("range" -> text)(RejudgeSubmitRange.apply)(RejudgeSubmitRange.unapply)
  }

  def rejudgePage(contestId: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canModify(contestId)) { implicit request =>
    import Contexts.adminExecutionContext
    getSelectedContests(contestId, loggedIn).map { contest =>
      Ok(html.admin.rejudge(rejudgeSubmitRangeForm, contest))
    }
  }

  def rejudgeRange(contestId: Int) = AsyncStack(parse.multipartFormData, AuthorityKey -> AdminPermissions.canModify(contestId)) { implicit request =>
    import Contexts.adminExecutionContext
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
    import Contexts.adminExecutionContext
    rabbitMq ! Message.queue(SubmitMessage(submitId), queue = "contester.submitrequests")
    db.run(sql"select Contest from NewSubmits where ID = $submitId".as[Int]).map { cids =>
      cids.headOption match {
        case Some(contestId) => Redirect(routes.AdminApplication.submits(contestId))
        case None => Redirect(routes.AdminApplication.index)
      }
    }
  }

  def showSubmit(contestId: Int, submitId: Int) = AsyncStack(AuthorityKey -> canSeeSubmit(submitId)) { implicit request =>
    import Contexts.adminExecutionContext

    Submits.getSubmitById(db, submitId).flatMap { optSubmit =>
      optSubmit.map { submit =>
        submit.fsub.submit.testingId.map { testingId =>
          db.run(sql"select ID, Submit, Start, Finish, ProblemID from Testings where ID = $testingId".as[Testing])
            .map(_.headOption)
        }.getOrElse(Future.successful(None)).flatMap { testingOpt =>
          testingOpt.flatMap { testing =>
            testing.problemId.map { problemId =>
              val phandle = PolygonURL(problemId)
              Outputs.getAllAssets(ws, fileserverUrl.get, shortn.get, submit.fsub.submit.submitId.id, testing.id, submit.fsub.details.map(_.test), phandle)
            }
          }.getOrElse(Future.successful(Map[Int, ResultAssets]()))
        }.zip(
        getSelectedContests(contestId, loggedIn)).map {
          case (outputs, contest) =>
            Ok(html.admin.showsubmit(submit, contest, outputs))
        }
      }.getOrElse(Future.successful(Redirect(routes.AdminApplication.submits(contestId))))
    }
  }

  def reprintSubmit(submitId: Int) = AsyncStack(AuthorityKey -> canRejudgeSubmit(submitId)) { implicit request =>
    rabbitMq ! Message.queue(SubmitTicketLite(SubmitIdLite(submitId)), queue = "contester.tickets")
    Future.successful(Ok("ok"))
  }

  private def getAllWaiterTasks(perm: WaiterPermissions)(implicit ec: ExecutionContext) =
    Ask[WaiterActor.Snapshot](waiterActorModel.waiterActor, WaiterActor.GetSnapshot(perm)).map(_.tasks)

  def showQandA(contestId: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canSpectate(contestId)) { implicit request =>
    import Contexts.adminExecutionContext
    ClarificationModel.getClarifications(db, contestId)
      .zip(ClarificationModel.getClarificationReqs(db, contestId))
      .zip(getSelectedContests(contestId, loggedIn))
        .zip(getAllWaiterTasks(loggedIn))
      .map {
      case (((clarifications, clReqs), contest), tasks) =>
        Ok(html.admin.qanda(tasks, clarifications, clReqs, contest))
    }
  }

  def tasks(contestId: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canSpectate(contestId)) { implicit request =>
    import Contexts.adminExecutionContext
    getSelectedContests(contestId, loggedIn)
      .zip(getAllWaiterTasks(loggedIn))
      .map {
        case (contest, tasks) =>
          Ok(html.admin.waitertasksmain(tasks, contest, loggedIn))
      }
  }

  def toggleClarification(clrId: Int) = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    import Contexts.adminExecutionContext
    ClarificationModel.toggleClarification(db, clrId).map { _ =>
      Ok("ok")
    }
  }

  def deleteClarification(contestId: Int, clrId: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canModify(contestId)) { implicit request =>
    import Contexts.adminExecutionContext
    ClarificationModel.deleteClarification(db, clrId).map { _ =>
      Ok("ok")
    }
  }

  private implicit val standardTimeout: akka.util.Timeout = {
    import scala.concurrent.duration._
    Duration(5, SECONDS)
  }

  private def joinAdminFeed(contestId: Int, perm: WaiterPermissions, requestHeader: RequestHeader) = {
    import Contexts.adminExecutionContext

/*    Ask[Enumerator[Event]](statusActorModel.statusActor, StatusActor.JoinAdmin(contestId)).zip(
      Ask[Enumerator[Event]](waiterActorModel.waiterActor, WaiterActor.Join(perm, requestHeader))).map {
      case (one, two) =>
        Enumerator.interleave(one, two)
    }*/
    Ask[Source[Event, NotUsed]](statusActorModel.statusActor, StatusActor.JoinAdmin(contestId))
  }

  def feed(contestId: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canSpectate(contestId)) { implicit request =>
    import Contexts.adminExecutionContext
    joinAdminFeed(contestId, loggedIn, request).map { e =>
      Ok.chunked(e).as(ContentTypes.EVENT_STREAM)
    }
  }

  private val postClarificationForm = Form {
    mapping("problem" -> text,
      "text" -> nonEmptyText,
      "isHidden" -> boolean
    )(PostClarification.apply)(PostClarification.unapply)
  }

  def postNewClarification(contestId: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canModify(contestId)) { implicit request =>
    import Contexts.adminExecutionContext
    getSelectedContests(contestId, loggedIn).map { contest =>
      Ok(html.admin.postclarification(None, postClarificationForm, contest))
    }
  }

  def postUpdateClarification(clarificationId: Int) = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    import Contexts.adminExecutionContext
    ClarificationModel.getClarification(db, clarificationId).flatMap { clOpt =>
      clOpt.map { cl =>
        val clObj = PostClarification(
          cl.problem, cl.text, cl.hidden
        )
        getSelectedContests(cl.contest, loggedIn).map { contest =>
          Ok(html.admin.postclarification(cl.id, postClarificationForm.fill(clObj), contest))
        }
      }.getOrElse(Future.successful(Redirect(routes.AdminApplication.postNewClarification(1))))
    }
  }

  def postClarification(contestId: Int, clarificationId: Option[Int]) = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    import Contexts.adminExecutionContext
    import utils.Db._
    postClarificationForm.bindFromRequest.fold(
      formWithErrors => getSelectedContests(1, loggedIn).map { contest =>
        BadRequest(html.admin.postclarification(clarificationId, formWithErrors, contest))
      },
      data => {
        Ask.apply[Clarification](statusActorModel.statusActor,
          Clarification(clarificationId, contestId, data.problem.toUpperCase, data.text, DateTime.now, data.hidden))
          .map { next =>
          Redirect(routes.AdminApplication.showQandA(contestId))
        }
      }
    )
  }

  private val clarificationResponseForm = Form {
    mapping("answer" -> text)(ClarificationResponse.apply)(ClarificationResponse.unapply)
  }

  private val answerList = Map(
    "No comments" -> "No comments",
    "Yes" -> "Yes",
    "No" -> "No",
    "Pending" -> "Pending"
  )

  def postAnswerForm(clrId: Int) = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    import Contexts.adminExecutionContext
    ClarificationModel.getClarificationReq(db, clrId).flatMap { optClr =>
      optClr.map { clr =>
        getSelectedContests(clr.contest, loggedIn).map { contest =>
          Ok(html.admin.postanswer(
            clarificationResponseForm.fill(ClarificationResponse(clr.getAnswer)), clr, answerList.toSeq, contest))
        }
      }.getOrElse(Future.successful(Redirect(routes.AdminApplication.showQandA(1))))
    }
  }

  def postAnswer(clrId: Int) = AsyncStack(parse.multipartFormData, AuthorityKey -> Permissions.any) { implicit request =>
    import Contexts.adminExecutionContext
    ClarificationModel.getClarificationReq(db, clrId).flatMap { optClr =>
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

  private val waiterTaskForm = Form {
    mapping("message" -> nonEmptyText, "rooms" -> text)(PostWaiterTask.apply)(PostWaiterTask.unapply)
  }

  def postNewWaiterTaskForm(contestId: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canCreateTasks) { implicit request =>
    import Contexts.adminExecutionContext
    getSelectedContests(contestId, loggedIn).map { contest =>
      Ok(html.admin.postwaitertask(None, waiterTaskForm, contest))
    }
  }

  def postWaiterTask(contestId: Int, id: Option[Int]) = AsyncStack(AuthorityKey -> AdminPermissions.canCreateTasks) { implicit request =>
    import Contexts.adminExecutionContext
    waiterTaskForm.bindFromRequest.fold(
      formWithErrors => getSelectedContests(contestId, loggedIn).map { contest =>
        BadRequest(html.admin.postwaitertask(id, formWithErrors, contest))
      },
      data => {
        Ask.apply[StoredWaiterTask](waiterActorModel.waiterActor, WaiterActor.NewTask(data.message, Nil)).map { posted =>
          Redirect(routes.AdminApplication.tasks(contestId))
        }
      }
    )
  }

  def ackWaiterTask(id: Long, room: String) = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    import Contexts.adminExecutionContext

    waiterActorModel.waiterActor ! WaiterActor.AckTask(id, room)
    Future.successful(Ok("ok"))
  }

  def unackWaiterTask(id: Long, room: String) = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    import Contexts.adminExecutionContext

    waiterActorModel.waiterActor ! WaiterActor.UnackTask(id, room)
    Future.successful(Ok("ok"))
  }

  def deleteWaiterTask(id: Long) = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    import Contexts.adminExecutionContext

    waiterActorModel.waiterActor ! WaiterActor.DeleteTask(id)
    Future.successful(Ok("ok"))
  }
}