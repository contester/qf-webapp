package controllers

import javax.inject.{Inject, Singleton}

import actors.{StatusActor, WaiterActor}
import actors.StatusActor.ClarificationAnswered
import akka.stream.Materializer
import com.github.nscala_time.time.Imports._
import com.google.common.collect.ImmutableRangeSet
import com.spingo.op_rabbit.Message
import jp.t2v.lab.play2.auth.AuthElement
import models._
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.EventSource.Event
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc.{Controller, RequestHeader}
import slick.driver.JdbcProfile
import utils.PolygonURL
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
                                  implicit val mat: Materializer,
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
    getSelectedContests(id, loggedIn).zip(monitorModel.getMonitor(id, true)).map {
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
      case ((contest, teamMap), submits) =>
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

  private def getClarifications(contestId: Int)(implicit ec: ExecutionContext) =
    db.run(
      sql"""select cl_id, cl_contest_idf, cl_task, cl_text, cl_date, cl_is_hidden from clarifications
             where cl_contest_idf = $contestId order by cl_date desc"""
        .as[Clarification])

  private def getClarificationReqs(contestId: Int)(implicit ec: ExecutionContext) =
    db.run(
      sql"""select ID, Contest, Team, Problem, Request, Answer, Arrived, Status from ClarificationRequests
               where Contest = $contestId order by Arrived desc"""
        .as[ClarificationRequest])

  def showQandA(contestId: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canSpectate(contestId)) { implicit request =>
    import Contexts.adminExecutionContext
    getClarifications(contestId)
      .zip(getClarificationReqs(contestId))
      .zip(getSelectedContests(contestId, loggedIn))
        .zip(waiterActorModel.getSnapshot(loggedIn.locations.toList))
      .map {
      case (((clarifications, clReqs), contest), tasks) =>
        Ok(html.admin.qanda(tasks, clarifications, clReqs, contest))
    }
  }

  private def getClarificationById(clrId: Int) =
    db.run(sql"""select cl_id, cl_contest_idf, cl_task, cl_text, cl_date, cl_is_hidden from clarifications
            where cl_id = $clrId""".as[Clarification])

  def toggleClarification(clrId: Int) = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    import Contexts.adminExecutionContext

    getClarificationById(clrId).flatMap { clrs =>
      Future.sequence(clrs.map { clr =>
        db.run(sqlu"update clarifications set cl_is_hidden = ${!clr.hidden} where cl_id = ${clr.id}")
      })
    }.map { _ =>
      Ok("ok")
    }
  }

  def deleteClarification(contestId: Int, clrId: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canModify(contestId)) { implicit request =>
    import Contexts.adminExecutionContext
    db.run(sqlu"delete from clarifications where cl_id = ${clrId} and cl_contest_idf = ${contestId}").map { _ =>
      Ok("ok")
    }
  }

  private val standardTimeout = {
    import scala.concurrent.duration._
    Duration(5, SECONDS)
  }

  private def joinAdminFeed(contestId: Int, rooms: List[String], requestHeader: RequestHeader): Future[Enumerator[Event]] = {
    import Contexts.adminExecutionContext
    import akka.pattern.ask

    statusActorModel.statusActor.ask(StatusActor.JoinAdmin(contestId))(standardTimeout).mapTo[StatusActor.AdminJoined].zip(
      waiterActorModel.join(rooms, requestHeader)).map {
      case (one, two) =>
        Enumerator.interleave(one.enumerator, two)
    }
  }

  def feed(contestId: Int) = AsyncStack(AuthorityKey -> AdminPermissions.canSpectate(contestId)) { implicit request =>
    import Contexts.adminExecutionContext
    joinAdminFeed(contestId, loggedIn.locations.toList, request).map { e =>
      Ok.feed(e).as("text/event-stream")
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
    db.run(
      sql"""select cl_id, cl_contest_idf, cl_task, cl_text, cl_date, cl_is_hidden from clarifications
            where cl_id = $clarificationId""".as[Clarification])
      .map(_.headOption).flatMap { clOpt =>
      clOpt.map { cl =>
        val clObj = PostClarification(
          cl.problem, cl.text, cl.hidden
        )
        getSelectedContests(cl.contest, loggedIn).map { contest =>
          Ok(html.admin.postclarification(Some(cl.id), postClarificationForm.fill(clObj), contest))
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
        val cdate = DateTime.now

        val cOp = clarificationId.map { id =>
          db.run(sqlu"""update clarifications set cl_task = ${data.problem}, cl_text = ${data.text},
              cl_is_hidden = ${data.hidden} where cl_id = $id""").map(_ => clarificationId)
        }.getOrElse(
            db.run(
              sqlu"""insert into clarifications (cl_contest_idf, cl_task, cl_text, cl_date, cl_is_hidden) values
                 (${contestId}, ${data.problem}, ${data.text}, $cdate, ${data.hidden})
                  """.andThen(sql"select last_insert_id()".as[Int]).withPinnedSession).map(_.headOption))

        cOp.map { optId =>
          for (realId <- optId) {
            val popt = if (data.problem.isEmpty) None else Some(data.problem.toUpperCase)
                statusActorModel.statusActor ! StatusActor.ClarificationUpdated(realId, contestId, cdate, popt, data.text)
          }
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
    import Contexts.adminExecutionContext
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
    import Contexts.adminExecutionContext
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
        import akka.pattern.ask

        waiterActorModel.waiterActor.ask(WaiterActor.NewTask(data.message, Nil))(standardTimeout)
          .mapTo[StoredWaiterTask].map { posted =>
          Redirect(routes.AdminApplication.showQandA(contestId))
        }
      }
    )
  }

  def ackWaiterTask(id: Long, room: String) = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    import Contexts.adminExecutionContext

    waiterActorModel.waiterActor ! WaiterActor.AckTask(id, room)
    Future.successful(Ok("ok"))
  }
}