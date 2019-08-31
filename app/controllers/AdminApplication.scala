package controllers

import javax.inject.{Inject, Singleton}
import actors.{StatusActor, WaiterActor}
import actors.StatusActor.ClarificationAnswered
import akka.NotUsed
import akka.stream.scaladsl.{Merge, Source}
import com.github.nscala_time.time.Imports._
import com.google.common.collect.ImmutableRangeSet
import com.mohiva.play.silhouette.api.{Authorization, Silhouette}
import com.mohiva.play.silhouette.impl.authenticators.SessionAuthenticator
import com.spingo.op_rabbit.Message
import models._
import play.api.{Configuration, Logger, Logging}
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.http.ContentTypes
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.EventSource.Event
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc._
import slick.basic.DatabaseConfig
import slick.dbio.Effect
import slick.jdbc.{GetResult, JdbcProfile}
import slick.sql.FixedSqlAction
import utils.auth.{AdminEnv, TeamsEnv}
import utils.{Ask, PolygonURL}
import views.html

import scala.concurrent.duration.{Duration, _}
import scala.concurrent.{ExecutionContext, Future}

case class RejudgeSubmitRange(range: String)
case class PostClarification(problem: String, text: String, hidden: Boolean)
case class ClarificationResponse(answer: String)

case class PostWaiterTask(message: String, rooms: String)

case class TeamDescription(id: Int, school: School)

case class EditTeam(schoolName: String, teamName: String, teamNum: Option[Int])

case class DisplayTeam(team: SlickModel.Team, school: School)

case class AdminPrintJob(id: Int, contestID: Int, team: Team, filename: String, arrived: DateTime, printed: Boolean, computerName: String)

case class SubmitIdLite(id: Int)
object SubmitIdLite {
  implicit val format = Json.format[SubmitIdLite]
}

case class SubmitTicketLite(submit: SubmitIdLite)
object SubmitTicketLite {
  implicit val format = Json.format[SubmitTicketLite]
}

@Singleton
class AdminApplication (cc: ControllerComponents,
                        silhouette: Silhouette[AdminEnv],
                        dbConfig: DatabaseConfig[JdbcProfile],
                             monitorModel: Monitor,
                                 rabbitMqModel: RabbitMqModel,
                             statusActorModel: StatusActorModel,
                                  configuration: Configuration,
                                  ws: WSClient) extends AbstractController(cc)  with I18nSupport with Logging {

  private val db = dbConfig.db
  import com.spingo.op_rabbit.PlayJsonSupport._

  private val rabbitMq = rabbitMqModel.rabbitMq

  private val fileserverUrl = configuration.get[String]("fileserver.url")
  private val shortn = configuration.get[String]("fileserver.shortn")

  private val fileserverBase = fileserverUrl.stripSuffix("fs/")

  logger.info(s"fileserverBase: $fileserverBase")

  import slick.jdbc.MySQLProfile.api._

  private def getSubmitCidQuery(submitId: Int) =
    SlickModel.newSubmits.filter(_.id === submitId).take(1).map(_.contest).result.headOption

  private def getSubmitCid(submitId: Int)(implicit ec: ExecutionContext): Future[Option[Int]] =
    db.run(getSubmitCidQuery(submitId))

  private def rejudgeRangeEx(range: String, account: Admin)(implicit ec: ExecutionContext): Future[Seq[Int]] = {
    val checks = {
      val builder = ImmutableRangeSet.builder[Integer]()
      range.split(',').map(parseItem).foreach(builder.add)
      builder.build()
    }
    db.run(SlickModel.newSubmits.map(x => (x.id, x.contest)).sortBy(_._1).result).map { submits =>
      val filtered = submits.filter { id =>
        checks.contains(id._1) && account.canModify(id._2)
      }

      for (id <- filtered) {
        rabbitMq ! Message.queue(SubmitMessage(id._1), queue = "contester.submitrequests")
      }
      filtered.map(_._1)
    }
  }

  private def showSubs(contestId: Int, limit: Option[Int], account: Admin)(implicit request: RequestHeader, ec: ExecutionContext) =
    getSelectedContests(contestId, account).zip(
      monitorModel.teamClient.getTeams(contestId)
    ).zip(db.run(Submits.getContestSubmits(contestId))).flatMap {
      case ((contest, teamMap), submits0) =>
        val canSeeAll = account.canSeeAll(contestId)
        def canSeeSubmit(s: Submit) =
          if (canSeeAll) true else !s.afterFreeze
        val submits = submits0.filter(canSeeSubmit)
        Submits.groupAndAnnotate(db, contest.contest.schoolMode, limit.map(submits.takeRight(_)).getOrElse(submits)).map { fullyDescribedSubmits =>
          Ok(html.admin.submits(fullyDescribedSubmits, teamMap, contest, account))
        }
      case _ =>
        Future.successful(Redirect(routes.AdminApplication.index))
    }

  private def getAllContests(implicit ec: ExecutionContext) =
    Ask[StatusActor.AllContests](statusActorModel.statusActor, StatusActor.GetAllContests).map(_.contests)

  private def getSelectedContests(contestId: Int, account: Admin)(implicit ec: ExecutionContext): Future[SelectedContest] =
    getAllContests.map(_.filter(c => account.canSpectate(c.id)).sortBy(_.id)).map { contests =>
      val cmap = contests.map(x => x.id -> x).toMap
      SelectedContest(cmap.getOrElse(contestId, contests.head), cmap.mapValues(_.name).toSeq)
    }

  private def getAllWaiterTasks(perm: WaiterPermissions)(implicit ec: ExecutionContext) =
    Ask[WaiterActor.Snapshot](statusActorModel.waiterActor, WaiterActor.GetSnapshot(perm)).map(_.tasks)


  import scala.concurrent.ExecutionContext.Implicits.global

  def monitor(id: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(id)).async { implicit request =>
    getSelectedContests(id, request.identity).zip(monitorModel.getMonitor(id, request.identity.canSeeAll(id))).map {
      case (contest, status) =>
        Ok(html.admin.monitor(contest, status.get.status))
    }
  }


  case class canSeeSubmit(submitId: Int) extends Authorization[Admin, SessionAuthenticator] {
    override def isAuthorized[B](identity: Admin, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] =
      getSubmitCid(submitId).map { cids =>
        cids.exists(identity.canSpectate)
      }
  }

  case class canRejudgeSubmit(submitId: Int) extends Authorization[Admin, SessionAuthenticator] {
    override def isAuthorized[B](identity: Admin, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] =
      getSubmitCid(submitId).map { cids =>
        cids.exists(identity.canModify)
      }
  }

  def index = silhouette.SecuredAction.async { implicit request =>
    Future.successful(Redirect(routes.AdminApplication.submits(request.identity.defaultContest)))
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


  def submits(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestId)).async { implicit request =>
    val lim = if (request.identity.canModify(contestId))
      None
    else
      Some(100)
    showSubs(contestId, lim, request.identity)
  }

  val rejudgeSubmitRangeForm = Form {
    mapping("range" -> text)(RejudgeSubmitRange.apply)(RejudgeSubmitRange.unapply)
  }

  def rejudgePage(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestId)).async { implicit request =>
    getSelectedContests(contestId, request.identity).map { contest =>
      Ok(html.admin.rejudge(rejudgeSubmitRangeForm, contest))
    }
  }

  def rejudgeRange(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestId))(parse.multipartFormData).async { implicit request =>
    rejudgeSubmitRangeForm.bindFromRequest.fold(
      formWithErrors => getSelectedContests(contestId, request.identity).map { contest =>
        BadRequest(html.admin.rejudge(formWithErrors, contest))
      },
      data => rejudgeRangeEx(data.range, request.identity).map { rejudged =>
        Redirect(routes.AdminApplication.rejudgePage(contestId)).flashing(
          "success" -> rejudged.mkString(" ")
        )
      }
    )
  }

  def rejudgeSubmit(submitId: Int) = silhouette.SecuredAction(canRejudgeSubmit(submitId)).async { implicit request =>
    rabbitMq ! Message.queue(SubmitMessage(submitId), queue = "contester.submitrequests")
    getSubmitCid(submitId).map { cids =>
      cids match {
        case Some(contestId) => Redirect(routes.AdminApplication.submits(contestId))
        case None => Redirect(routes.AdminApplication.index)
      }
    }
  }

  private def getTestingByID(testingID: Int) =
    db.run(SlickModel.testings.filter(_.id === testingID).result.headOption)

  private def getSubmitAndTesting(submitId: Int) =
    Submits.getSubmitById(db, submitId).flatMap {
      case Some(submit) =>
        submit.fsub.submit.testingId.map(x => getTestingByID(x))
          .getOrElse(Future.successful(None)).map { testing =>
          Some((submit, testing))
        }
      case None => Future.successful(None)
    }

  def showSubmit(contestId: Int, submitId: Int) = silhouette.SecuredAction(canSeeSubmit(submitId)).async { implicit request =>
    getSubmitAndTesting(submitId).flatMap {
      case Some((submit, testingOpt)) =>
          testingOpt.flatMap { testing =>
            testing.problemId.map { problemId =>
              val phandle = PolygonURL(problemId)
              Outputs.getAllAssets2(ws, fileserverBase, shortn, submit.fsub.submit.submitId.id, testing.id, submit.fsub.details.map(_.test), phandle)
            }
          }.getOrElse(Future.successful(Map[Int, ResultAssets]())).zip(
          getSelectedContests(contestId, request.identity)).map {
          case (outputs, contest) =>
            Ok(html.admin.showsubmit(submit, contest, outputs))
        }
      case None =>
        Future.successful(NotFound)
    }
  }

  def downloadArchiveSubmit(contestId: Int, submitId: Int) = silhouette.SecuredAction(canSeeSubmit(submitId)).async { implicit request =>
    getSubmitAndTesting(submitId).map {
      case Some((submit, testingOpt)) =>
        testingOpt.flatMap { testing =>
          testing.problemId.map { problemId =>
            val phandle = PolygonURL(problemId)
            val pprefix = phandle.prefix.stripPrefix("problem/")
            val downloadLoc = s"/fs2/?contest=$shortn&submit=${submit.fsub.submit.submitId.id}&testing=${testing.id}&problem=${pprefix}"
            logger.trace(s"download: $downloadLoc")
            Ok("download").as("application/zip").withHeaders("X-Accel-Redirect" -> downloadLoc)
          }
        }.getOrElse(NotFound)
      case None => NotFound
    }
  }

  def reprintSubmit(submitId: Int) = silhouette.SecuredAction(canRejudgeSubmit(submitId)).async { implicit request =>
    rabbitMq ! Message.queue(SubmitTicketLite(SubmitIdLite(submitId)), queue = "contester.tickets")
    Future.successful(Ok("ok"))
  }

  def showQandA(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestId)).async { implicit request =>
    ClarificationModel.getClarifications(db, contestId)
      .zip(ClarificationModel.getClarificationReqs(db, contestId))
      .zip(getSelectedContests(contestId, request.identity))
        .zip(getAllWaiterTasks(request.identity))
      .map {
      case (((clarifications, clReqs), contest), tasks) =>
        Ok(html.admin.qanda(tasks, clarifications, clReqs, contest))
    }
  }

  def tasks(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestId)).async { implicit request =>
    getSelectedContests(contestId, request.identity)
      .zip(getAllWaiterTasks(request.identity))
      .map {
        case (contest, tasks) =>
          Ok(html.admin.waitertasksmain(tasks, contest, request.identity))
      }
  }

  // TODO: check permissions, do just toggle instead of full update
  def toggleClarification(clrId: Int) = silhouette.SecuredAction.async { implicit request =>
    db.run(SlickModel.clarifications.filter(_.id === clrId).result).flatMap { found =>
      found.headOption match {
        case Some(clr) =>
          Ask.apply[Clarification](statusActorModel.statusActor, clr.copy(hidden = !clr.hidden))
            .map { _ =>
              Ok("ok")
            }
        case None => Future.successful(NotFound)
      }
    }
  }

  def deleteClarification(contestId: Int, clrId: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestId)).async { implicit request =>
    Ask.apply[Option[Clarification]](statusActorModel.statusActor, StatusActor.DeleteClarification(clrId)).map { _ =>
      Ok("ok")
    }
  }

  private implicit val standardTimeout: akka.util.Timeout = {
    import scala.concurrent.duration._
    Duration(5, SECONDS)
  }

  private def joinAdminFeed(contestId: Int, perm: WaiterPermissions, requestHeader: RequestHeader) = {
    Ask[Source[Event, NotUsed]](statusActorModel.statusActor, StatusActor.JoinAdmin(contestId)).zip(
      Ask[Source[Event, NotUsed]](statusActorModel.waiterActor, WaiterActor.Join(perm, requestHeader))).map {
      case (one, two) =>
        Source.combine(one, two)(Merge(_))
    }
  }

  def feed(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestId)).async { implicit request =>
    joinAdminFeed(contestId, request.identity, request).map { e =>
      Ok.chunked(e).as(ContentTypes.EVENT_STREAM)
    }
  }

  private val postClarificationForm = Form {
    mapping("problem" -> text,
      "text" -> nonEmptyText,
      "isHidden" -> boolean
    )(PostClarification.apply)(PostClarification.unapply)
  }

  private def selectableProblems(problems: Seq[Problem]) = Seq[(String, String)](("", "Выберите задачу")) ++ Problems.toSelect(problems)

  def postNewClarification(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestId)).async { implicit request =>
    getSelectedContests(contestId, request.identity).flatMap { contest =>
      monitorModel.problemClient.getProblems(contest.contest.id).map { problems =>
        Ok(html.admin.postclarification(None, postClarificationForm, selectableProblems(problems), contest))
      }
    }
  }

  // TODO: check permissions
  def postUpdateClarification(clarificationId: Int) = silhouette.SecuredAction.async { implicit request =>
    ClarificationModel.getClarification(db, clarificationId).flatMap { clOpt =>
      clOpt.map { cl =>
        val clObj = PostClarification(
          cl.problem, cl.text, cl.hidden
        )
        getSelectedContests(cl.contest, request.identity).flatMap { contest =>
          monitorModel.problemClient.getProblems(contest.contest.id).map { problems =>
            Ok(html.admin.postclarification(cl.id, postClarificationForm.fill(clObj), selectableProblems(problems), contest))
          }
        }
      }.getOrElse(Future.successful(Redirect(routes.AdminApplication.postNewClarification(1))))
    }
  }

  // TODO: check permissions
  def postClarification(contestId: Int, clarificationId: Option[Int]) = silhouette.SecuredAction.async { implicit request =>
    import utils.Db._
    postClarificationForm.bindFromRequest.fold(
      formWithErrors => getSelectedContests(1, request.identity).flatMap { contest =>
        monitorModel.problemClient.getProblems(contest.contest.id).map { problems =>
          BadRequest(html.admin.postclarification(clarificationId, formWithErrors, selectableProblems(problems), contest))
        }
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

  // TODO: check permissions
  def postAnswerForm(clrId: Int) = silhouette.SecuredAction.async { implicit request =>
    ClarificationModel.getClarificationReq(db, clrId).flatMap { optClr =>
      optClr.map { clr =>
        getSelectedContests(clr.contest, request.identity).map { contest =>
          Ok(html.admin.postanswer(
            clarificationResponseForm.fill(ClarificationResponse(clr.getAnswer)), clr, answerList.toSeq, contest))
        }
      }.getOrElse(Future.successful(Redirect(routes.AdminApplication.showQandA(1))))
    }
  }

  // TODO: check permissions
  def postAnswer(clrId: Int) = silhouette.SecuredAction(parse.multipartFormData).async { implicit request =>
    ClarificationModel.getClarificationReq(db, clrId).flatMap { optClr =>
      optClr.map { clr =>
        clarificationResponseForm.bindFromRequest.fold(
          formWithErrors => getSelectedContests(clr.contest, request.identity).map { contest =>
            BadRequest(html.admin.postanswer(formWithErrors, clr, answerList.toSeq, contest))
          },
          data => {
            db.run(SlickModel.clarificationRequests.filter(_.id === clrId).map(x => (x.answer, x.answered)).update((Some(data.answer),true))).map { _ =>
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

  def postNewWaiterTaskForm(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withCreateTasks).async { implicit request =>
    getSelectedContests(contestId, request.identity).map { contest =>
      Ok(html.admin.postwaitertask(None, waiterTaskForm, contest))
    }
  }

  def postWaiterTask(contestId: Int, id: Option[Int]) = silhouette.SecuredAction(AdminPermissions.withCreateTasks).async { implicit request =>
    waiterTaskForm.bindFromRequest.fold(
      formWithErrors => getSelectedContests(contestId, request.identity).map { contest =>
        BadRequest(html.admin.postwaitertask(id, formWithErrors, contest))
      },
      data => {
        Ask.apply[StoredWaiterTask](statusActorModel.waiterActor, WaiterActor.NewTask(data.message, Nil)).map { posted =>
          Redirect(routes.AdminApplication.tasks(contestId))
        }
      }
    )
  }

  def ackWaiterTask(id: Long, room: String) = silhouette.SecuredAction.async { implicit request =>
    statusActorModel.waiterActor ! WaiterActor.AckTask(id, room)
    Future.successful(Ok("ok"))
  }

  def unackWaiterTask(id: Long, room: String) = silhouette.SecuredAction.async { implicit request =>
    statusActorModel.waiterActor ! WaiterActor.UnackTask(id, room)
    Future.successful(Ok("ok"))
  }

  def deleteWaiterTask(id: Long) = silhouette.SecuredAction.async { implicit request =>
    statusActorModel.waiterActor ! WaiterActor.DeleteTask(id)
    Future.successful(Ok("ok"))
  }

  private def getPrintJobs(contestID: Int) = {
    import com.github.tototoshi.slick.MySQLJodaSupport._

    db.run(
      (for {
        (j, l) <- SlickModel.printJobs.filter(_.contest === contestID) join SlickModel.compLocations on (_.computer === _.id)
      } yield (j, l)).sortBy(_._1.arrived).result)
    .zip(monitorModel.teamClient.getTeams(contestID)).map {
      case (jobs, teams) =>
        jobs.flatMap { src =>
          teams.get(src._1.team).map { team =>
            AdminPrintJob(src._1.id.get.toInt, src._1.contest, team, src._1.filename, src._1.arrived, src._1.printed.getOrElse(0) == 255, src._2.name)
          }
        }
    }
  }

  def printJobs(contestID: Int)= silhouette.SecuredAction(AdminPermissions.withSpectate(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity)
      .zip(getPrintJobs(contestID))
      .map {
        case (contest, jobs) =>
          Ok(html.admin.printjobs(jobs, contest, request.identity))
      }

  }

  def reprintJob(printJobID: Int) = silhouette.SecuredAction.async { implicit request =>
    rabbitMq ! Message.queue(printing.PrintJobID(printJobID), queue = "contester.printrequests")
    Future.successful(Ok("ok"))
  }

  def listTeams(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity).flatMap { contest =>
      db.run((for {
        ((p, t), s) <- SlickModel.participants.filter(_.contest === contest.contest.id) join SlickModel.teams on (_.team === _.id) join SlickModel.schools on (_._2.school === _.id)
      } yield (p, t, s)).result).map { results =>
        val cvt = results.map {
          case (p, t, s) =>
            DisplayTeam(t, s)
        }
        Ok(html.admin.teamlist(cvt, contest, request.identity))
      }
    }
  }

  private val editTeamForm = Form {
    mapping("schoolName" -> text,
    "teamName" -> text,
    "teamNum" -> optional(number)
    )(EditTeam.apply)(EditTeam.unapply)
  }

  private def displayEditTeam(form: Option[Form[EditTeam]], team: SlickModel.Team, school: School, contest: SelectedContest)(implicit request: RequestHeader, ec: ExecutionContext) = {
    Ok(html.admin.editteam(form.getOrElse(editTeamForm.fill(EditTeam(school.name, team.name, team.num))), TeamDescription(team.id, school), contest))
  }

  def editTeam(contestID: Int, teamID: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity)
      .zip(db.run((for {
      (t, s) <- SlickModel.teams.filter(_.id === teamID) join SlickModel.schools on (_.school === _.id)
    } yield (t, s)).result.headOption)).map {
      case (contest, vOpt) =>
      vOpt match {
        case Some((team, school)) =>
          displayEditTeam(None, team, school, contest)
        case None =>
          NotFound
      }
    }
  }

  def postEditTeam(contestID: Int, teamID: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity)
      .zip(db.run((for {
        (t, s) <- SlickModel.teams.filter(_.id === teamID) join SlickModel.schools on (_.school === _.id)
      } yield (t, s)).result.headOption)).flatMap {
      case (contest, vOpt) =>
        vOpt match {
          case Some((team, school)) =>
            editTeamForm.bindFromRequest.fold(
              formWithErrors => {
                Future.successful(displayEditTeam(Some(formWithErrors), team, school, contest))
              },
              data => {
                val updateSchool = if (data.schoolName != school.name) {
                  Some(SlickModel.schools.filter(_.id === school.id).map(_.name).update(data.schoolName))
                } else None
                val updateTeam = if (data.teamName != team.name) {
                  Some(SlickModel.teams.filter(_.id === team.id).map(_.name).update(data.teamName))
                } else None

                val vec = updateSchool.toVector ++ updateTeam.toVector
                val vseq = DBIO.sequence(vec)

                db.run(vseq).map { v =>
                  Redirect(routes.AdminApplication.editTeam(contestID, teamID))
                }
              },
            )
          case None =>
            Future.successful(NotFound)
        }
    }
  }
}