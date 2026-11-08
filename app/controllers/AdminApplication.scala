package controllers

import actors.StatusActor.ClarificationAnswered
import actors.{StatusActor, WaiterActor}
import akka.NotUsed
import akka.stream.scaladsl.{Merge, Source}
import com.github.nscala_time.time.Imports._
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.{Authorization, Silhouette}
import com.mohiva.play.silhouette.impl.authenticators.SessionAuthenticator
import com.spingo.op_rabbit.Message
import models._
import org.apache.commons.io.FileUtils
import org.stingray.contester.dbmodel.{Clarification, Problem, School}
import org.stingray.contester.dbmodel.SlickModel.{Team, teams}
import play.api.data.Form
import play.api.data.Forms._
import play.api.http.ContentTypes
import play.api.i18n.I18nSupport
import play.api.libs.EventSource.Event
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc._
import play.api.{Configuration, Logging}
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import utils.auth.AdminEnv
import utils.{Ask, PolygonURL}
import views.html

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/** all: rejudge every submit of the problem, not just the outdated ones. */
case class RejudgeProblem(problem: String, all: Boolean)
case class PostClarification(problem: String, text: String, hidden: Boolean)
case class ClarificationResponse(answer: String)

case class PostWaiterTask(message: String, rooms: String)

case class ContestDescription(id: Int)

case class EditTeam(schoolID: Int, teamName: String, teamNum: Option[Int], notRated: Boolean = false, disqualified: Boolean = false,
                    disabled: Boolean = false, noPrint: Boolean = false)

case class PrintTarget(addr: String, name: String, area: String, printer: String)
case class ServiceTeamsStatus(kind: InitialImportTools.ServiceTeamKind, schoolId: Option[Int], teams: Int)

case class EditSchool(shortName: String, fullName: String)

case class DisplayTeam(team: Team, school: School, login: String, password: String)

case class DisplaySchool(id: Int, name: String)

case class AdminPrintAttempt(id: Long, kind: String, created: DateTime, requestedBy: Option[String], printer: String,
                             status: String, pagesPrinted: Int, renderPages: Option[Int], error: String, detail: String)

case class AdminPrintJob(id: Long, contestID: Int, team: org.stingray.contester.dbmodel.Team, filename: String, arrived: DateTime,
                         printed: Option[DateTime], computerName: String, pages: Int, error: String,
                         attempts: Seq[AdminPrintAttempt])

case class SubmitIdLite(id: Int)
object SubmitIdLite {
  implicit val format = Json.format[SubmitIdLite]
}

case class SubmitTicketLite(submit: SubmitIdLite)
object SubmitTicketLite {
  implicit val format = Json.format[SubmitTicketLite]
}

case class EditContest(name: String, schoolMode: Boolean, startTime: DateTime, freezeTime: DateTime, endTime: DateTime,
                       exposeTime: DateTime, polygonID: Option[String], language: String)

case class EditAdmin(username: String, password: String, spectator: String, administrator: String,
                     locations: String, unrestricted: String)

class AdminApplication (cc: ControllerComponents, silhouette: Silhouette[AdminEnv], dbConfig: DatabaseConfig[JdbcProfile],
                             monitorModel: Monitor,
                                 rabbitMqModel: RabbitMqModel,
                             statusActorModel: StatusActorModel,
                        printingModel: PrintingModel,
                                  configuration: Configuration,
                        externalDatabases: ExternalDatabases,
                                  ws: WSClient,
                        liveHooks: live.LiveHooks,
                        liveConfig: live.LiveMonitorConfig,
                        liveMonitor: live.LiveMonitorRef) extends AbstractController(cc)  with I18nSupport with Logging {

  implicit val ec = defaultExecutionContext

  private val db = dbConfig.db
  import com.spingo.op_rabbit.PlayJsonSupport._

  private val rabbitMq = rabbitMqModel.rabbitMq

  private val fileserverUrl = configuration.get[String]("fileserver.url")
  private val shortn = configuration.get[String]("fileserver.shortn")

  private val fileserverBase = fileserverUrl.stripSuffix("fs/")

  logger.info(s"fileserverBase: $fileserverBase")

  import org.stingray.contester.dbmodel.SlickModel
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._

  private[this] def getSubmitCid(submitId: Long)(implicit ec: ExecutionContext): Future[Option[Int]] =
    db.run(SlickModel.submits.filter(_.id === submitId).take(1).map(_.contest).result.headOption)

  private def getAllContests(implicit ec: ExecutionContext) =
    Ask[StatusActor.AllContests](statusActorModel.statusActor, StatusActor.GetAllContests).map(_.contests)

  private def getSelectedContests(contestId: Int, account: Admin)(implicit ec: ExecutionContext): Future[SelectedContest] =
    getAllContests.map(_.filter(c => account.canSpectate(c.id)).sortBy(_.id)).map { contests =>
      val cmap = contests.map(x => x.id -> x).toMap
      val selected = cmap.getOrElse(contestId, contests.head)
      SelectedContest(selected, cmap.mapValues(_.name).toSeq, account.canModify(selected.id))
    }

  private def getAllWaiterTasks(perm: WaiterPermissions)(implicit ec: ExecutionContext) =
    Ask[WaiterActor.Snapshot](statusActorModel.waiterActor, WaiterActor.GetSnapshot(perm)).map(_.tasks)

  def monitor(id: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(id)).async { implicit request =>
    if (liveConfig.enabled)
      getSelectedContests(id, request.identity).flatMap { contest =>
        LiveStreams.snapshot(liveMonitor.ref, LiveStreams.adminJoin(id, request.identity.canSeeAll(id))).map { initial =>
          Ok(html.admin.livemonitor(contest, initial)).withHeaders(LiveStreams.pageHeaders: _*)
        }
      }
    else getSelectedContests(id, request.identity).zip(monitorModel.getMonitor(id, request.identity.canSeeAll(id))).map {
      case (contest, Some(status)) =>
        Ok(html.admin.monitor(contest, status.status))
      case (_, None) => NotFound
    }
  }

  // The public page and stream of any contest, for the people who can open its admin monitor: what the public
  // would see, whether or not the contest is on the public list. Nothing here is reachable without a session.
  def publicPreview(id: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(id)).async { implicit request =>
    if (!liveConfig.enabled) Future.successful(NotFound)
    else getSelectedContests(id, request.identity).flatMap { sel =>
      if (sel.contest.id != id) Future.successful(NotFound)
      else LiveStreams.snapshot(liveMonitor.ref, LiveStreams.publicPreviewJoin(id)).map { initial =>
        Ok(html.publiclive(sel.contest, routes.AdminApplication.publicPreviewEvents(id).url, initial))
          .withHeaders(LiveStreams.pageHeaders: _*)
      }
    }
  }

  def publicPreviewEvents(id: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(id)).async { implicit request =>
    LiveStreams.stream(liveMonitor.ref.filter(_ => liveConfig.enabled), LiveStreams.publicPreviewJoin(id))
  }

  // The real board for accounts that can see everything, otherwise the freezing board.
  def monitorEvents(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestId)).async { implicit request =>
    LiveStreams.stream(liveMonitor.ref.filter(_ => liveConfig.enabled),
      LiveStreams.adminJoin(contestId, request.identity.canSeeAll(contestId)))
  }


  /** The submit's contest and whether it arrived after that contest's freeze time (the same rule as the status list). */
  private[this] def getSubmitContestFreeze(submitId: Long)(implicit ec: ExecutionContext): Future[Option[(Int, Boolean)]] =
    db.run((for {
      s <- SlickModel.submits if s.id === submitId
      c <- SlickModel.contests if c.id === s.contest
    } yield (s.contest, s.arrived > c.freezeTime)).take(1).result.headOption)

  /** Spectators may open a submit of their contest; those with a restricted view only if it arrived before the freeze. */
  case class canSeeSubmit(submitId: Int) extends Authorization[Admin, SessionAuthenticator] {
    override def isAuthorized[B](identity: Admin, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] =
      getSubmitContestFreeze(submitId).map { found =>
        found.exists { case (cid, afterFreeze) => identity.canSeeSubmit(cid, afterFreeze) }
      }
  }

  case class canRejudgeSubmit(submitId: Int) extends Authorization[Admin, SessionAuthenticator] {
    override def isAuthorized[B](identity: Admin, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] =
      getSubmitCid(submitId).map { cids =>
        cids.exists(identity.canModify)
      }
  }

  def index = silhouette.SecuredAction.async { implicit request =>
    Future.successful(Redirect(routes.AdminApplication.submits(request.identity.defaultContest, None, None, None)))
  }

  // The status page's rows as they arrive and finish; 404 when the live monitor is off (the page is then static).
  def statusEvents(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestId)).async { implicit request =>
    LiveStreams.stream(liveMonitor.ref.filter(_ => liveConfig.enabled),
      LiveStreams.adminStatusJoin(contestId, request.identity.canSeeAll(contestId)))
  }

  def submits(contestId: Int, team: Option[Int], before: Option[Long], after: Option[Long]) =
    silhouette.SecuredAction(AdminPermissions.withSpectate(contestId)).async { implicit request =>
      val account = request.identity
      getSelectedContests(contestId, account).zip(statusActorModel.teamClient.getTeams(contestId)).flatMap {
        case (contest, teamMap) =>
          AdminSubmits.page(db, contestId, team, before, after, account.canSeeAll(contestId)).map { page =>
            Ok(html.admin.submits(page, teamMap, contest, account, team,
              live = liveMonitor.ref.isDefined && liveConfig.enabled, newest = before.isEmpty && page.newer.isEmpty))
          }
      }
    }

  def customTests(contestId: Int, team: Option[Int], before: Option[Long], after: Option[Long]) =
    silhouette.SecuredAction(AdminPermissions.withSpectate(contestId)).async { implicit request =>
      getSelectedContests(contestId, request.identity).zip(statusActorModel.teamClient.getTeams(contestId)).flatMap {
        case (contest, teamMap) =>
          AdminCustomTests.page(db, contestId, team, before, after).map { page =>
            Ok(html.admin.customtests(page, teamMap, contest, team))
          }
      }
    }

  def showCustomTest(contestId: Int, id: Long) =
    silhouette.SecuredAction(AdminPermissions.withSpectate(contestId)).async { implicit request =>
      AdminCustomTests.get(db, contestId, id).flatMap {
        case Some(ev) =>
          getSelectedContests(contestId, request.identity).zip(statusActorModel.teamClient.getTeams(contestId)).map {
            case (contest, teamMap) => Ok(html.admin.showcustomtest(ev, teamMap.get(ev.team), contest, request.identity.canModify(contestId)))
          }
        case None => Future.successful(NotFound)
      }
    }

  val rejudgeProblemForm = Form {
    mapping("problem" -> nonEmptyText, "all" -> default(boolean, false))(RejudgeProblem.apply)(RejudgeProblem.unapply)
  }

  def rejudgePage(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestId)).async { implicit request =>
    getSelectedContests(contestId, request.identity).zip(AdminSubmits.revisionStatus(db, contestId)).map { case (contest, status) =>
      Ok(html.admin.rejudge(contest, status))
    }
  }

  /**
   * Rejudges the finished submits of one problem whose last testing is not against the problem's current
   * URL, or with `all` every submit of the problem. The URL is recomputed here, not taken from the page, so a stale page cannot rejudge to an old revision.
   */
  def rejudgeProblem(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestId)).async { implicit request =>
    val back = Redirect(routes.AdminApplication.rejudgePage(contestId))
    rejudgeProblemForm.bindFromRequest.fold(
      _ => Future.successful(back.flashing("error" -> "No problem given")),
      data => AdminSubmits.revisionStatus(db, contestId).flatMap { status =>
        status.find(_.problem == data.problem).flatMap(_.latestURL) match {
          case None =>
            Future.successful(back.flashing("error" -> s"Problem ${data.problem}: no revision loaded"))
          case Some(url) =>
            val ids = if (data.all) AdminSubmits.allSubmitIds(db, contestId, data.problem)
              else AdminSubmits.rejudgePlan(db, contestId, data.problem, url).map(_.outdated.map(_._1))
            ids.map { ids =>
              ids.foreach { id =>
                rabbitMq ! Message.queue(SubmitMessage(id), queue = "contester.submitrequests")
                Try(liveHooks.rejudged(id.toInt))
              }
              back.flashing("success" -> s"${ids.size} ${if (data.all) "(all) " else ""}submits of problem ${data.problem}")
            }
        }
      }
    )
  }

  // Called from the status page via ajax, which has nothing to do with a response body.
  def rejudgeSubmit(submitId: Int) = silhouette.SecuredAction(canRejudgeSubmit(submitId)) { implicit request =>
    rabbitMq ! Message.queue(SubmitMessage(submitId), queue = "contester.submitrequests")
    Try(liveHooks.rejudged(submitId))
    NoContent
  }

  private def getTestingByID(testingID: Int) =
    db.run(SlickModel.testingByID(testingID.toLong).result.headOption)

  private def getSubmitAndTesting(submitId: Int) =
    Submits.getSubmitById(db, submitId).flatMap {
      case Some(submit) =>
        submit.fsub.submit.testingId.map(x => getTestingByID(x))
          .getOrElse(Future.successful(None)).map { testing =>
          Some((submit, testing))
        }
      case None => Future.successful(None)
    }

  /** The submit with the results of its current testing, or of the given earlier one (a run before a rejudge). */
  private def getSubmitAtTesting(submitId: Int, testing: Option[Long]): Future[Option[(SubmitDetails, Option[Long])]] =
    getSubmitAndTesting(submitId).flatMap {
      case Some((submit, current)) =>
        testing.filterNot(t => current.exists(_._1 == t)) match {
          case None => Future.successful(Some((submit, current.map(_._1))))
          case Some(other) =>
            getTestingByID(other.toInt).flatMap {
              case Some(t) if t._2 == submitId =>
                Submits.loadSubmitDetails(db, other.toInt).map { details =>
                  Some((submit.copy(fsub = submit.fsub.copy(details = details)), Some(other)))
                }
              case _ => Future.successful(None)
            }
        }
      case None => Future.successful(None)
    }

  def showSubmit(contestId: Int, submitId: Int, testing: Option[Long]) = silhouette.SecuredAction(canSeeSubmit(submitId)).async { implicit request =>
    getSubmitAtTesting(submitId, testing).flatMap {
      case Some((submit, viewed)) =>
        val outputsF = viewed.map { tid =>
          getTestingByID(tid.toInt).flatMap {
            case Some(t) =>
              Outputs.getAllAssets2(ws, fileserverBase, shortn, submit.fsub.submit.submitId.id, tid.toInt,
                submit.fsub.details.map(_.test), PolygonURL(t._4))
            case None => Future.successful(Map[Int, ResultAssets]())
          }
        }.getOrElse(Future.successful(Map[Int, ResultAssets]()))
        outputsF.zip(getSelectedContests(contestId, request.identity))
          .zip(AdminSubmits.runHistory(db, submitId, submit.fsub.submit.testingId.map(_.toLong))).map {
          case ((outputs, contest), runs) =>
            Ok(html.admin.showsubmit(submit, contest, outputs, runs, viewed))
        }
      case None =>
        Future.successful(NotFound)
    }
  }

  def downloadArchiveSubmit(contestId: Int, submitId: Int) = silhouette.SecuredAction(canSeeSubmit(submitId)).async { implicit request =>
    val q = for {
      s <- SlickModel.submits if s.id === submitId.toLong
      t <- SlickModel.testings if s.testingID === t.id
    } yield (t.id, t.problemURL)

    db.run(q.take(1).result.headOption).map {
        case Some((testingID, problemURL)) =>
          val phandle = PolygonURL(problemURL)
          val pprefix = phandle.prefix.stripPrefix("problem/")
          val downloadLoc = s"/fs2/?contest=$shortn&submit=${submitId}&testing=${testingID}&problem=${pprefix}"
          logger.trace(s"download: $downloadLoc")
          Ok("download").as("application/zip").withHeaders("X-Accel-Redirect" -> downloadLoc)
        case None => NotFound
    }
  }

  def showQandA(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestId)).async { implicit request =>
    db.run(SlickModel.getClarificationsForContest(contestId).result)
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

  // TODO: do just toggle instead of full update
  def toggleClarification(clrId: Long) = silhouette.SecuredAction.async { implicit request =>
    db.run(SlickModel.clarificationByID(clrId).result).flatMap { found =>
      found.headOption match {
        case Some(clr) if !request.identity.canModify(clr.contest) => Future.successful(Forbidden)
        case Some(clr) =>
          Ask.apply[Clarification](statusActorModel.statusActor, clr.copy(hidden = !clr.hidden))
            .map { _ =>
              Ok("ok")
            }
        case None => Future.successful(NotFound)
      }
    }
  }

  def deleteClarification(contestId: Int, clrId: Long) = silhouette.SecuredAction(AdminPermissions.withModify(contestId)).async { implicit request =>
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

  private[this] def selectableProblems(problems: Seq[Problem]) = Seq[(String, String)](("", "Выберите задачу")) ++ Problems.toSelect(problems)

  def postNewClarification(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestId)).async { implicit request =>
    getSelectedContests(contestId, request.identity).flatMap { contest =>
      statusActorModel.problemClient.getProblems(contest.contest.id).map { problems =>
        Ok(html.admin.postclarification(None, postClarificationForm, selectableProblems(problems), contest))
      }
    }
  }

  def postUpdateClarification(clarificationId: Long) = silhouette.SecuredAction.async { implicit request =>
    db.run(SlickModel.clarificationByID(clarificationId).result.headOption).flatMap { clOpt =>
      clOpt.map { cl =>
        if (!request.identity.canModify(cl.contest)) Future.successful(Forbidden) else {
          val clObj = PostClarification(
            cl.problem, cl.text, cl.hidden
          )
          getSelectedContests(cl.contest, request.identity).flatMap { contest =>
            statusActorModel.problemClient.getProblems(contest.contest.id).map { problems =>
              Ok(html.admin.postclarification(cl.id, postClarificationForm.fill(clObj), selectableProblems(problems), contest))
            }
          }
        }
      }.getOrElse(Future.successful(Redirect(routes.AdminApplication.index)))
    }
  }

  def postClarification(contestId: Int, clarificationId: Option[Long]) = silhouette.SecuredAction(AdminPermissions.withModify(contestId)).async { implicit request =>
    // an existing clarification may only be edited within its own contest
    val ownedByContest = clarificationId match {
      case Some(id) => db.run(SlickModel.clarificationByID(id).result.headOption).map(_.exists(_.contest == contestId))
      case None => Future.successful(true)
    }
    ownedByContest.flatMap {
      case false => Future.successful(Forbidden)
      case true => postClarificationForm.bindFromRequest.fold(
      formWithErrors => getSelectedContests(contestId, request.identity).flatMap { contest =>
        statusActorModel.problemClient.getProblems(contest.contest.id).map { problems =>
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

  private[this] def getClarificationReq(id: Long)(implicit ec: ExecutionContext) =
    db.run(ClarificationModel.getClarificationReq(id).result.headOption)

  def postAnswerForm(clrId: Long) = silhouette.SecuredAction.async { implicit request =>
    getClarificationReq(clrId).flatMap { optClr =>
      optClr.map { clr =>
        if (!request.identity.canModify(clr.contest)) Future.successful(Forbidden) else
        getSelectedContests(clr.contest, request.identity).map { contest =>
          Ok(html.admin.postanswer(
            clarificationResponseForm.fill(ClarificationResponse(clr.getAnswer)), clr, answerList.toSeq, contest))
        }
      }.getOrElse(Future.successful(Redirect(routes.AdminApplication.index)))
    }
  }

  def postAnswer(clrId: Long) = silhouette.SecuredAction(parse.multipartFormData).async { implicit request =>
    getClarificationReq(clrId).flatMap { optClr =>
      optClr.map { clr =>
        if (!request.identity.canModify(clr.contest)) Future.successful(Forbidden) else
        clarificationResponseForm.bindFromRequest.fold(
          formWithErrors => getSelectedContests(clr.contest, request.identity).map { contest =>
            BadRequest(html.admin.postanswer(formWithErrors, clr, answerList.toSeq, contest))
          },
          data => {
            db.run(ClarificationModel.getClarificationReqShort(clrId).update((data.answer,true))).map { _ =>
              statusActorModel.statusActor ! ClarificationAnswered(clr.contest, clrId, clr.team, clr.problem, data.answer)
              Redirect(routes.AdminApplication.showQandA(clr.contest))
            }
          }
        )
      }.getOrElse(Future.successful(Redirect(routes.AdminApplication.index)))
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
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    import org.stingray.contester.dbmodel.SlickModel._

    val jobsAndAttempts = db.run(locatedPrintJobsByContest(contestID).result).flatMap { jobs =>
      val q = dbPrintAttempts.filter(_.jobId inSet jobs.map(_.id)).sortBy(_.id.desc)
        .joinLeft(dbPrintRenders).on(_.renderId === _.id)
        .map { case (a, r) => (a, r.map(_.pages)) }
      db.run(q.result).map { attempts =>
        (jobs, attempts.groupBy(_._1.jobId).mapValues(_.map { case (a, renderPages) =>
          AdminPrintAttempt(a.id.get, a.kind, a.created, a.requestedBy, a.printer, a.status, a.pagesPrinted, renderPages, a.error, a.detail)
        }))
      }
    }

    jobsAndAttempts.zip(statusActorModel.teamClient.getTeams(contestID)).map {
      case ((jobs, attempts), teams) =>
        jobs.flatMap { src =>
          teams.get(src.team).map { team =>
            AdminPrintJob(src.id, src.contest, team, src.filename, src.arrived, src.printed, src.computerName, src.pages, src.error,
              attempts.getOrElse(src.id, Seq.empty))
          }
        }
    }
  }

  /** All known computers with their area and the area's printer (may be empty), for the test print form. */
  private def printTargets: Future[Seq[PrintTarget]] = {
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    val q = (for {
      c <- SlickModel.compLocations
      a <- SlickModel.areas if a.id === c.location
    } yield (c.id, c.name, a.name, a.printer)).sortBy(x => (x._3, x._2))
    db.run(q.result).map(_.map(x => PrintTarget(x._1.address, x._2, x._3, x._4)))
  }

  /** The first (lowest id) jury team taking part in the contest. */
  private def firstJuryTeam(contestID: Int): Future[Option[Int]] = {
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    val q = (for {
      s <- SlickModel.schools if s.fullName === InitialImportTools.JurySchoolFullName
      t <- SlickModel.teams if t.school === s.id
      p <- SlickModel.participants if (p.team === t.id && p.contest === contestID)
    } yield t.id).sorted.take(1)
    db.run(q.result.headOption)
  }

  def listPrintJobs(contestID: Int)= silhouette.SecuredAction(AdminPermissions.withSpectate(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity)
      .zip(getPrintJobs(contestID))
      .zip(printTargets)
      .map {
        case ((contest, jobs), targets) =>
          Ok(html.admin.printjobs(jobs, targets, contest, request.identity))
      }

  }

  /** Test print: queues an uploaded file as a print job from the chosen computer, attributed to the contest's first jury team. */
  def injectPrintJob(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestID))(parse.multipartFormData).async { implicit request =>
    val back = Redirect(routes.AdminApplication.listPrintJobs(contestID))
    val computer = request.body.dataParts.get("computer").flatMap(_.headOption).getOrElse("")
    request.body.file("file").filter(_.filename.nonEmpty) match {
      case None => Future.successful(back.flashing("error" -> "Pick a file to print"))
      case Some(file) =>
        printTargets.zip(firstJuryTeam(contestID)).flatMap {
          case (targets, teamOpt) =>
            (targets.find(_.addr == computer), teamOpt) match {
              case (None, _) => Future.successful(back.flashing("error" -> "Pick a computer"))
              case (Some(t), _) if t.printer.trim.isEmpty =>
                Future.successful(back.flashing("error" -> s"Area ${t.area} has no printer set"))
              case (_, None) =>
                Future.successful(back.flashing("error" -> "This contest has no jury team; add one on the misc page"))
              case (Some(t), Some(team)) =>
                val bytes = FileUtils.readFileToByteArray(file.ref.path.toFile)
                printingModel.insertAndPrintJob(contestID, team, file.filename, bytes, t.addr).map {
                  case true => back.flashing("success" -> s"Queued ${file.filename} for ${t.name} (${t.area}, ${t.printer})")
                  case false => back.flashing("error" -> "Job saved but could not be queued; see the log")
                }
            }
        }
    }
  }

  /** Prints the job again from its stored render (or renders it, if it never was). */
  def reprintJob(printJobID: Long) = silhouette.SecuredAction.async { implicit request =>
    printingModel.createPrintOnlyAttempt(printJobID, Some(request.identity.username)).map {
      case Right(_) => Ok("ok")
      case Left(message) => InternalServerError(message)
    }
  }

  /** Renders the job from its source again, and prints the result. */
  def rerenderJob(printJobID: Long) = silhouette.SecuredAction.async { implicit request =>
    printingModel.createFullAttempt(printJobID, Some(request.identity.username)).map {
      case Some(_) => Ok("ok")
      case None => InternalServerError("can't build print job (unknown computer location, team or contest?)")
    }
  }

  /** Password slips of the contest as a PDF, without jury and coach teams; or only one school's or one team's. */
  def passwordSheet(contestID: Int, school: Option[Int], team: Option[Int]) =
    silhouette.SecuredAction(AdminPermissions.withSpectate(contestID)).async { implicit request =>
      if (!request.identity.canSeeTeamPasswords(contestID)) {
        Future.successful(Forbidden("not allowed to see team passwords"))
      } else {
        getSelectedContests(contestID, request.identity).flatMap { contest =>
          val q = (for {
            t <- SlickModel.teams
            p <- SlickModel.participants if (t.id === p.team && p.contest === contestID)
            s <- SlickModel.schools if (s.id === t.school)
            a <- SlickModel.assignments if (a.team === p.team && a.contest === p.contest)
          } yield (t, s, a)).result

          db.run(q).map { rows =>
            val all = rows.map { case (t, s, a) =>
              PasswordSheet.Row(t.id, s.id, PasswordCard(contest.contest.name, s.fullName, t.num, t.name, a.username, a.password))
            }
            val selected = PasswordSheet.select(all, school, team)
            Ok(PasswordSheet.render(selected, all.map(_.card)))
              .as("application/pdf")
              .withHeaders("Content-Disposition" -> s"""inline; filename="passwords-$contestID.pdf"""")
          }
        }
      }
    }

  def listTeams(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity).flatMap { contest =>
      val teamQuery = (for {
        t <- SlickModel.teams
        p <- SlickModel.participants if (t.id === p.team && p.contest === contestID)
        s <- SlickModel.schools if (s.id === t.school)
        a <- SlickModel.assignments if (a.team === p.team && a.contest === p.contest)
      } yield (t, p, s, a)).sortBy(_._1.id).result

      val schoolQuery = SlickModel.schools.sortBy(_.fullName).result

      db.run(teamQuery.zip(schoolQuery)).map { case (results, schools) =>
        val cvt = results.map {
          case (t, p, s, a) =>
            DisplayTeam(t, s, a.username, a.password)
        }
        val dsc = schools.map(x => DisplaySchool(x.id, x.fullName))
        Ok(html.admin.teamlist(cvt, dsc, contest, request.identity))
      }
    }
  }

  private val editTeamForm = Form {
    mapping("schoolID" -> number,
    "teamName" -> text,
    "teamNum" -> optional(number),
    "notRated" -> boolean,
    "disqualified" -> boolean,
    "disabled" -> boolean,
    "noPrint" -> boolean
    )(EditTeam.apply)(EditTeam.unapply)
  }

  private def displayEditTeam(form: Option[Form[EditTeam]], team: SlickModel.Team, schools: Seq[DisplaySchool], contest: SelectedContest,
                              participant: Option[SlickModel.Participant])(implicit request: RequestHeader, ec: ExecutionContext) = {
    Ok(html.admin.editteam(form.getOrElse(
      editTeamForm.fill(EditTeam(team.school, team.name, if (team.num!=0) Some(team.num) else None,
        participant.exists(_.notRated), participant.exists(_.disqualified),
        participant.exists(_.disabled), participant.exists(_.noPrint)))), team.id, schools.map(x => x.id.toString -> x.name), contest, participant.isDefined))
  }

  private def forEditTeam(request: SecuredRequest[AdminEnv, AnyContent], contestID: Int, teamID: Int) =
    getSelectedContests(contestID, request.identity)
      .zip(db.run(SlickModel.teams.filter(_.id === teamID).result.headOption zip SlickModel.schools.sortBy(_.fullName).result zip
        SlickModel.participants.filter(x => x.contest === contestID && x.team === teamID).result.headOption)).map {
      case (contest, ((teamOpt, schools), participant)) =>
        teamOpt match {
          case Some(team) => Some((contest, team, schools.map(x => DisplaySchool(x.id, x.fullName)), participant))
          case None => None
    }}

  def editTeam(contestID: Int, teamID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestID)).async { implicit request =>
    forEditTeam(request, contestID, teamID).map {
      case Some((contest, team, schools, participant)) => displayEditTeam(None, team, schools, contest, participant)
      case None => NotFound
    }
  }

  private val editSchoolForm = Form {
    mapping("shortName" -> text,
      "fullName" -> nonEmptyText
    )(EditSchool.apply)(EditSchool.unapply)
  }

  private def displayEditSchool(form: Form[EditSchool], schoolID: Int, contest: SelectedContest)(implicit request: RequestHeader) =
    Ok(html.admin.editschool(form, schoolID, contest))

  def editSchool(contestID: Int, schoolID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity)
      .zip(db.run(SlickModel.schools.filter(_.id === schoolID).result.headOption)).map {
      case (contest, Some(school)) =>
        displayEditSchool(editSchoolForm.fill(EditSchool(school.shortName, school.fullName)), schoolID, contest)
      case _ => NotFound
    }
  }

  def postEditSchool(contestID: Int, schoolID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity)
      .zip(db.run(SlickModel.schools.filter(_.id === schoolID).result.headOption)).flatMap {
      case (contest, Some(_)) =>
        editSchoolForm.bindFromRequest.fold(
          formWithErrors => Future.successful(BadRequest(html.admin.editschool(formWithErrors, schoolID, contest))),
          data => db.run(SlickModel.schools.filter(_.id === schoolID).map(x => (x.shortName, x.fullName))
            .update((data.shortName.trim, data.fullName.trim))).map { _ =>
            Redirect(routes.AdminApplication.editSchool(contestID, schoolID)).flashing("success" -> "School updated.")
          }
        )
      case _ => Future.successful(NotFound)
    }
  }

  def postEditTeam(contestID: Int, teamID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestID)).async { implicit request =>
    forEditTeam(request, contestID, teamID).flatMap {
      case Some((contest, team, schools, participant)) =>
            editTeamForm.bindFromRequest.fold(
              formWithErrors => {
                Future.successful(displayEditTeam(Some(formWithErrors), team, schools, contest, participant))
              },
              data => {
                // teams (school, num) is unique: moving a team into a school that already has that number would violate it
                val taken = if (data.schoolID != team.school) db.run(
                  SlickModel.teams.filter(x => x.school === data.schoolID && x.num === team.num && x.id =!= team.id).exists.result)
                else Future.successful(false)

                taken.flatMap {
                  case true =>
                    val msg = s"That school already has a team number ${team.num}"
                    Future.successful(displayEditTeam(Some(editTeamForm.fill(data).withError("schoolID", msg)), team, schools, contest, participant))
                  case false =>
                    val updateTeam = if (data.teamName != team.name || data.schoolID != team.school) {
                      db.run(SlickModel.teams.filter(_.id === team.id).map(x => (x.name, x.school)).update((data.teamName, data.schoolID)))
                    } else Future.successful(0)

                    // Only this contest's participation changes; a team that does not take part has nothing to switch.
                    val updateRated = if (participant.exists(p => p.notRated != data.notRated || p.disqualified != data.disqualified ||
                      p.disabled != data.disabled || p.noPrint != data.noPrint)) {
                      db.run(SlickModel.participants.filter(x => x.contest === contestID && x.team === teamID)
                        .map(x => (x.notRated, x.disqualified, x.disabled, x.noPrint))
                        .update((data.notRated, data.disqualified, data.disabled, data.noPrint)))
                    } else Future.successful(0)

                    updateTeam.zip(updateRated).flatMap { case (a, b) =>
                      // The new details of the team in every contest it takes part in go to the team list and the live monitor.
                      (if (a + b > 0) db.run(SlickModel.localTeamQuery.filter(_.teamId === teamID).result) else Future.successful(Seq.empty))
                        .map { updated =>
                          updated.foreach { t =>
                            statusActorModel.teamClient.put(t)
                            Try(liveHooks.teamChanged(t.contest, t))
                          }
                          Redirect(routes.AdminApplication.editTeam(contestID, teamID))
                        }
                    }
                }
              },
            )
          case None =>
            Future.successful(NotFound)
        }
  }

  def showContestList(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity).flatMap { contest =>
      db.run(SlickModel.allContests.result).map { results =>
        val cvt = results.filter(x => request.identity.canSpectate(x.id))
        Ok(html.admin.contestlist(cvt, contest, request.identity))
      }
    }}

  import utils.DateParse._

  private val editContestForm = Form {
    mapping(
      "name" -> text,
      "schoolMode" -> boolean,
      "startTime" -> nscalaDateTime,
      "freezeTime" -> nscalaDateTime,
      "endTime" -> nscalaDateTime,
      "exposeTime" -> nscalaDateTime,
      "polygonID" -> optional(text).transform[Option[String]](_.map(_.trim).filter(_.nonEmpty), identity),
      "language" -> text
    )(EditContest.apply)(EditContest.unapply)
  }

  def editContest(contestID: Int, ceditID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(ceditID)).async { implicit request =>
    getSelectedContests(contestID, request.identity).zip(
      db.run(SlickModel.getContestByID(ceditID).result.headOption)
    ).map {
      case (sc, c) =>
        c match {
          case Some(cedit) =>
            val ec = EditContest(cedit.name, cedit.schoolMode, cedit.startTime, cedit.freezeTime, cedit.endTime, cedit.exposeTime, cedit.polygonID, cedit.language)
            val cd = ContestDescription(ceditID)
            Ok(html.admin.editcontest(editContestForm.fill(ec), cd, sc))
          case None =>
            NotFound
        }
    }
  }

  def postEditContest(contestID: Int, ceditID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(ceditID)).async { implicit request =>
    import com.github.tototoshi.slick.PostgresJodaSupport._
    getSelectedContests(contestID, request.identity).flatMap { sc =>
            editContestForm.bindFromRequest.fold(
              formWithErrors => {
                Future.successful(Ok(html.admin.editcontest(formWithErrors, ContestDescription(ceditID), sc)))
              },
              data => {
                val upd = SlickModel.getContestByIDToResync(ceditID)
                    .update((data.name, data.schoolMode, data.startTime, data.freezeTime, data.endTime, data.exposeTime, data.polygonID, data.language))
                db.run(upd).flatMap(_ => announceContestChanged(ceditID)).map { _ =>
                  Redirect(routes.AdminApplication.editContest(contestID, ceditID))
                }
              },
            )
    }
  }

  /** Tells the status actor and the live monitor what the contest row is now; the database stays the truth. */
  private def announceContestChanged(contestID: Int): Future[Unit] =
    db.run(SlickModel.getContestByID(contestID).result.headOption).map(_.foreach(statusActorModel.contestChanged))
      .recover { case e => logger.warn(s"cannot announce the change of contest $contestID: $e") }

  // Exactly one of: `minutes` (the quick buttons) or `start` (the new start time).
  private val shiftContestForm = Form(tuple("minutes" -> optional(number), "start" -> optional(nscalaDateTime)))

  /**
   * Moves all four times of a contest that has not started: by some minutes (negative: earlier), or so that it
   * starts at a given time.
   */
  def postShiftContest(contestID: Int, ceditID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(ceditID)).async { implicit request =>
    import com.github.tototoshi.slick.PostgresJodaSupport._
    val back = Redirect(routes.AdminApplication.editContest(contestID, ceditID))
    shiftContestForm.bindFromRequest.fold(
      _ => Future.successful(back.flashing("error" -> "Give a number of minutes or a start time")),
      {
        case (None, None) | (Some(_), Some(_)) =>
          Future.successful(back.flashing("error" -> "Give a number of minutes or a start time"))
        case (minutes, start) =>
          val action = SlickModel.getContestByID(ceditID).result.headOption.flatMap {
            case None => slick.dbio.DBIO.successful(Left("No such contest"))
            case Some(c) =>
              val now = DateTime.now
              val moved = start match {
                case Some(s) => ContestShift.startAt(c, s, now)
                case None => ContestShift.shift(c, org.joda.time.Duration.standardMinutes(minutes.get), now)
              }
              moved match {
                case Left(error) => slick.dbio.DBIO.successful(Left(error))
                case Right(t) =>
                  SlickModel.contests.filter(_.id === ceditID).map(x => (x.startTime, x.freezeTime, x.endTime, x.exposeTime))
                    .update((t.start, t.freeze, t.end, t.expose))
                    .map(_ => Right((t.start, new org.joda.time.Duration(c.startTime, t.start))))
              }
          }.transactionally
          db.run(action).flatMap {
            case Left(error) => Future.successful(back.flashing("error" -> error))
            case Right((newStart, delta)) => announceContestChanged(ceditID).map { _ =>
              logger.info(s"${request.identity} moved contest $ceditID by ${delta.getStandardSeconds} s, start is now $newStart")
              back.flashing("success" -> s"Contest moved ${ContestShift.describe(delta)}; it starts at ${utils.ShortTime.format(newStart)}")
            }
          }
      }
    )
  }

  def showImportedTeamList(contestID: Int, school: Boolean) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity).flatMap { contest =>
      val sdb = if (school) externalDatabases.schoolWeb.db else externalDatabases.studentWeb.db
      InitialImportTools.getImportedTeams(sdb, school).map { results =>
        Ok(html.admin.importedteams(results.toSeq, contest, request.identity, school))
      }
    }}

  def importTeams(contestID: Int, school: Boolean)= silhouette.SecuredAction(AdminPermissions.withModify(1)).async { implicit request =>
    getSelectedContests(contestID, request.identity).flatMap { contest =>
      val sdb = if (school) externalDatabases.schoolWeb.db else externalDatabases.studentWeb.db
      InitialImportTools.getImportedTeams(sdb, school).flatMap { results =>
        val contests = if(school) Seq(3,4) else Seq(1,2)
        InitialImportTools.populateContestsWithTeams(db, results, contests, false).map { _ =>
          Redirect(routes.AdminApplication.index)
        }
      }
    }
  }

  private case class ServiceTeamsRequest(count: Int, contests: List[Int])

  private val serviceTeamsForm = Form {
    mapping("count" -> number(min = 1, max = 50), "contests" -> list(number))(ServiceTeamsRequest.apply)(ServiceTeamsRequest.unapply)
      .verifying("Pick at least one contest", _.contests.nonEmpty)
  }

  private def addServiceTeams(contestID: Int, kind: InitialImportTools.ServiceTeamKind) =
    silhouette.SecuredAction(AdminPermissions.withModify(1)).async { implicit request =>
      val back = Redirect(routes.AdminApplication.miscForms(contestID))
      serviceTeamsForm.bindFromRequest.fold(
        formWithErrors => Future.successful(back.flashing("error" ->
          s"${kind.fullName}: ${(formWithErrors.errors ++ formWithErrors.globalErrors).map(_.message).mkString("; ")}")),
        data => getSelectedContests(contestID, request.identity).flatMap { selected =>
          val known = selected.contests.map(_._1).toSet
          val contests = data.contests.filter(known).distinct.sorted
          if (contests.isEmpty) Future.successful(back.flashing("error" -> s"${kind.fullName}: no known contests selected"))
          else for {
            _ <- InitialImportTools.addServiceTeams(db, kind, data.count, contests)
            status <- InitialImportTools.serviceTeamStatus(db, kind)
          } yield back.flashing(
            "success" -> s"${kind.fullName}: ${data.count} teams in contests ${contests.mkString(", ")}.",
            "sheetContest" -> contests.head.toString,
            "sheetSchool" -> status._1.map(_.toString).getOrElse(""))
        }
      )
    }

  private case class RoomAdminsRequest(letter: String, contests: List[Int])

  private val roomAdminsForm = Form {
    mapping("letter" -> nonEmptyText(maxLength = 1), "contests" -> list(number))(RoomAdminsRequest.apply)(RoomAdminsRequest.unapply)
      .verifying("Pick at least one contest", _.contests.nonEmpty)
  }

  def postRoomAdmins(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withSuperAdmin).async { implicit request =>
    val back = Redirect(routes.AdminApplication.miscForms(contestID))
    roomAdminsForm.bindFromRequest.fold(
      formWithErrors => Future.successful(back.flashing("error" ->
        s"Room admins: ${(formWithErrors.errors ++ formWithErrors.globalErrors).map(_.message).mkString("; ")}")),
      data => getSelectedContests(contestID, request.identity).flatMap { selected =>
        val known = selected.contests.map(_._1).toSet
        val contests = data.contests.filter(known).distinct.sorted
        if (contests.isEmpty) Future.successful(back.flashing("error" -> "Room admins: no known contests selected"))
        else for {
          rooms <- InitialImportTools.roomsWithComputers(db)
          logins = InitialImportTools.roomLogins(rooms, data.letter)
          _ <- InitialImportTools.upsertRoomAdmins(db, logins, contests, PasswordHash.hash)
        } yield back.flashing(
          "success" -> s"Room admins: ${logins.size} logins, spectating contests ${contests.mkString(", ")}.",
          "roomLogins" -> logins.map(l => s"${l.username}\t${l.password}").mkString("\n"))
      }
    )
  }

  def addJuryTeams(contestID: Int) = addServiceTeams(contestID, InitialImportTools.JuryTeams)

  def addCoachTeams(contestID: Int) = addServiceTeams(contestID, InitialImportTools.CoachTeams)

  def importNetmapComputers = silhouette.SecuredAction(AdminPermissions.withModify(1)).async { implicit request =>
    InitialImportTools.getNetmapComputers(externalDatabases.netmap.db).flatMap { comps =>
      InitialImportTools.importNetmapComputers(db, comps).map { _ =>
        Redirect(routes.AdminApplication.index)
      }
    }
  }

  def copyContestsForTesting(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(1)).async { implicit request =>
    import com.github.tototoshi.slick.PostgresJodaSupport._
    val now = org.joda.time.DateTime.now
    val action = SlickModel.contests.filter(_.id <= 10).sortBy(_.id).result.flatMap { originals =>
      DBIO.sequence(originals.map { c =>
        SlickModel.getContestByIDToResync(c.id + 10)
          .update((c.name, c.schoolMode, now, c.freezeTime, c.endTime, c.exposeTime, c.polygonID, c.language))
      })
    }
    db.run(action.transactionally).flatMap(_ => Future.traverse(1 to 10)(i => announceContestChanged(i + 10))).map { _ =>
      Redirect(routes.AdminApplication.miscForms(contestID))
    }
  }

  private def normalizeList(s: String) =
    s.split(',').map(_.trim).filter(_.nonEmpty).mkString(",")

  private val aclConstraint = play.api.data.validation.Constraint[String]("constraint.acl") { s =>
    val bad = s.split(',').map(_.trim).filter(_.nonEmpty).filterNot(x => x == "*" || scala.util.Try(x.toInt).isSuccess)
    if (bad.isEmpty) play.api.data.validation.Valid
    else play.api.data.validation.Invalid(s"Not a contest id or *: ${bad.mkString(", ")}")
  }

  private val adminForm = Form {
    mapping(
      "username" -> nonEmptyText,
      "password" -> text,
      "spectator" -> text.verifying(aclConstraint),
      "administrator" -> text.verifying(aclConstraint),
      "locations" -> text,
      "unrestricted" -> text.verifying(aclConstraint)
    )(EditAdmin.apply)(EditAdmin.unapply)
  }

  private def showAdmins(contest: SelectedContest, form: Form[EditAdmin], status: Status)(implicit request: RequestHeader) =
    db.run(SlickModel.admins.sortBy(_.username).result).map { admins =>
      status(html.admin.admins(admins, form, contest))
    }

  def adminsPage(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withSuperAdmin).async { implicit request =>
    getSelectedContests(contestID, request.identity).flatMap { contest =>
      showAdmins(contest, adminForm, Ok)
    }
  }

  def postAddAdmin(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withSuperAdmin).async { implicit request =>
    getSelectedContests(contestID, request.identity).flatMap { contest =>
      adminForm.bindFromRequest.fold(
        formWithErrors => showAdmins(contest, formWithErrors, BadRequest),
        data => {
          val form = adminForm.fill(data.copy(password = ""))
          if (data.password.isEmpty)
            showAdmins(contest, form.withError("password", "Password is required"), BadRequest)
          else
            db.run(SlickModel.admins.filter(_.username === data.username).exists.result).flatMap {
              case true =>
                showAdmins(contest, form.withError("username", "Already exists"), BadRequest)
              case false =>
                val entry = org.stingray.contester.dbmodel.AdminEntry(data.username, PasswordHash.hash(data.password),
                  normalizeList(data.spectator), normalizeList(data.administrator), normalizeList(data.locations),
                  normalizeList(data.unrestricted))
                db.run(SlickModel.admins += entry).map { _ =>
                  Redirect(routes.AdminApplication.adminsPage(contestID)).flashing("success" -> s"Added ${data.username}")
                }
            }
        }
      )
    }
  }

  def editAdmin(contestID: Int, username: String) = silhouette.SecuredAction(AdminPermissions.withSuperAdmin).async { implicit request =>
    getSelectedContests(contestID, request.identity).zip(
      db.run(SlickModel.admins.filter(_.username === username).result.headOption)
    ).map {
      case (contest, Some(a)) =>
        val data = EditAdmin(a.username, "", a.spectator, a.administrator, a.locations, a.unrestricted)
        Ok(html.admin.editadmin(adminForm.fill(data), username, contest))
      case _ => NotFound
    }
  }

  def postEditAdmin(contestID: Int, username: String) = silhouette.SecuredAction(AdminPermissions.withSuperAdmin).async { implicit request =>
    getSelectedContests(contestID, request.identity).flatMap { contest =>
      adminForm.bindFromRequest.fold(
        formWithErrors => Future.successful(BadRequest(html.admin.editadmin(formWithErrors, username, contest))),
        data => {
          val administrator = normalizeList(data.administrator)
          val dropsOwnSuper = username == request.identity.username && !administrator.split(',').contains("*")
          if (dropsOwnSuper)
            Future.successful(BadRequest(html.admin.editadmin(
              adminForm.fill(data.copy(password = "")).withError("administrator", "You can't remove * from your own account"),
              username, contest)))
          else {
            val q = SlickModel.admins.filter(_.username === username)
            val perms = q.map(x => (x.spectator, x.administrator, x.locations, x.unrestricted))
              .update((normalizeList(data.spectator), administrator, normalizeList(data.locations), normalizeList(data.unrestricted)))
            val action =
              if (data.password.isEmpty) perms
              else perms.andThen(q.map(_.password).update(PasswordHash.hash(data.password)))
            db.run(action.transactionally).map { _ =>
              Redirect(routes.AdminApplication.adminsPage(contestID)).flashing("success" -> s"Updated $username")
            }
          }
        }
      )
    }
  }

  def deleteAdmin(contestID: Int, username: String) = silhouette.SecuredAction(AdminPermissions.withSuperAdmin).async { implicit request =>
    if (username == request.identity.username)
      Future.successful(Redirect(routes.AdminApplication.adminsPage(contestID)).flashing("error" -> "You can't delete your own account"))
    else
      db.run(SlickModel.admins.filter(_.username === username).delete).map { _ =>
        Redirect(routes.AdminApplication.adminsPage(contestID)).flashing("success" -> s"Deleted $username")
      }
  }

  private val extraInfoForm = Form(
    mapping(
      "num" -> number(min = 0),
      "heading" -> text.transform[String](_.trim, identity).verifying("Required", _.nonEmpty),
      "data" -> text
    )((num, heading, data) => org.stingray.contester.dbmodel.Extrainfo(0, num, heading, data))(x => Some((x.num, x.heading, x.data)))
  )

  def extraInfoPage(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity).zip(AdminExtraInfo.list(db, contestID)).map {
      case (contest, pages) => Ok(html.admin.extrainfo(pages, contest))
    }
  }

  def newExtraInfo(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity).zip(AdminExtraInfo.list(db, contestID)).map {
      case (contest, pages) =>
        val next = if (pages.isEmpty) 1 else pages.map(_.num).max + 1
        Ok(html.admin.editextrainfo(extraInfoForm.fill(org.stingray.contester.dbmodel.Extrainfo(contestID, next, "", "")), None, contest))
    }
  }

  def editExtraInfo(contestID: Int, num: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity).zip(AdminExtraInfo.get(db, contestID, num)).map {
      case (contest, Some(page)) => Ok(html.admin.editextrainfo(extraInfoForm.fill(page), Some(num), contest))
      case _ => NotFound
    }
  }

  /** Adds a page when `original` is empty, otherwise saves the page that has that number now. */
  def postExtraInfo(contestID: Int, original: Option[Int]) = silhouette.SecuredAction(AdminPermissions.withModify(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity).flatMap { contest =>
      extraInfoForm.bindFromRequest.fold(
        formWithErrors => Future.successful(BadRequest(html.admin.editextrainfo(formWithErrors, original, contest))),
        data => AdminExtraInfo.save(db, contestID, original, data).map {
          case AdminExtraInfo.Saved =>
            Redirect(routes.AdminApplication.extraInfoPage(contestID)).flashing("success" -> s"Saved page ${data.num}")
          case AdminExtraInfo.NumberTaken =>
            BadRequest(html.admin.editextrainfo(extraInfoForm.fill(data).withError("num", s"Page ${data.num} already exists"), original, contest))
          case AdminExtraInfo.Missing =>
            Redirect(routes.AdminApplication.extraInfoPage(contestID)).flashing("error" -> "That page was deleted meanwhile")
        }
      )
    }
  }

  def deleteExtraInfo(contestID: Int, num: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestID)).async { implicit request =>
    AdminExtraInfo.delete(db, contestID, num).map { found =>
      Redirect(routes.AdminApplication.extraInfoPage(contestID))
        .flashing(if (found) "success" -> s"Deleted page $num" else "error" -> s"No page $num")
    }
  }

  private val areaPrinterForm = Form(single("printer" -> text))

  def areasPage(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withSuperAdmin).async { implicit request =>
    getSelectedContests(contestID, request.identity)
      .zip(db.run(SlickModel.areas.sortBy(_.id).result))
      .zip(AgentPrinterList.all(db)).map {
      case ((contest, areas), printers) => Ok(html.admin.areas(areas, printers, contest))
    }
  }

  def postEditArea(contestID: Int, areaID: Int) = silhouette.SecuredAction(AdminPermissions.withSuperAdmin).async { implicit request =>
    areaPrinterForm.bindFromRequest.fold(
      _ => Future.successful(BadRequest),
      printer => db.run(SlickModel.areas.filter(_.id === areaID).map(_.printer).update(printer.trim)).map { n =>
        Redirect(routes.AdminApplication.areasPage(contestID)).flashing(
          if (n == 0) "error" -> s"No area $areaID" else "success" -> s"Saved printer for area $areaID")
      }
    )
  }

  def deleteArea(contestID: Int, areaID: Int) = silhouette.SecuredAction(AdminPermissions.withSuperAdmin).async { implicit request =>
    val back = Redirect(routes.AdminApplication.areasPage(contestID))
    db.run(SlickModel.compLocations.filter(_.location === areaID).length.result).flatMap {
      case 0 =>
        db.run(SlickModel.areas.filter(_.id === areaID).delete).map { n =>
          back.flashing(if (n == 0) "error" -> s"No area $areaID" else "success" -> s"Deleted area $areaID")
        }
      case n =>
        Future.successful(back.flashing("error" -> s"Area $areaID still has $n computers; delete them first"))
    }
  }

  def computersPage(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withSuperAdmin).async { implicit request =>
    val q = (for {
      c <- SlickModel.compLocations
      a <- SlickModel.areas if a.id === c.location
    } yield (c.id, c.name, a.name)).sortBy(x => (x._3, x._2))
    getSelectedContests(contestID, request.identity).zip(db.run(q.result)).map {
      case (contest, comps) => Ok(html.admin.computers(comps.map(x => (x._1.address, x._2, x._3)), contest))
    }
  }

  def deleteComputers(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withSuperAdmin).async { implicit request =>
    val addrs = request.body.asFormUrlEncoded.flatMap(_.get("addr")).getOrElse(Seq.empty)
    val back = Redirect(routes.AdminApplication.computersPage(contestID))
    if (addrs.isEmpty)
      Future.successful(back.flashing("error" -> "Nothing selected"))
    else
      db.run(SlickModel.compLocations.filter(_.id inSet addrs.map(com.github.tminglei.slickpg.InetString(_))).delete).map { n =>
        back.flashing("success" -> s"Deleted $n computers")
      }
  }

  def miscForms(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(1)).async { implicit request =>
    getSelectedContests(contestID, request.identity).flatMap { contest =>
      for {
        comps <- InitialImportTools.getNetmapComputers(externalDatabases.netmap.db)
        statuses <- Future.sequence(InitialImportTools.ServiceTeamKinds.map { kind =>
          InitialImportTools.serviceTeamStatus(db, kind).map { case (schoolId, count) => ServiceTeamsStatus(kind, schoolId, count) }
        })
      } yield Ok(html.admin.misc(comps, statuses, contest, request.identity))
    }
  }

  /** Runs a custom test again: the director takes it from the queue like a fresh one and writes the result over the old. */
  def rerunCustomTest(contestId: Int, id: Long) =
    silhouette.SecuredAction(AdminPermissions.withModify(contestId)).async { implicit request =>
      AdminCustomTests.reset(db, contestId, id).map {
        case true =>
          rabbitMq ! Message.queue(ServerSideEvalID(id.toInt), queue = "contester.evalrequests")
          Redirect(routes.AdminApplication.showCustomTest(contestId, id))
        case false => NotFound
      }
    }

}
