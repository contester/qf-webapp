package controllers

import actors.StatusActor.ClarificationAnswered
import actors.StatusActor
import akka.NotUsed
import akka.stream.scaladsl.{Merge, Source}
import com.github.nscala_time.time.Imports._
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.{Authorization, Silhouette}
import com.mohiva.play.silhouette.impl.authenticators.SessionAuthenticator
import models._
import org.apache.commons.io.FileUtils
import org.stingray.contester.dbmodel.{Clarification, Problem}
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
import utils.Ask
import views.html

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/** all: rejudge every submit of the problem, not just the outdated ones. */
case class RejudgeProblem(problem: String, all: Boolean)
case class PostClarification(problem: String, text: String, hidden: Boolean)
case class ClarificationResponse(answer: String)


class AdminApplication (cc: ControllerComponents, silhouette: Silhouette[AdminEnv], dbConfig: DatabaseConfig[JdbcProfile],
                             monitorModel: Monitor,
                             statusActorModel: StatusActorModel,
                                  configuration: Configuration,
                                  ws: WSClient,
                        liveHooks: live.LiveHooks,
                        liveMonitor: live.LiveMonitorRef,
                        stats: live.ConnectionStats) extends AbstractController(cc)  with I18nSupport with Logging {

  implicit val ec = defaultExecutionContext

  private val db = dbConfig.db
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
      val selected = cmap.getOrElse(contestId, contests.headOption.getOrElse(
        throw new NoSuchElementException("there are no current rounds")))
      SelectedContest(selected, cmap.mapValues(_.name).toSeq, account.canModify(selected.id), account.isSuperAdmin)
    }

  // How many connections each feed has, for global admins: the page, and the stream it is updated by.
  def contester(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withSuperAdmin).async { implicit request =>
    getSelectedContests(contestId, request.identity).map { contest =>
      Ok(html.admin.contester(contest)).withHeaders(LiveStreams.pageHeaders: _*)
    }
  }

  def contesterEvents = silhouette.SecuredAction(AdminPermissions.withSuperAdmin) { implicit request =>
    LiveStreams.response(stats.events())
  }

  // The live points board of the round: admins see it always, the real one, while the round runs too. The page
  // carries the board the stream starts with; when the live monitor cannot give one (it does not hold the round),
  // the board is rendered from the database instead.
  def monitor(id: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(id)).async { implicit request =>
    monitorModel.source.round(id).flatMap { round =>
      BoardAccess.decide(round, AdminViewer, org.joda.time.DateTime.now) match {
        case Left(result) => Future.successful(result)
        case Right(contest) =>
          getSelectedContests(id, request.identity).zip(LiveStreams.snapshot(liveMonitor.ref, LiveStreams.adminJoin(contest.id))).flatMap {
            case (selected, Some(initial)) =>
              Future.successful(Ok(html.admin.livemonitor(selected.copy(contest = contest), initial))
                .withHeaders(LiveStreams.pageHeaders: _*))
            case (selected, None) =>
              monitorModel.source.board(contest.id).map(board => Ok(html.admin.monitor(selected.copy(contest = contest), board)))
          }
      }
    }
  }

  // The admin live board's stream (deltas of the points board); admins only.
  def monitorEvents(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestId)).async { implicit request =>
    LiveStreams.stream(liveMonitor.ref, LiveStreams.adminJoin(contestId), stats)
  }


  /** Spectators of a submit's round may open it (there is no freeze: every submit is visible to them). */
  case class canSeeSubmit(submitId: Int) extends Authorization[Admin, SessionAuthenticator] {
    override def isAuthorized[B](identity: Admin, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] =
      getSubmitCid(submitId).map(_.exists(identity.canSpectate))
  }

  case class canRejudgeSubmit(submitId: Int) extends Authorization[Admin, SessionAuthenticator] {
    override def isAuthorized[B](identity: Admin, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] =
      getSubmitCid(submitId).map { cids =>
        cids.exists(identity.canModify)
      }
  }

  def index = silhouette.SecuredAction.async { implicit request =>
    getAllContests.map { contests =>
      contests.sortBy(_.id).headOption match {
        case Some(c) => Redirect(routes.AdminApplication.submits(c.id, None, None, None))
        case None => Redirect(routes.AdminApplication.showContestList(0))
      }
    }
  }

  // The status page's rows as they arrive and finish.
  def statusEvents(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestId)).async { implicit request =>
    LiveStreams.stream(liveMonitor.ref,
      LiveStreams.adminStatusJoin(contestId), stats)
  }

  def submits(contestId: Int, team: Option[Int], before: Option[Long], after: Option[Long]) =
    silhouette.SecuredAction(AdminPermissions.withSpectate(contestId)).async { implicit request =>
      val account = request.identity
      getSelectedContests(contestId, account).zip(statusActorModel.teamClient.getTeams(contestId)).flatMap {
        case (contest, teamMap) =>
          AdminSubmits.page(db, contestId, team, before, after).map { page =>
            Ok(html.admin.submits(page, teamMap, contest, account, team,
              live = liveMonitor.ref.isDefined, newest = before.isEmpty && page.newer.isEmpty))
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
            ids.flatMap(ids => AdminSubmits.resetForRejudge(db, ids).map(_ => ids)).map { ids =>
              Try(liveHooks.rejudgedAll(ids.map(_.toInt)))
              back.flashing("success" -> s"${ids.size} ${if (data.all) "(all) " else ""}submits of problem ${data.problem}")
            }
        }
      }
    )
  }

  // Called from the status page via ajax, which has nothing to do with a response body.
  def rejudgeSubmit(submitId: Int) = silhouette.SecuredAction(canRejudgeSubmit(submitId)).async { implicit request =>
    AdminSubmits.resetForRejudge(db, Seq(submitId.toLong)).map { _ =>
      Try(liveHooks.rejudged(submitId))
      NoContent
    }
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
            case Some(_) =>
              Outputs.getAllAssets2(ws, fileserverBase, shortn, submit.fsub.submit.submitId.id, tid.toInt)
            case None => Future.successful(Map[Int, ResultAssets]())
          }
        }.getOrElse(Future.successful(Map[Int, ResultAssets]()))
        val rowsF = viewed.map(Submits.loadTestRows(db, _)).getOrElse(Future.successful(Nil))
        outputsF.zip(getSelectedContests(contestId, request.identity))
          .zip(AdminSubmits.runHistory(db, submitId, submit.fsub.submit.testingId.map(_.toLong))).zip(rowsF).map {
          case (((outputs, contest), runs), rows) =>
            Ok(html.admin.showsubmit(submit, contest, outputs, runs, viewed, rows))
        }
      case None =>
        Future.successful(NotFound)
    }
  }

  def downloadArchiveSubmit(contestId: Int, submitId: Int) = silhouette.SecuredAction(canSeeSubmit(submitId)).async { implicit request =>
    val q = for {
      s <- SlickModel.submits if s.id === submitId.toLong
      t <- SlickModel.testings if s.testingID === t.id
    } yield t.id

    db.run(q.take(1).result.headOption).map {
        case Some(testingID) =>
          // The bridge's /fs2/ proxy finds the archive by the testing id alone; no problem prefix is needed.
          val downloadLoc = s"/fs2/?contest=$shortn&submit=${submitId}&testing=${testingID}"
          logger.trace(s"download: $downloadLoc")
          Ok("download").as("application/zip").withHeaders("X-Accel-Redirect" -> downloadLoc)
        case None => NotFound
    }
  }

  def showQandA(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestId)).async { implicit request =>
    db.run(SlickModel.getClarificationsForContest(contestId).result)
      .zip(ClarificationModel.getClarificationReqs(db, contestId))
      .zip(getSelectedContests(contestId, request.identity))
      .map {
      case ((clarifications, clReqs), contest) =>
        Ok(html.admin.qanda(clarifications, clReqs, contest))
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

  private def joinAdminFeed(contestId: Int) =
    Ask[Source[Event, NotUsed]](statusActorModel.statusActor, StatusActor.JoinAdmin(contestId))

  def feed(contestId: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestId)).async { implicit request =>
    joinAdminFeed(contestId).map { e =>
      Ok.chunked(e.via(stats.track(live.FeedKind.AdminFeed, contestId))).as(ContentTypes.EVENT_STREAM)
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

  def showContestList(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestID)).async { implicit request =>
    Rounds.currentRounds(db).flatMap { rounds =>
      if (rounds.isEmpty)
        // no round, so no page chrome (it needs a selected round): say so instead of failing
        Future.successful(Ok(play.twirl.api.Html("<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>No rounds</title></head>" +
          "<body><p>There are no rounds with a start and an end in the current season.</p></body></html>")))
      else
        getSelectedContests(contestID, request.identity).map { contest =>
          Ok(html.admin.contestlist(rounds.filter(x => request.identity.canSpectate(x.id)), contest))
        }
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

  /** Runs a custom test again as a copy with a new id (the bridge picks it up like a fresh one); shows the copy. */
  def rerunCustomTest(contestId: Int, id: Long) =
    silhouette.SecuredAction(AdminPermissions.withModify(contestId)).async { implicit request =>
      AdminCustomTests.rerun(db, contestId, id).map {
        case Some(copy) => Redirect(routes.AdminApplication.showCustomTest(contestId, copy))
        case None => NotFound
      }
    }

}
