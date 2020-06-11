package controllers

import actors.StatusActor.ClarificationAnswered
import actors.{StatusActor, WaiterActor}
import akka.NotUsed
import akka.stream.scaladsl.{Merge, Source}
import com.github.nscala_time.time.Imports._
import com.google.common.collect.ImmutableRangeSet
import com.mohiva.play.silhouette.api.{Authorization, Silhouette}
import com.mohiva.play.silhouette.impl.authenticators.SessionAuthenticator
import com.spingo.op_rabbit.Message
import models._
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

case class RejudgeSubmitRange(range: String)
case class PostClarification(problem: String, text: String, hidden: Boolean)
case class ClarificationResponse(answer: String)

case class PostWaiterTask(message: String, rooms: String)

case class TeamDescription(id: Int, school: School)

case class ContestDescription(id: Int)

case class EditTeam(schoolName: String, teamName: String, teamNum: Option[Int])

case class DisplayTeam(team: SlickModel.Team, school: School, login: String, password: String)

case class AdminPrintJob(id: Int, contestID: Int, team: Team, filename: String, arrived: DateTime,
                         printed: Option[DateTime], computerName: String, error: String)

case class SubmitIdLite(id: Int)
object SubmitIdLite {
  implicit val format = Json.format[SubmitIdLite]
}

case class SubmitTicketLite(submit: SubmitIdLite)
object SubmitTicketLite {
  implicit val format = Json.format[SubmitTicketLite]
}

case class EditContest(name: String, schoolMode: Boolean, startTime: DateTime, freezeTime: DateTime, endTime: DateTime,
                       exposeTime: DateTime, polygonID: String, language: String)

case class ImportTeamsProps(contests: Seq[Int])

class AdminApplication (cc: ControllerComponents, silhouette: Silhouette[AdminEnv], dbConfig: DatabaseConfig[JdbcProfile],
                             monitorModel: Monitor,
                                 rabbitMqModel: RabbitMqModel,
                             statusActorModel: StatusActorModel,
                        printingModel: PrintingModel,
                                  configuration: Configuration,
                        externalDatabases: ExternalDatabases,
                                  ws: WSClient) extends AbstractController(cc)  with I18nSupport with Logging {

  implicit val ec = defaultExecutionContext

  private val db = dbConfig.db
  import com.spingo.op_rabbit.PlayJsonSupport._

  private val rabbitMq = rabbitMqModel.rabbitMq

  private val fileserverUrl = configuration.get[String]("fileserver.url")
  private val shortn = configuration.get[String]("fileserver.shortn")

  private val fileserverBase = fileserverUrl.stripSuffix("fs/")

  logger.info(s"fileserverBase: $fileserverBase")

  import utils.MyPostgresProfile.api._

  private[this] def getSubmitCid(submitId: Long)(implicit ec: ExecutionContext): Future[Option[Int]] =
    db.run(SlickModel.submits.filter(_.id === submitId).take(1).map(_.contest).result.headOption)

  private[this] def rejudgeRangeEx(range: String, account: Admin)(implicit ec: ExecutionContext): Future[Seq[Long]] = {
    val checks = {
      val builder = ImmutableRangeSet.builder[java.lang.Long]()
      range.split(',').map(parseItem).foreach(builder.add)
      builder.build()
    }
    db.run(SlickModel.submits.map(x => (x.id, x.contest)).sortBy(_._1).result).map { submits =>
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
      statusActorModel.teamClient.getTeams(contestId)
    ).zip(db.run(Submits.getContestSubmits(contestId))).flatMap {
      case ((contest, teamMap), submitsx) =>
        val canSeeAll = account.canSeeAll(contestId)
        def canSeeSubmit(s: Submit) =
          if (canSeeAll) true else !s.afterFreeze
        val submits = submitsx.map(Submits.upliftSub(_)).filter(canSeeSubmit)
        Submits.groupAndAnnotate(db, false, limit.map(submits.takeRight(_)).getOrElse(submits)).map { fullyDescribedSubmits =>
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

  private[this] val rangeRe = "(\\d*)\\.\\.(\\d*)".r

  private[this] def parseItem(item: String): com.google.common.collect.Range[java.lang.Long] =
    if (item.contains("..")) {
      item match {
        case rangeRe(left, right) =>
          if (left.isEmpty) {
            if (right.isEmpty) {
              com.google.common.collect.Range.all[java.lang.Long]()
            } else {
              com.google.common.collect.Range.atMost[java.lang.Long](right.toLong)
            }
          } else {
            if (right.isEmpty) {
              com.google.common.collect.Range.atLeast[java.lang.Long](left.toLong)
            } else {
              com.google.common.collect.Range.closed[java.lang.Long](left.toLong, right.toLong)
            }
          }
      }
    } else com.google.common.collect.Range.singleton[java.lang.Long](item.toLong)


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
    db.run(SlickModel.testings.filter(_.id === testingID.toLong).result.headOption)

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
          testingOpt.map { testing =>
            val problemId = testing._4
              val phandle = PolygonURL(problemId)
              Outputs.getAllAssets2(ws, fileserverBase, shortn, submit.fsub.submit.submitId.id, testing._1.toInt, submit.fsub.details.map(_.test), phandle)
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
    val q = for {
      s <- SlickModel.submits if s.id === submitId.toLong
      t <- SlickModel.testings if s.testingID === t.id
    } yield (s.testingID, t.problemURL)

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

  def reprintSubmit(submitId: Int) = silhouette.SecuredAction(canRejudgeSubmit(submitId)).async { implicit request =>
    rabbitMq ! Message.queue(SubmitTicketLite(SubmitIdLite(submitId)), queue = "contester.tickets")
    Future.successful(Ok("ok"))
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

  // TODO: check permissions, do just toggle instead of full update
  def toggleClarification(clrId: Long) = silhouette.SecuredAction.async { implicit request =>
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

  // TODO: check permissions
  def postUpdateClarification(clarificationId: Long) = silhouette.SecuredAction.async { implicit request =>
    db.run(SlickModel.clarifications.filter(_.id === clarificationId).result.headOption).flatMap { clOpt =>
      clOpt.map { cl =>
        val clObj = PostClarification(
          cl.problem, cl.text, cl.hidden
        )
        getSelectedContests(cl.contest, request.identity).flatMap { contest =>
          statusActorModel.problemClient.getProblems(contest.contest.id).map { problems =>
            Ok(html.admin.postclarification(cl.id, postClarificationForm.fill(clObj), selectableProblems(problems), contest))
          }
        }
      }.getOrElse(Future.successful(Redirect(routes.AdminApplication.postNewClarification(1))))
    }
  }

  // TODO: check permissions
  def postClarification(contestId: Int, clarificationId: Option[Long]) = silhouette.SecuredAction.async { implicit request =>
    postClarificationForm.bindFromRequest.fold(
      formWithErrors => getSelectedContests(1, request.identity).flatMap { contest =>
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
    db.run(SlickModel.clarificationRequests.filter(_.id === id).result.headOption)

  // TODO: check permissions
  def postAnswerForm(clrId: Long) = silhouette.SecuredAction.async { implicit request =>
    getClarificationReq(clrId).flatMap { optClr =>
      optClr.map { clr =>
        getSelectedContests(clr.contest, request.identity).map { contest =>
          Ok(html.admin.postanswer(
            clarificationResponseForm.fill(ClarificationResponse(clr.getAnswer)), clr, answerList.toSeq, contest))
        }
      }.getOrElse(Future.successful(Redirect(routes.AdminApplication.showQandA(1))))
    }
  }

  // TODO: check permissions
  def postAnswer(clrId: Long) = silhouette.SecuredAction(parse.multipartFormData).async { implicit request =>
    getClarificationReq(clrId).flatMap { optClr =>
      optClr.map { clr =>
        clarificationResponseForm.bindFromRequest.fold(
          formWithErrors => getSelectedContests(clr.contest, request.identity).map { contest =>
            BadRequest(html.admin.postanswer(formWithErrors, clr, answerList.toSeq, contest))
          },
          data => {
            db.run(SlickModel.clarificationRequests.filter(_.id === clrId).map(x => (x.answer, x.answered)).update((data.answer,true))).map { _ =>
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
    import models.SlickModel._

    db.run(
      (for {
        (j, l) <- dbPrintJobs.filter(_.contest === contestID) join compLocations on (_.computer === _.id)
      } yield (j.id, j.contest, j.team, j.filename, j.arrived, j.printed, l.name, j.error)).sortBy(_._5.desc).result)
    .zip(statusActorModel.teamClient.getTeams(contestID)).map {
      case (jobs, teams) =>
        jobs.flatMap { src =>
          teams.get(src._3).map { team =>
            AdminPrintJob(src._1.toInt, src._2, team, src._4, src._5, src._6, src._7, src._8)
          }
        }
    }
  }

  def listPrintJobs(contestID: Int)= silhouette.SecuredAction(AdminPermissions.withSpectate(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity)
      .zip(getPrintJobs(contestID))
      .map {
        case (contest, jobs) =>
          Ok(html.admin.printjobs(jobs, contest, request.identity))
      }

  }

  def reprintJob(printJobID: Int) = silhouette.SecuredAction.async { implicit request =>
    printingModel.printJobByID(printJobID).map { _ =>
      Ok("ok")
    }
  }

  def listTeams(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity).flatMap { contest =>
      db.run((for {
        t <- SlickModel.teams
        p <- SlickModel.participants if (t.id === p.team && p.contest === contestID)
        s <- SlickModel.schools if (s.id === t.school)
        a <- SlickModel.assignments if (a.team === p.team && a.contest === p.contest)
      } yield (t, p, s, a)).result).map { results =>
        val cvt = results.map {
          case (t, p, s, a) =>
            DisplayTeam(t, s, a.username, a.password)
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
    Ok(html.admin.editteam(form.getOrElse(editTeamForm.fill(EditTeam(school.name, team.name, if (team.num!=0) Some(team.num) else None))), TeamDescription(team.id, school), contest))
  }

  def editTeam(contestID: Int, teamID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestID)).async { implicit request =>
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

  def postEditTeam(contestID: Int, teamID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(contestID)).async { implicit request =>
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

  def showContestList(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity).flatMap { contest =>
      db.run(SlickModel.contests.sortBy(_.id).result).map { results =>
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
      "polygonID" -> text,
      "language" -> text
    )(EditContest.apply)(EditContest.unapply)
  }

  def editContest(contestID: Int, ceditID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(ceditID)).async { implicit request =>
    getSelectedContests(contestID, request.identity).zip(
      db.run(SlickModel.contests.filter(_.id === ceditID).take(1).result.headOption)
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
    import utils.Db._
    import com.github.tototoshi.slick.MySQLJodaSupport._
    getSelectedContests(contestID, request.identity).flatMap { sc =>
            editContestForm.bindFromRequest.fold(
              formWithErrors => {
                Future.successful(Ok(html.admin.editcontest(formWithErrors, ContestDescription(ceditID), sc)))
              },
              data => {
                val upd = SlickModel.contests.filter(_.id === ceditID).map(x => (x.name, x.schoolMode, x.startTime, x.freezeTime, x.endTime, x.exposeTime, x.polygonId, x.language))
                    .update((data.name, data.schoolMode, data.startTime, data.freezeTime, data.endTime, data.exposeTime, data.polygonID, data.language))
                db.run(upd).map { v =>
                  Redirect(routes.AdminApplication.editContest(contestID, ceditID))
                }
              },
            )
    }
  }

  def showImportedTeamList(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withSpectate(contestID)).async { implicit request =>
    getSelectedContests(contestID, request.identity).flatMap { contest =>
      InitialImportTools.getImportedTeamsEn(externalDatabases.studentWeb.db).map { results =>
        Ok(html.admin.importedteams(results.toSeq, contest, request.identity))
      }
    }}

  def importTeams(contestID: Int)= silhouette.SecuredAction(AdminPermissions.withModify(1)).async { implicit request =>
    getSelectedContests(contestID, request.identity).flatMap { contest =>
      InitialImportTools.getImportedTeamsEn(externalDatabases.studentWeb.db).map { results =>
        Redirect(routes.AdminApplication.index)
      }
    }
  }

  def importNetmapComputers = silhouette.SecuredAction(AdminPermissions.withModify(1)).async { implicit request =>
    InitialImportTools.getNetmapComputers(externalDatabases.netmap.db).flatMap { comps =>
      InitialImportTools.importNetmapComputers(db, comps).map { _ =>
        Redirect(routes.AdminApplication.index)
      }
    }
  }

  def miscForms(contestID: Int) = silhouette.SecuredAction(AdminPermissions.withModify(1)).async { implicit request =>
    getSelectedContests(contestID, request.identity).flatMap { contest =>
      InitialImportTools.getNetmapComputers(externalDatabases.netmap.db).map { comps =>
        Ok(html.admin.misc(comps, contest, request.identity))
      }
    }
  }
}