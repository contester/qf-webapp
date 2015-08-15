package controllers

import javax.inject.{Inject, Singleton}

import akka.actor.{ActorSystem, Props}
import com.spingo.op_rabbit.{QueueMessage, RabbitControl}
import jp.t2v.lab.play2.auth.AuthElement
import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{Controller, RequestHeader}
import slick.driver.JdbcProfile
import slick.jdbc.GetResult
import views.html
import com.github.nscala_time.time.Imports._

import scala.concurrent.Future

case class RejudgeSubmitRange(range: String)
case class PostClarification(id: Option[Int], contest: Int, problem: String, text: String, date: Option[DateTime], hidden: Boolean)

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
    monitorModel.getMonitor(id, true).map(x => Ok(html.monitor(x.get.contest, x.get.status)))
  }

  private def anyUser(account: Admin): Future[Boolean] = Future.successful(true)

  private def showSubs(contestId: Int, limit: Option[Int])(implicit request: RequestHeader) =
    db.run(Contests.getContest(contestId)).map(_.headOption).zip(
      db.run(Contests.getTeams(contestId)).map(_.map(x => x.localId -> x).toMap)
    ).zip(db.run(Submits.getContestSubmits(contestId))).flatMap {
      case ((Some(contest), teamMap), submits) =>
        Submits.groupAndAnnotate(db, contest.schoolMode, limit.map(submits.take).getOrElse(submits)).map { fullyDescribedSubmits =>
          Ok(html.admin.submits(fullyDescribedSubmits, teamMap))
        }
      case _ =>
        Future.successful(Ok(html.admin.submits(Nil, Map())))
    }

  def index = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    showSubs(1, Some(20))
  }

  case class IdRange(left: Option[Int], right: Option[Int]) extends Function[Int, Boolean] {
    def apply(id: Int) =
      left.map(_ <= id).getOrElse(true) &&
      right.map(_ >= id).getOrElse(true)
  }

  val rangeRe = "(\\d*)\\.\\.(\\d*)".r

  def parseItem(item: String): Either[Int, IdRange] =
    if (item.contains("..")) {
      item match {
        case rangeRe(left, right) =>
          Right(IdRange(if (left.isEmpty) None else Some(left.toInt), if (right.isEmpty) None else Some(right.toInt)))
      }
    } else Left(item.toInt)

  import slick.driver.MySQLDriver.api._

  def rejudgeRangeEx(range: String) = {
    val items = range.split(',').map(parseItem)
    val numbers = items.filter(_.isLeft).map(_.left.get).toSet
    val ranges = items.filter(_.isRight).map(_.right.get)

    val checks: Seq[Function[Int, Boolean]] = ranges.toSeq.+:(numbers)

    db.run(sql"""select ID from NewSubmits order by ID""".as[Int]).map { submits =>
      for (id <- submits) {
        if (checks.exists(x => x(id))) {
          rabbitMq ! QueueMessage(SubmitMessage(id), queue = "contester.submitrequests")
        }
      }
      ()
    }
  }


  def submits(contestId: Int) = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    showSubs(contestId, None)
  }

  val rejudgeSubmitRangeForm = Form {
    mapping("range" -> text)(RejudgeSubmitRange.apply)(RejudgeSubmitRange.unapply)
  }

  def rejudgePage = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    Future.successful(Ok(html.admin.rejudge(rejudgeSubmitRangeForm)))
  }

  def rejudgeRange = AsyncStack(parse.multipartFormData, AuthorityKey -> anyUser) { implicit request =>
    rejudgeSubmitRangeForm.bindFromRequest.fold(
      formWithErrors => Future.successful(BadRequest(html.admin.rejudge(formWithErrors))),
      data => rejudgeRangeEx(data.range).map { _ =>
        Redirect(routes.AdminApplication.rejudgePage)
      }
    )
  }

  def showSubmit(submitId: Int) = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    Submits.getSubmitById(db, submitId).map(x => Ok(html.admin.showsubmit(x)))
  }

  val postClarificationForm = Form {
    mapping("id" -> optional(number),
      "contest" -> number,
      "problem" -> text,
      "text" -> nonEmptyText,
      "date" -> optional(jodaDate),
      "hidden" -> boolean
    )(PostClarification.apply)(PostClarification.unapply)
  }

  def postNewClarification(contestId: Int) = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    Future.successful(Ok(html.admin.postclarification(postClarificationForm, contestId)))
  }

  case class Clarification1(id: Int, contest: Int, problem: String, text: String, date: DateTime, hidden: Boolean)
  object Clarification1 {
    implicit val getResult = GetResult(r =>
      Clarification1(r.nextInt(), r.nextInt(), r.nextString(), r.nextString(), new DateTime(r.nextTimestamp()),
        r.nextBoolean())
    )
  }

  import utils.Db._

  def postUpdateClarification(clarificationId: Int) = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
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


}