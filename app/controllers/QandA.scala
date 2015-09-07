package controllers

import javax.inject.Inject

import akka.actor.{ActorSystem, Props}
import com.spingo.op_rabbit.{RabbitControl, Message}
import jp.t2v.lab.play2.auth.AuthElement
import models.{AuthConfigImpl, LoggedInTeam, Monitor, Problems}
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.Json
import play.api.mvc.{Controller, RequestHeader}
import slick.driver.JdbcProfile
import views.html

import scala.concurrent.{ExecutionContext, Future}


case class ClarificationReqData(problem: String, text: String)

case class ClarificationRequestId(id: Int)
object ClarificationRequestId {
  implicit val format = Json.format[ClarificationRequestId]
}

class QandA @Inject() (dbConfigProvider: DatabaseConfigProvider,
                             monitorModel: Monitor,
                             val auth: AuthWrapper,
                       system: ActorSystem,
                             val messagesApi: MessagesApi) extends Controller with AuthElement with AuthConfigImpl with I18nSupport {

  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import dbConfig.driver.api._

  val rabbitMq = system.actorOf(Props[RabbitControl])
  import com.spingo.op_rabbit.PlayJsonSupport._

  private def anyUser(account: LoggedInTeam): Future[Boolean] = Future.successful(true)

  private val clarificationReqForm = Form {
    mapping("problem" -> text, "text" -> text)(ClarificationReqData.apply)(ClarificationReqData.unapply)
  }

  private def clrForm(loggedInTeam: LoggedInTeam, form: Form[ClarificationReqData])(implicit request: RequestHeader, ec: ExecutionContext) =
    db.run(loggedInTeam.contest.getProblems).flatMap { probs =>
      db.run(loggedInTeam.contest.getClarifications).flatMap { clars =>
        db.run(loggedInTeam.getClarificationRequests).map { clReq =>
          html.clarifications(loggedInTeam, clars, clReq, Problems.toSelect(probs), form)
        }
      }
    }

  def index = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    implicit val ec = StackActionExecutionContext
    clrForm(loggedIn, clarificationReqForm).map(Ok(_))
  }

  def post = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn
    implicit val ec = StackActionExecutionContext

    clarificationReqForm.bindFromRequest.fold(
      formWithErrors => clrForm(loggedIn, formWithErrors).map(BadRequest(_)),
      clrData => {
        db.run(
          (sqlu"""insert into ClarificationRequests (Contest, Team, Problem, Request, Arrived) values
                (${loggedInTeam.contest.id}, ${loggedInTeam.team.localId}, ${clrData.problem}, ${clrData.text},
                CURRENT_TIMESTAMP())
              """.andThen(sql"select last_insert_id()".as[Int])).withPinnedSession
        ).map { clrIds =>
          clrIds.foreach { clrId =>
            rabbitMq ! Message.queue(ClarificationRequestId(clrId), queue = "contester.clarificationrequests")
          }
          Redirect(routes.QandA.index)
        }
      }
    )
  }

}