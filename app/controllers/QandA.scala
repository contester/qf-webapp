package controllers

import javax.inject.Inject

import actors.StatusActor
import actors.StatusActor.ClarificationRequested
import akka.actor.{ActorSystem, Props}
import com.spingo.op_rabbit.{RabbitControl, Message}
import jp.t2v.lab.play2.auth.AuthElement
import models._
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

class QandA @Inject() (dbConfigProvider: DatabaseConfigProvider,
                       val auth: AuthWrapper,
                       statusActorModel: StatusActorModel,
                       val messagesApi: MessagesApi) extends Controller with AuthElement with AuthConfigImpl with I18nSupport {
  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import dbConfig.driver.api._

  private val clarificationReqForm = Form {
    mapping("problem" -> text, "text" -> text)(ClarificationReqData.apply)(ClarificationReqData.unapply)
  }

  private def clrForm(loggedInTeam: LoggedInTeam, form: Form[ClarificationReqData])(implicit request: RequestHeader, ec: ExecutionContext) =
    db.run(loggedInTeam.contest.getProblems).flatMap { probs =>
      ClarificationModel.getVisibleClarifications(db, loggedInTeam.contest.id).flatMap { clars =>
        ClarificationModel.getTeamClarificationReqs(db, loggedInTeam.contest.id, loggedInTeam.team.localId).map { clReq =>
          html.clarifications(loggedInTeam, clars, clReq, Seq[(String, String)](("", "Выберите задачу")) ++ Problems.toSelect(probs), form)
        }
      }
    }

  def index = AsyncStack(AuthorityKey -> Permissions.any) { implicit request =>
    implicit val ec = StackActionExecutionContext
    clrForm(loggedIn, clarificationReqForm).map { v =>
      statusActorModel.statusActor ! StatusActor.AckAllClarifications(loggedIn.contest.id, loggedIn.team.localId)
      Ok(v)
    }
  }

  def post = AsyncStack(AuthorityKey -> UserPermissions.any) { implicit request =>
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
          for (clrId <- clrIds) {
            statusActorModel.statusActor ! ClarificationRequested(loggedInTeam.contest.id, clrId)
          }
          Redirect(routes.QandA.index)
        }
      }
    )
  }
}