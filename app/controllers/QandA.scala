package controllers

import actors.StatusActor
import actors.StatusActor.ClarificationRequested
import com.mohiva.play.silhouette.api.Silhouette
import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.mvc.{AbstractController, ControllerComponents, RequestHeader}
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import utils.auth.TeamsEnv
import views.html

import scala.concurrent.ExecutionContext

case class ClarificationReqData(problem: String, text: String)

class QandA (cc: ControllerComponents,
             silhouette: Silhouette[TeamsEnv],
             dbConfig: DatabaseConfig[JdbcProfile],
                       statusActorModel: StatusActorModel) extends AbstractController(cc) with I18nSupport {
  private val db = dbConfig.db
  import dbConfig.profile.api._

  private val clarificationReqForm = Form {
    mapping("problem" -> text, "text" -> text)(ClarificationReqData.apply)(ClarificationReqData.unapply)
  }

  private implicit val standardTimeout: akka.util.Timeout = {
    import scala.concurrent.duration._
    Duration(5, SECONDS)
  }

  import utils.Ask

  private def clrForm(loggedInTeam: LoggedInTeam, form: Form[ClarificationReqData])(implicit request: RequestHeader, ec: ExecutionContext) =
    statusActorModel.problemClient.getProblems(loggedInTeam.contest.id).flatMap { probs =>
      Ask.apply[Seq[Clarification]](statusActorModel.statusActor, StatusActor.GetVisibleClarifications(loggedInTeam.contest.id)).flatMap { clars =>
        ClarificationModel.getTeamClarificationReqs(db, loggedInTeam.contest.id, loggedInTeam.team.localId).map { clReq =>
          html.clarifications(loggedInTeam, clars, clReq, Seq[(String, String)](("", "Выберите задачу")) ++ Problems.toSelect(probs), form)
        }
      }
    }

  import scala.concurrent.ExecutionContext.Implicits.global

  def index = silhouette.SecuredAction.async { implicit request =>
    clrForm(request.identity, clarificationReqForm).map { v =>
      statusActorModel.statusActor ! StatusActor.AckAllClarifications(request.identity.contest.id, request.identity.team.localId)
      Ok(v)
    }
  }

  def post = silhouette.SecuredAction.async { implicit request =>
    clarificationReqForm.bindFromRequest.fold(
      formWithErrors => clrForm(request.identity, formWithErrors).map(BadRequest(_)),
      clrData => {
        db.run(
          (sqlu"""insert into ClarificationRequests (Contest, Team, Problem, Request, Arrived) values
                (${request.identity.contest.id}, ${request.identity.team.localId}, ${clrData.problem}, ${clrData.text},
                CURRENT_TIMESTAMP())
              """.andThen(sql"select last_insert_id()".as[Int])).withPinnedSession
        ).map { clrIds =>
          for (clrId <- clrIds) {
            statusActorModel.statusActor ! ClarificationRequested(request.identity.contest.id, clrId)
          }
          Redirect(routes.QandA.index)
        }
      }
    )
  }
}