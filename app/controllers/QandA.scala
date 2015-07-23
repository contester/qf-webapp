package controllers

import javax.inject.Inject

import akka.actor.ActorSystem
import jp.t2v.lab.play2.auth.AuthElement
import models.{Problems, LoggedInTeam, AuthConfigImpl, Monitor}
import org.joda.time.DateTime
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{RequestHeader, Controller}
import slick.driver.JdbcProfile
import slick.jdbc.GetResult
import views.html
import play.api.libs.concurrent.Execution.Implicits.defaultContext


import scala.concurrent.Future


case class ClarificationReqData(problem: String, text: String)

class QandA @Inject() (dbConfigProvider: DatabaseConfigProvider,
                             monitorModel: Monitor,
                             val auth: AuthWrapper,
                             val messagesApi: MessagesApi) extends Controller with AuthElement with AuthConfigImpl with I18nSupport {

  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import utils.Db._
  import dbConfig.driver.api._

  private def anyUser(account: LoggedInTeam): Future[Boolean] = Future.successful(true)

  private val clarificationReqForm = Form {
    mapping("problem" -> text, "text" -> text)(ClarificationReqData.apply)(ClarificationReqData.unapply)
  }

  private def clrForm(loggedInTeam: LoggedInTeam, form: Form[ClarificationReqData])(implicit request: RequestHeader) =
    db.run(loggedInTeam.contest.getProblems).flatMap { probs =>
      db.run(loggedInTeam.contest.getClarifications).flatMap { clars =>
        db.run(loggedInTeam.getClarificationRequests).map { clReq =>
          html.clarifications(loggedInTeam, clars, clReq, Problems.toSelect(probs), form)
        }
      }
    }

  def index = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    clrForm(loggedIn, clarificationReqForm).map(Ok(_))
  }

  def post = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn

    clarificationReqForm.bindFromRequest.fold(
      formWithErrors => clrForm(loggedIn, formWithErrors).map(BadRequest(_)),
      clrData => {
        db.run(
          sqlu"""insert into ClarificationRequests (Contest, Team, Problem, Request, Arrived) values
                (${loggedInTeam.contest.id}, ${loggedInTeam.team.localId}, ${clrData.problem}, ${clrData.text},
                CURRENT_TIMESTAMP())
              """
        ).map { _ =>
          Redirect(routes.QandA.index)
        }
      }
    )
  }

}