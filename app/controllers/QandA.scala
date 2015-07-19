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

case class Clarification(time: DateTime, problem: String, text: String)
case class ClarificationRequest(time: DateTime, problem: String, text: String, answer: String)

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

  implicit private val getClarification = GetResult(r => Clarification(new DateTime(r.nextTimestamp()), r.nextString().toUpperCase, r.nextString()))

  implicit private val getClarificationRequest = GetResult(
    r => ClarificationRequest(new DateTime(r.nextTimestamp()), r.nextString(), r.nextString(), r.nextString())
  )

  private def getClarificationsQuery(contestId: Int) =
    sql"""select cl_date, cl_task, cl_text from clarifications where cl_is_hidden = '0' and cl_contest_idf = $contestId""".as[Clarification]

  private def getClarifications(contestId: Int) =
    db.run(getClarificationsQuery(contestId))

  private def getClarificationRequests(contestId: Int, teamId: Int) =
    db.run(
      sql"""select Arrived, Problem, Request, Answer from ClarificationRequests
           where Contest = $contestId and Team = $teamId""".as[ClarificationRequest]
    )


  private val clarificationReqForm = Form {
    mapping("problem" -> text, "text" -> text)(ClarificationReqData.apply)(ClarificationReqData.unapply)
  }

  private def clrForm(loggedInTeam: LoggedInTeam, form: Form[ClarificationReqData])(implicit request: RequestHeader) =
    db.run(loggedInTeam.contest.getProblems).flatMap { probs =>
      getClarifications(loggedInTeam.contest.id).flatMap { clars =>
        getClarificationRequests(loggedInTeam.contest.id, loggedInTeam.team.localId).map { clReq =>
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