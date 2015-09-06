package controllers

import javax.inject.Inject

import akka.actor.ActorSystem
import jp.t2v.lab.play2.auth.AuthElement
import models.{AuthConfigImpl, LoggedInTeam}
import org.apache.commons.io.FileUtils
import org.joda.time.DateTime
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{RequestHeader, Controller}
import slick.driver.JdbcProfile
import slick.jdbc.GetResult
import views.html

import scala.concurrent.{ExecutionContext, Future}

package printing {
  case class SubmitData(textOnly: Boolean)
  case class PrintEntry(filename: String, arrived: DateTime, printed: Boolean)

  object PrintEntry {
    implicit val getResult = GetResult(
      r => PrintEntry(r.nextString(), new DateTime(r.nextTimestamp()), r.nextBoolean())
    )
  }
}

class Printing @Inject() (val dbConfigProvider: DatabaseConfigProvider,
                         val auth: AuthWrapper,
                             system: ActorSystem, val messagesApi: MessagesApi) extends Controller with AuthElement
      with AuthConfigImpl with I18nSupport {
  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import dbConfig.driver.api._
  import utils.Db._

  private def anyUser(account: LoggedInTeam): Future[Boolean] = Future.successful(true)

  private val printForm = Form {
    mapping("textOnly" -> boolean)(printing.SubmitData.apply)(printing.SubmitData.unapply)
  }

  private def getPrintForm(loggedIn: LoggedInTeam, form: Form[printing.SubmitData])(implicit request: RequestHeader, ec: ExecutionContext) =
    db.run(sql"""select Filename, Arrived, Printed = 255 from PrintJobs
         where Contest = ${loggedIn.contest.id} and Team = ${loggedIn.team.localId} order by Arrived desc""".as[printing.PrintEntry]
      ).map { printJobs =>
      html.printform(loggedIn, form, printJobs)
    }

  def index = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    implicit val ec = StackActionExecutionContext
    getPrintForm(loggedIn, printForm).map(Ok(_))
  }

  def post = AsyncStack(parse.multipartFormData, AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn
    implicit val ec = StackActionExecutionContext

    val parsed = printForm.bindFromRequest
    val solutionOpt = request.body.file("file").map { solution =>
      solution.filename -> FileUtils.readFileToByteArray(solution.ref.file)
    }

    val parsed0 = if (solutionOpt.isDefined) parsed
      else parsed.withGlobalError("can't read file")

    parsed0.fold(
      formWithErrors => getPrintForm(loggedIn, formWithErrors).map(BadRequest(_)),
      submitData => {
        db.run(
          sqlu"""insert into PrintJobs (Contest, Team, Filename, DATA, Computer, Arrived) values
                    (${loggedInTeam.contest.id}, ${loggedInTeam.team.localId}, ${solutionOpt.get._1},
            ${solutionOpt.get._2}, ${request.remoteAddress}, CURRENT_TIMESTAMP())
                  """
        ).map { rows =>
          Redirect(routes.Printing.index)
        }
      }
    )
  }

}
