package controllers

import javax.inject.Inject

import akka.actor.ActorSystem
import jp.t2v.lab.play2.auth.AuthElement
import models.{LoggedInTeam, AuthConfigImpl}
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.Controller
import slick.driver.JdbcProfile
import slick.jdbc.{PositionedParameters, SetParameter}
import views.html
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future

package printing {
  case class SubmitData(textOnly: Boolean)
}

class Printing @Inject() (override val dbConfigProvider: DatabaseConfigProvider,
                             system: ActorSystem, val messagesApi: MessagesApi) extends Controller with AuthElement
      with AuthConfigImpl with I18nSupport {

  private def anyUser(account: LoggedInTeam): Future[Boolean] = Future.successful(true)

  private val printForm = Form {
    mapping("textOnly" -> boolean)(printing.SubmitData.apply)(printing.SubmitData.unapply)
  }

  def index = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn

    Future.successful(Ok(html.printform(loggedInTeam, printForm)))
  }

  val dbConfig = dbConfigProvider.get[JdbcProfile]
  import dbConfig.driver.api._

  implicit object SetByteArray extends SetParameter[Array[Byte]] {
    override def apply(v1: Array[Byte], v2: PositionedParameters): Unit = v2.setBytes(v1)
  }

  def post = AsyncStack(parse.multipartFormData, AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn

    val parsed = printForm.bindFromRequest
    val solutionOpt = request.body.file("file").map { solution =>
      solution.filename -> FileUtils.readFileToByteArray(solution.ref.file)
    }

    val parsed0 = if (solutionOpt.isDefined) parsed
      else parsed.withGlobalError("can't read file")

    parsed0.fold(
      formWithErrors =>
        Future.successful(BadRequest(html.printform(loggedInTeam, formWithErrors))),
      submitData => {
        db.db.run(
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
