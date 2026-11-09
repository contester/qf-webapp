package controllers

import com.mohiva.play.silhouette.api.Silhouette
import models._
import org.apache.commons.io.FileUtils
import play.api.Logging
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{AbstractController, ControllerComponents, RequestHeader}
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import utils.auth.TeamsEnv
import views.html

import scala.concurrent.{ExecutionContext, Future}

package printing {
case class SubmitData(textOnly: Boolean)
}

class Printing (cc: ControllerComponents,
                silhouette: Silhouette[TeamsEnv],
                configuration: play.api.Configuration,
                dbConfig: DatabaseConfig[JdbcProfile],
                printingModel: PrintingModel) extends AbstractController(cc) with I18nSupport with Logging {
  private val db = dbConfig.db
  private val printLimits = utils.PrintableCheck.limitsFrom(configuration)

  implicit val ec = defaultExecutionContext

  private val printForm = Form {
    mapping("textOnly" -> boolean)(printing.SubmitData.apply)(printing.SubmitData.unapply)
  }

  private def getPrintForm(loggedIn: LoggedInTeam, form: Form[printing.SubmitData], location: Option[Location])(implicit request: RequestHeader, ec: ExecutionContext) =
    printingModel.printJobsForTeam(loggedIn.contest.id, loggedIn.team.id).map { printJobs =>
      html.printform(loggedIn,location, form, printJobs, printLimits.maxBytes)
    }

  def index = silhouette.SecuredAction.async { implicit request =>
    Locator.locate(db, request.remoteAddress).flatMap { location =>
      getPrintForm(request.identity, printForm, location).map(Ok(_))
    }
  }

  def post = silhouette.SecuredAction(parse.multipartFormData).async { implicit request =>
    val parsed = printForm.bindFromRequest
    val solutionOpt = request.body.file("file").map { solution =>
      solution.filename -> FileUtils.readFileToByteArray(solution.ref.path.toFile)
    }

    val parsed0 = solutionOpt match {
      case None => parsed.withGlobalError("can't read file")
      case Some((_, data)) => utils.PrintableCheck.check(data, printLimits) match {
        case utils.PrintableCheck.Ok => parsed
        case utils.PrintableCheck.Binary => parsed.withGlobalError(Messages("error.printNotText"))
        case utils.PrintableCheck.TooLong => parsed.withGlobalError(Messages("error.printTooLong"))
      }
    }

    Locator.locate(db, request.remoteAddress).flatMap { location =>
      if (location.isEmpty) {
        logger.debug(s"Printing from UNKNOWN LOCATION remote addr: ${request.remoteAddress}")
      }
      parsed0.fold(
        formWithErrors => getPrintForm(request.identity, formWithErrors, location).map(BadRequest(_)),
        submitData => {
          printingModel.insertAndPrintJob(request.identity.contest.id, request.identity.team.id,
            solutionOpt.get._1, solutionOpt.get._2, request.remoteAddress).flatMap {
              case true => Future.successful(Redirect(routes.Printing.index))
              case false =>
                getPrintForm(request.identity, parsed0.withGlobalError(Messages("error.printNotQueued")), location).map(InternalServerError(_))
            }
        }
      )

    }
  }

}
