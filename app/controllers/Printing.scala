package controllers

import com.google.protobuf.ByteString
import com.mohiva.play.silhouette.api.Silhouette
import com.spingo.op_rabbit.Message
import models._
import org.apache.commons.io.FileUtils
import org.joda.time.DateTime
import play.api.Logging
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.mvc.{AbstractController, ControllerComponents, RequestHeader}
import protos.Tickets.{Computer, IdName, PrintJob}
import slick.basic.DatabaseConfig
import slick.jdbc.{GetResult, JdbcProfile}
import utils.auth.TeamsEnv
import views.html

import scala.concurrent.{ExecutionContext, Future}

package printing {

import play.api.libs.json.Json

case class SubmitData(textOnly: Boolean)
   case class PrintEntry(filename: String, arrived: DateTime, printed: Boolean)

  object PrintEntry {
    implicit val getResult = GetResult(
      r => PrintEntry(r.nextString(), new DateTime(r.nextTimestamp()), r.nextBoolean())
    )
  }
}

class Printing (cc: ControllerComponents,
                silhouette: Silhouette[TeamsEnv],
                dbConfig: DatabaseConfig[JdbcProfile],
                printingModel: PrintingModel) extends AbstractController(cc) with I18nSupport with Logging {
  private val db = dbConfig.db
  import dbConfig.profile.api._

  implicit val ec = defaultExecutionContext

  private val printForm = Form {
    mapping("textOnly" -> boolean)(printing.SubmitData.apply)(printing.SubmitData.unapply)
  }

  private def getPrintForm(loggedIn: LoggedInTeam, form: Form[printing.SubmitData], location: Option[Location])(implicit request: RequestHeader, ec: ExecutionContext) =
    db.run(sql"""select Filename, Arrived, Printed = 255 from PrintJobs
         where Contest = ${loggedIn.contest.id} and Team = ${loggedIn.team.localId} order by Arrived desc""".as[printing.PrintEntry]
      ).map { printJobs =>
      html.printform(loggedIn,location, form, printJobs)
    }

  def index = silhouette.SecuredAction.async { implicit request =>
    Locator.locate(db, request.remoteAddress).flatMap { location =>
      getPrintForm(request.identity, printForm, location).map(Ok(_))
    }
  }

  private val russianLetters = "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯ"
  private val russianSet = (russianLetters + russianLetters.toLowerCase).toSet
  private def countRussianLetters(x: String) = x.count(russianSet)

  private def toQ(x: Byte): Byte =
    if (x > 127) {
      "?".toByte
    } else x

  private def fixEncoding(x: Array[Byte]): Array[Byte] = {
    x.map(toQ)
  }

  def post = silhouette.SecuredAction(parse.multipartFormData).async { implicit request =>
    val parsed = printForm.bindFromRequest
    val solutionOpt = request.body.file("file").map { solution =>
      solution.filename -> FileUtils.readFileToByteArray(solution.ref.path.toFile)
    }.map(x => x._1 -> fixEncoding(x._2))

    val parsed0 = if (solutionOpt.isDefined) parsed
      else parsed.withGlobalError("can't read file")

    Locator.locate(db, request.remoteAddress).flatMap { location =>
      if (location.isEmpty) {
        logger.debug(s"Printing from UNKNOWN LOCATION remote addr: ${request.remoteAddress}")
      }
      parsed0.fold(
        formWithErrors => getPrintForm(request.identity, formWithErrors, location).map(BadRequest(_)),
        submitData => {
          printingModel.insertPrintJob(request.identity.contest.id, request.identity.team.localId,
            solutionOpt.get._1, solutionOpt.get._2, request.remoteAddress).flatMap { printJobIds =>
            printJobIds.map { jobId=>
              printingModel.printJobByID(jobId)
            }
            Future.successful(Redirect(routes.Printing.index))
          }
        }
      )

    }
  }

}
