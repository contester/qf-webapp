package controllers

import java.nio.charset.StandardCharsets
import javax.inject.Inject

import akka.actor.{ActorSystem, Props}
import com.spingo.op_rabbit.{Message, RabbitControl}
import jp.t2v.lab.play2.auth.AuthElement
import models._
import org.apache.commons.io.FileUtils
import org.joda.time.DateTime
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Controller, RequestHeader}
import slick.driver.JdbcProfile
import slick.jdbc.GetResult
import views.html

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

package printing {

import play.api.libs.json.Json

case class SubmitData(textOnly: Boolean)
   case class PrintEntry(filename: String, arrived: DateTime, printed: Boolean)

  object PrintEntry {
    implicit val getResult = GetResult(
      r => PrintEntry(r.nextString(), new DateTime(r.nextTimestamp()), r.nextBoolean())
    )
  }

  case class PrintJobID(id: Int)

  object PrintJobID {
    implicit val format = Json.format[PrintJobID]
  }
}

class Printing @Inject() (val dbConfigProvider: DatabaseConfigProvider,
                         val auth: AuthWrapper, rabbitMqModel: RabbitMqModel,
                          val messagesApi: MessagesApi) extends Controller with AuthElement
      with AuthConfigImpl with I18nSupport {
  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import dbConfig.driver.api._
  import utils.Db._

  private val rabbitMq = rabbitMqModel.rabbitMq
  import com.spingo.op_rabbit.PlayJsonSupport._

  private val printForm = Form {
    mapping("textOnly" -> boolean)(printing.SubmitData.apply)(printing.SubmitData.unapply)
  }

  private def getPrintForm(loggedIn: LoggedInTeam, form: Form[printing.SubmitData], location: Option[Location])(implicit request: RequestHeader, ec: ExecutionContext) =
    db.run(sql"""select Filename, Arrived, Printed = 255 from PrintJobs
         where Contest = ${loggedIn.contest.id} and Team = ${loggedIn.team.localId} order by Arrived desc""".as[printing.PrintEntry]
      ).map { printJobs =>
      html.printform(loggedIn,location, form, printJobs)
    }

  def index = AsyncStack(AuthorityKey -> UserPermissions.any) { implicit request =>
    implicit val ec = StackActionExecutionContext
    Locator.locate(db, request.remoteAddress).flatMap { location =>
      getPrintForm(loggedIn, printForm, location).map(Ok(_))
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

  def post = AsyncStack(parse.multipartFormData, AuthorityKey -> UserPermissions.any) { implicit request =>
    val loggedInTeam = loggedIn
    implicit val ec = StackActionExecutionContext

    val parsed = printForm.bindFromRequest
    val solutionOpt = request.body.file("file").map { solution =>
      solution.filename -> FileUtils.readFileToByteArray(solution.ref.file)
    }.map(x => x._1 -> fixEncoding(x._2))

    val parsed0 = if (solutionOpt.isDefined) parsed
      else parsed.withGlobalError("can't read file")

    Logger.info(s"Remote addr: ${request.remoteAddress}")
    Locator.locate(db, request.remoteAddress).flatMap { location =>
      parsed0.fold(
        formWithErrors => getPrintForm(loggedIn, formWithErrors, location).map(BadRequest(_)),
        submitData => {
          db.run(
            (sqlu"""insert into PrintJobs (Contest, Team, Filename, DATA, Computer, Arrived) values
                    (${loggedInTeam.contest.id}, ${loggedInTeam.team.localId}, ${solutionOpt.get._1},
            ${solutionOpt.get._2}, INET_ATON(${request.remoteAddress}), CURRENT_TIMESTAMP())
                  """.andThen(sql"select last_insert_id()".as[Int])).withPinnedSession
          ).map { printJobIds =>
            printJobIds.foreach { printJobId =>
              rabbitMq ! Message.queue(printing.PrintJobID(printJobId), queue = "contester.printrequests")
            }
            Redirect(routes.Printing.index)
          }
        }
      )

    }
  }

}
