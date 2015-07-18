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
import slick.jdbc.{GetResult, PositionedParameters, SetParameter}
import views.html
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future
import org.joda.time.DateTime

package printing {


case class SubmitData(textOnly: Boolean)
  case class PrintEntry(filename: String, arrived: DateTime, printed: Boolean)
}

class Printing @Inject() (override val dbConfigProvider: DatabaseConfigProvider,
                             system: ActorSystem, val messagesApi: MessagesApi) extends Controller with AuthElement
      with AuthConfigImpl with I18nSupport {
  val dbConfig = dbConfigProvider.get[JdbcProfile]
  import dbConfig.driver.api._

  private def anyUser(account: LoggedInTeam): Future[Boolean] = Future.successful(true)

  private val printForm = Form {
    mapping("textOnly" -> boolean)(printing.SubmitData.apply)(printing.SubmitData.unapply)
  }

  private implicit val getPrintEntry = GetResult(
    r => printing.PrintEntry(r.nextString(), new DateTime(r.nextTimestamp()), r.nextBoolean())
  )

  private def getPrintEntryQuery(contest: Int, team: Int) =
    sql"""select Filename, Arrived, Printed = 255 from PrintJobs
         where Contest = $contest and Team = $team order by Arrived desc""".as[printing.PrintEntry]

  def index = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn

    db.db.run(getPrintEntryQuery(loggedInTeam.contest.id, loggedInTeam.team.localId)).map { printJobs =>
      Ok(html.printform(loggedInTeam, printForm, printJobs))
    }
  }


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
      formWithErrors => {
        db.db.run(getPrintEntryQuery(loggedInTeam.contest.id, loggedInTeam.team.localId)).map { printJobs =>
          BadRequest(html.printform(loggedInTeam, formWithErrors, printJobs))
        }
      },
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
