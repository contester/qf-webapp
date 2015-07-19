package controllers

import javax.inject.Inject

import akka.actor.ActorSystem
import jp.t2v.lab.play2.auth.AuthElement
import models._
import org.apache.commons.io.FileUtils
import org.joda.time.DateTime
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.Controller
import slick.driver.JdbcProfile
import slick.jdbc.GetResult
import views.html
import play.api.libs.concurrent.Execution.Implicits.defaultContext


import scala.concurrent.Future

package serversideeval {

case class ServerSideData(compiler: Int)
}

class ServerSideEval @Inject() (override val dbConfigProvider: DatabaseConfigProvider,
                             system: ActorSystem, val messagesApi: MessagesApi) extends Controller with AuthElement with AuthConfigImpl with I18nSupport {
  import utils.Db._
  import controllers.serversideeval.ServerSideData

  val dbConfig = dbConfigProvider.get[JdbcProfile]
  import dbConfig.driver.api._

  private def anyUser(account: LoggedInTeam): Future[Boolean] = Future.successful(true)

  val serverSideForm = Form {
    mapping("compiler" -> number)(ServerSideData.apply)(ServerSideData.unapply)
  }

  implicit val getEval = GetResult(r =>
    EvalEntry(r.nextInt(), new DateTime(r.nextTimestamp()), r.nextString(), r.nextBytes(), r.nextBytes(),
      r.nextBytesOption(), r.nextInt(), r.nextLong(), r.nextLong(), r.nextInt(), r.nextStringOption())
  )

  def evalQuery(contest: Int, team: Int) =
    sql"""SELECT e.ID, e.Arrived, e.Ext, substring(e.Source, 1, 110) as Source, substring(e.Input, 1, 110) as Input,
    substring(e.Output, 1, 110) as Output, e.Timex, e.Memory, e.Info, e.Result, r.Description as Status
    FROM Eval e LEFT JOIN ResultDesc r ON e.Result=r.ID WHERE e.Team=$team
    AND e.Contest=$contest ORDER BY Arrived DESC""".as[EvalEntry]

  def getEvals(contest: Int, team: Int) =
    db.db.run(evalQuery(contest, team))


  def index = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn
    db.db.run(loggedInTeam.contest.getCompilers).zip(getEvals(loggedInTeam.contest.id, loggedInTeam.team.localId)).map {
      case (compilers, evals) =>
        Ok(html.sendwithinput(loggedInTeam, serverSideForm, Compilers.toSelect(compilers), evals))
    }
  }

  def post = AsyncStack(parse.multipartFormData, AuthorityKey -> anyUser) { implicit request =>
    val loggedInTeam = loggedIn

    db.db.run(loggedInTeam.contest.getCompilers).zip(getEvals(loggedInTeam.contest.id, loggedInTeam.team.localId)).flatMap {
      case (compilers, evals) =>
        val parsed = serverSideForm.bindFromRequest
        val solutionOpt = request.body.file("file").map { solution =>
          FileUtils.readFileToByteArray(solution.ref.file)
        }
        val inputFileOpt = request.body.file("inputfile").map { ifile =>
          FileUtils.readFileToByteArray(ifile.ref.file)
        }

        val parsed0 = if (solutionOpt.isDefined) parsed
        else parsed.withGlobalError("can't open the file")

        parsed0.fold(
          formWithErrors => {
            Future.successful(BadRequest(html.sendwithinput(loggedInTeam, formWithErrors, Compilers.toSelect(compilers), evals)))
          },
          submitData => {
            val cext = compilers.map(x => x.id -> x).toMap.apply(submitData.compiler).ext
            db.db.run(
              sqlu"""insert into Eval (Contest, Team, Ext, Source, Input, Arrived) values
                    (${loggedInTeam.contest.id}, ${loggedInTeam.team.localId}, ${cext}, ${solutionOpt.get},
                ${inputFileOpt.get}, CURRENT_TIMESTAMP())
                  """
            ).map { wat =>
              println(wat)
              Redirect(routes.ServerSideEval.index)
            }
          }
        )
    }
  }


}