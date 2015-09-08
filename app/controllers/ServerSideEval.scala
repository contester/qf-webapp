package controllers

import javax.inject.Inject

import akka.actor.{Props, ActorSystem}
import com.spingo.op_rabbit.{Message, RabbitControl}
import jp.t2v.lab.play2.auth.AuthElement
import models._
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.Json
import play.api.mvc.{RequestHeader, Controller}
import slick.driver.JdbcProfile
import slick.jdbc.GetResult
import views.html

import scala.concurrent.{ExecutionContext, Future}

package serversideeval {

case class ServerSideData(compiler: Int)
}

import com.github.nscala_time.time.Imports.DateTime

case class ServerSideEvalID(id: Int)

object ServerSideEvalID {
  implicit val formatServerSideEvalMessage = Json.format[ServerSideEvalID]
}

class ServerSideEval @Inject() (val dbConfigProvider: DatabaseConfigProvider,
                               val auth: AuthWrapper,
                             system: ActorSystem, val messagesApi: MessagesApi) extends Controller with AuthElement with AuthConfigImpl with I18nSupport {
  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import utils.Db._
  import dbConfig.driver.api._
  import controllers.serversideeval.ServerSideData

  val rabbitMq = system.actorOf(Props[RabbitControl])
  import com.spingo.op_rabbit.PlayJsonSupport._

  private val serverSideForm = Form {
    mapping("compiler" -> number)(ServerSideData.apply)(ServerSideData.unapply)
  }

  implicit private val getEval = GetResult(r =>
    EvalEntry(r.nextInt(), new DateTime(r.nextTimestamp()), r.nextString(), r.nextBytes(), r.nextBytes(),
      r.nextBytesOption(), r.nextInt(), r.nextLong(), r.nextLong(), r.nextInt(), r.nextStringOption())
  )

  private def evalQuery(contest: Int, team: Int) =
    sql"""SELECT e.ID, e.Arrived, e.Ext, substring(e.Source, 1, 110) as Source, substring(e.Input, 1, 110) as Input,
    substring(e.Output, 1, 110) as Output, e.Timex, e.Memory, e.Info, e.Result, r.Description as Status
    FROM Eval e LEFT JOIN ResultDesc r ON e.Result=r.ID WHERE e.Team=$team
    AND e.Contest=$contest ORDER BY Arrived DESC""".as[EvalEntry]

  private def getEvals(contest: Int, team: Int) =
    db.run(evalQuery(contest, team))

  private def getEvalForm(loggedInTeam: LoggedInTeam, compilers:Seq[Compiler],
                          form: Form[ServerSideData])(implicit request: RequestHeader, ec: ExecutionContext) =
    getEvals(loggedInTeam.contest.id, loggedInTeam.team.localId).map { evals =>
        html.sendwithinput(loggedInTeam, form, Compilers.toSelect(compilers), evals)
    }

  def index = AsyncStack(AuthorityKey -> UserPermissions.any) { implicit request =>
    val loggedInTeam = loggedIn
    implicit val ec = StackActionExecutionContext

    db.run(loggedInTeam.contest.getCompilers).flatMap { compilers =>
      getEvalForm(loggedIn, compilers, serverSideForm).map(Ok(_))
    }
  }

  def post = AsyncStack(parse.multipartFormData, AuthorityKey -> UserPermissions.any) { implicit request =>
    val loggedInTeam = loggedIn
    implicit val ec = StackActionExecutionContext

    db.run(loggedInTeam.contest.getCompilers).flatMap { compilers =>
        val parsed = serverSideForm.bindFromRequest
        val solutionOpt = request.body.file("file").map { solution =>
          FileUtils.readFileToByteArray(solution.ref.file)
        }
        val inputFile = request.body.file("inputfile").map { ifile =>
          FileUtils.readFileToByteArray(ifile.ref.file)
        }.getOrElse(new Array[Byte](0))

        val parsed0 = if (solutionOpt.isDefined) parsed
        else parsed.withGlobalError("can't open the file")

        parsed0.fold(
          formWithErrors => getEvalForm(loggedInTeam, compilers, formWithErrors).map(BadRequest(_)),
          submitData => {
            val cext = compilers.map(x => x.id -> x).toMap.apply(submitData.compiler).ext
            db.run(
              (sqlu"""insert into Eval (Contest, Team, Ext, Source, Input, Arrived) values
                    (${loggedInTeam.contest.id}, ${loggedInTeam.team.localId}, ${cext}, ${solutionOpt.get},
                ${inputFile}, CURRENT_TIMESTAMP())
                  """.andThen(sql"select last_insert_id()".as[Int])).withPinnedSession
            ).map { wat =>
              wat.foreach { wid =>
                rabbitMq ! Message.queue(ServerSideEvalID(wid), queue = "contester.evalrequests")
              }

              Redirect(routes.ServerSideEval.index)
            }
          }
        )
    }
  }


}