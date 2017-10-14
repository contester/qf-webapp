package controllers

import java.nio.charset.StandardCharsets
import javax.inject.Inject

import akka.actor.{ActorSystem, Props}
import com.spingo.op_rabbit.{Message, RabbitControl}
import jp.t2v.lab.play2.auth.AuthElement
import models._
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.Json
import play.api.mvc.{Controller, RequestHeader}
import slick.jdbc.JdbcProfile
import slick.jdbc.GetResult
import utils.FormUtil
import views.html

import scala.concurrent.{ExecutionContext, Future}

package serversideeval {

case class ServerSideData(compiler: Int, inlinesolution: String, inlinedata: String)
}

import com.github.nscala_time.time.Imports.DateTime

case class ServerSideEvalID(id: Int)

object ServerSideEvalID {
  implicit val format = Json.format[ServerSideEvalID]
}

class ServerSideEval @Inject() (val dbConfigProvider: DatabaseConfigProvider,
                               val auth: AuthWrapper, rabbitMqModel: RabbitMqModel,
                             val messagesApi: MessagesApi) extends Controller with AuthElement with AuthConfigImpl with I18nSupport {
  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import utils.Db._
  import dbConfig.driver.api._
  import controllers.serversideeval.ServerSideData

  val rabbitMq = rabbitMqModel.rabbitMq
  import com.spingo.op_rabbit.PlayJsonSupport._

  private val serverSideForm = Form {
    mapping("compiler" -> number, "inlinesolution" -> text, "inlinedata" -> text)(ServerSideData.apply)(ServerSideData.unapply)
  }

  private def evalQuery(contest: Int, team: Int) =
    sql"""SELECT ID, Touched, Ext, Source, Input, Output, Timex, Memory, Info, Result, Contest, Team, Processed, Arrived
    FROM Eval WHERE Team=$team
    AND Contest=$contest ORDER BY Arrived DESC""".as[EvalEntry]

  private def getEvals(contest: Int, team: Int) =
    db.run(evalQuery(contest, team))

  private def getEvalById(id: Int)(implicit ec: ExecutionContext) =
    db.run(sql"""SELECT ID, Touched, Ext, Source, Input, Output, Timex, Memory, Info, Result, Contest, Team, Processed, Arrived
    FROM Eval WHERE ID = $id ORDER BY Arrived DESC""".as[EvalEntry]).map(_.headOption)

  private def canSeeEval(id: Int)(account: LoggedInTeam): Future[Boolean] = {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext

    getEvalById(id).map { cids =>
      cids.exists(cid => account.matching(ContestTeamIds(cid.contest, cid.team)))
    }
  }


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

  def details(id: Int) = AsyncStack(AuthorityKey -> canSeeEval(id)) { implicit request =>
    val loggedInTeam = loggedIn
    implicit val ec = StackActionExecutionContext

    getEvalById(id).map { opt =>
      opt.map { ev =>
        Ok(html.evaldetails(loggedInTeam, ev))
      }.getOrElse(Redirect(routes.ServerSideEval.index))
    }
  }

  def post = AsyncStack(parse.multipartFormData, AuthorityKey -> UserPermissions.any) { implicit request =>
    val loggedInTeam = loggedIn
    implicit val ec = StackActionExecutionContext

    db.run(loggedInTeam.contest.getCompilers).flatMap { compilers =>
        val parsed = serverSideForm.bindFromRequest

        parsed.fold(
          formWithErrors => getEvalForm(loggedInTeam, compilers, formWithErrors).map(BadRequest(_)),
          submitData => {
            val cext = compilers.map(x => x.id -> x).toMap.apply(submitData.compiler).ext
            val solutionBytes = FormUtil.inlineOrFile(submitData.inlinesolution, request.body.file("file")).getOrElse(FormUtil.emptyBytes)
            val dataBytes = FormUtil.inlineOrFile(submitData.inlinedata, request.body.file("inputfile")).getOrElse(FormUtil.emptyBytes)
            db.run(
              (sqlu"""insert into Eval (Contest, Team, Ext, Source, Input, Arrived) values
                    (${loggedInTeam.contest.id}, ${loggedInTeam.team.localId}, ${cext}, ${solutionBytes},
                ${dataBytes}, CURRENT_TIMESTAMP())
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