package controllers

import java.nio.charset.StandardCharsets

import javax.inject.Inject
import akka.actor.{ActorSystem, Props}
import com.mohiva.play.silhouette.api.{Authorization, Silhouette}
import com.mohiva.play.silhouette.impl.authenticators.SessionAuthenticator
import com.spingo.op_rabbit.{Message, RabbitControl}
import models._
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.Json
import play.api.mvc._
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import slick.jdbc.GetResult
import utils.FormUtil
import utils.auth.{AdminEnv, TeamsEnv}
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

class ServerSideEval (cc: ControllerComponents,
                      silhouette: Silhouette[TeamsEnv],
                      dbConfig: DatabaseConfig[JdbcProfile], rabbitMqModel: RabbitMqModel
                             ) extends AbstractController(cc) with I18nSupport {
  private val db = dbConfig.db
  import utils.Db._
  import dbConfig.profile.api._
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

  private def getEvalForm(loggedInTeam: LoggedInTeam, compilers:Seq[Compiler],
                          form: Form[ServerSideData])(implicit request: RequestHeader, ec: ExecutionContext) =
    getEvals(loggedInTeam.contest.id, loggedInTeam.team.localId).map { evals =>
      html.sendwithinput(loggedInTeam, form, Compilers.forForm(compilers), evals)
    }

  case class canSeeEval(id: Int)(implicit ec: ExecutionContext) extends Authorization[LoggedInTeam, SessionAuthenticator] {
    override def isAuthorized[B](identity: LoggedInTeam, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] =
      getEvalById(id).map { cids =>
        cids.exists(cid => identity.matching(ContestTeamIds(cid.contest, cid.team)))
      }
  }


  import scala.concurrent.ExecutionContext.Implicits.global


  def index = silhouette.SecuredAction.async { implicit request =>
    db.run(request.identity.contest.getCompilers.result).flatMap { compilers =>
      getEvalForm(request.identity, compilers, serverSideForm).map(Ok(_))
    }
  }

  def details(id: Int) = silhouette.SecuredAction(canSeeEval(id)).async { implicit request =>
    getEvalById(id).map { opt =>
      opt.map { ev =>
        Ok(html.evaldetails(request.identity, ev))
      }.getOrElse(Redirect(routes.ServerSideEval.index))
    }
  }

  def post = silhouette.SecuredAction(parse.multipartFormData).async { implicit request =>
    db.run(request.identity.contest.getCompilers.result).flatMap { compilers =>
        val parsed = serverSideForm.bindFromRequest

        parsed.fold(
          formWithErrors => getEvalForm(request.identity, compilers, formWithErrors).map(BadRequest(_)),
          submitData => {
            val cext = compilers.map(x => x.id -> x).toMap.apply(submitData.compiler).ext
            val solutionBytes = FormUtil.inlineOrFile(submitData.inlinesolution, request.body.file("file")).getOrElse(FormUtil.emptyBytes)
            val dataBytes = FormUtil.inlineOrFile(submitData.inlinedata, request.body.file("inputfile")).getOrElse(FormUtil.emptyBytes)
            db.run(
              (sqlu"""insert into Eval (Contest, Team, Ext, Source, Input, Arrived) values
                    (${request.identity.contest.id}, ${request.identity.team.localId}, ${cext}, ${solutionBytes},
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