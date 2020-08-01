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
  private[this] val db = dbConfig.db
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel._
  import controllers.serversideeval.ServerSideData

  private[this] val rabbitMq = rabbitMqModel.rabbitMq
  import com.spingo.op_rabbit.PlayJsonSupport._

  private[this] val serverSideForm = Form {
    mapping("compiler" -> number, "inlinesolution" -> text, "inlinedata" -> text)(ServerSideData.apply)(ServerSideData.unapply)
  }

  private[this] def insertEval(contest:Int, team:Int, lang:Int, solution: Array[Byte], data: Array[Byte]) = {
    val q = (SlickModel.customTests.map(x => (x.contest, x.team, x.language, x.source, x.input)) returning SlickModel.customTests.map(_.id)) += (
      contest, team, lang, solution, data
    )
    db.run(q)
  }

  private[this] def getEvalForm(loggedInTeam: LoggedInTeam, compilers:Seq[Compiler],
                          form: Form[ServerSideData])(implicit request: RequestHeader, ec: ExecutionContext) =
    db.run(SlickModel.customTestWithLang.filter(x => (x.contest === loggedInTeam.contest.id && x.team === loggedInTeam.team.id)).result).map { evals =>
      html.sendwithinput(loggedInTeam, form, Compilers.forForm(compilers), evals)
    }

  private[this] case class canSeeEval(id: Long)(implicit ec: ExecutionContext) extends Authorization[LoggedInTeam, SessionAuthenticator] {
    override def isAuthorized[B](identity: LoggedInTeam, authenticator: SessionAuthenticator)(implicit request: Request[B]): Future[Boolean] = {
      db.run(SlickModel.customTests.filter(x => (x.id === id && x.contest === identity.contest.id && x.team === identity.team.id)).exists.result)
    }
  }

  private[this] implicit val ec = defaultExecutionContext

  val getCompilers = SlickModel.sortedCompilers

  def index = silhouette.SecuredAction.async { implicit request =>
    db.run(getCompilers.result).flatMap { compilers =>
      getEvalForm(request.identity, compilers, serverSideForm).map(Ok(_))
    }
  }

  def details(id: Long) = silhouette.SecuredAction(canSeeEval(id)).async { implicit request =>
    db.run(SlickModel.customTestWithLang.filter(_.id === id).result.headOption).map { opt =>
      opt.map { ev =>
        Ok(html.evaldetails(request.identity, ev))
      }.getOrElse(Redirect(routes.ServerSideEval.index))
    }
  }

  def post = silhouette.SecuredAction(parse.multipartFormData).async { implicit request =>
    db.run(getCompilers.result).flatMap { compilers =>
        val parsed = serverSideForm.bindFromRequest

        parsed.fold(
          formWithErrors => getEvalForm(request.identity, compilers, formWithErrors).map(BadRequest(_)),
          submitData => {
            val solutionBytes = FormUtil.inlineOrFile(submitData.inlinesolution, request.body.file("file")).getOrElse(FormUtil.emptyBytes)
            val dataBytes = FormUtil.inlineOrFile(submitData.inlinedata, request.body.file("inputfile")).getOrElse(FormUtil.emptyBytes)
            insertEval(request.identity.contest.id, request.identity.team.id, submitData.compiler, solutionBytes, dataBytes).map {  wid =>
              rabbitMq ! Message.queue(ServerSideEvalID(wid.toInt), queue = "contester.evalrequests")
              Redirect(routes.ServerSideEval.index)
            }
          }
        )
    }
  }
}