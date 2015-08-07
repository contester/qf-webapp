package controllers

import java.lang.Compiler
import javax.inject.{Inject, Singleton}

import actors.StatusActor
import akka.actor.{Props, ActorSystem}
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import com.spingo.op_rabbit.RabbitControl
import com.spingo.op_rabbit.consumer.Subscription
import jp.t2v.lab.play2.auth.AuthElement
import models.Submits.{ACMScoreAndStatus, SchoolScoreAndStatus, StatusSubmit}
import models._
import org.apache.commons.io.FileUtils
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.iteratee.Iteratee
import play.api.libs.json.JsValue
import play.api.mvc.BodyParsers.parse
import play.api.mvc.{Action, Controller}
import slick.driver.JdbcProfile
import views.html

import scala.concurrent.Future
import scala.text

@Singleton
class AdminApplication @Inject() (dbConfigProvider: DatabaseConfigProvider,
                             monitorModel: Monitor,
                             system: ActorSystem,
                             val auth: AuthWrapper,
                             val messagesApi: MessagesApi) extends Controller with AuthElement with AdminAuthConfigImpl with I18nSupport{

  private val dbConfig = dbConfigProvider.get[JdbcProfile]
  private val db = dbConfig.db
  import dbConfig.driver.api._
  import utils.Db._

  val rabbitMq = system.actorOf(Props[RabbitControl])
  def monitor(id: Int) = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    monitorModel.getMonitor(id, true).map(x => Ok(html.monitor(x.get.contest, x.get.status)))
  }

  private def anyUser(account: Admin): Future[Boolean] = Future.successful(true)

  private def annot8(submits: Seq[Submit], schoolMode: Boolean) =
    if (schoolMode)
      Submits.annotateSchoolSubmits(db, submits)
    else
      Submits.annotateACMSubmits(db, submits)

  def index = AsyncStack(AuthorityKey -> anyUser) { implicit request =>

    db.run(Submits.getContestSubmits(1)).flatMap(x => annot8(x.take(20), false)).map { subs =>
      Ok(html.adminsubmits(subs, false))
    }
  }

  def submits(contestId: Int) = AsyncStack(AuthorityKey -> anyUser) { implicit request =>
    db.run(Submits.getContestSubmits(contestId)).flatMap(x => annot8(x, false)).map { subs =>
      Ok(html.adminsubmits(subs, false))
    }
  }
}