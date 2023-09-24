package models

import com.github.nscala_time.time.Imports._
import com.github.tminglei.slickpg.InetString
import com.google.protobuf.ByteString
import com.spingo.op_rabbit.Message
import play.api.Logging
import play.api.libs.json.JsValue
import protos.Tickets.{Computer, IdName, PrintJob, PrintJobReport}
import slick.basic.DatabaseConfig
import slick.jdbc.{JdbcBackend, JdbcProfile}

import scala.concurrent.{ExecutionContext, Future}

case class ShortenedPrintJob(id: Int, filename: String, contest: Int, team: Int, computerID: String,
                             computerName: String, areaID: Int, areaName: String, printer: String,
                             data: Array[Byte], arrived: DateTime)

case class PrintEntry(filename: String, arrived: DateTime, printed: Boolean, pages: Int)

class PrintingModel(dbConfig: DatabaseConfig[JdbcProfile], statusActorModel: StatusActorModel, rabbitMqModel: RabbitMqModel)(implicit ec: ExecutionContext) extends Logging {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel._

  private[this] val db = dbConfig.db


  def insertAndPrintJob(contest:Int, team:Int, filename: String, data: Array[Byte], computerAsString: String) = {
    db.run((dbPrintJobs.map(x => (x.contest, x.team, x.filename, x.data, x.computer)) returning(dbPrintJobs.map(_.id))) += (
      contest, team, filename, data, InetString(computerAsString)
    )).flatMap { id =>
      printJobByID(id)
    }
  }

  private implicit val standardTimeout: akka.util.Timeout = {
    import scala.concurrent.duration._
    Duration(5, SECONDS)
  }

  private[this] def buildProtoFromID(id: Long) = {
    val q = for {
      pj <- dbPrintJobs.filter(_.id === id)
      cl <- compLocations if cl.id === pj.computer
      area <- areas if area.id === cl.location
    } yield (pj.id, pj.filename, pj.contest, pj.team, pj.computer, cl.name, area.id, area.name, area.printer, pj.data, pj.arrived)
    db.run(q.result.headOption).flatMap( pjOpt =>
      pjOpt.map { x =>
        val pj = ShortenedPrintJob(x._1.toInt, x._2, x._3, x._4, x._5.address, x._6, x._7, x._8, x._9, x._10, x._11)
        statusActorModel.teamClient.getTeam(pj.contest, pj.team).zip(statusActorModel.getContest(pj.contest)).map {
          case (team, contestOpt) =>
            Some(PrintJob(
              filename=pj.filename,
              contest=Some(IdName(pj.contest, contestOpt.get.name)),
              team=Some(IdName(pj.team, team.get.teamFullName)),
              computer=Some(Computer(id=pj.computerID, name=pj.computerName)),
              area=Some(IdName(id=pj.areaID, name=pj.areaName)),
              data = ByteString.copyFrom(pj.data),
              timestampSeconds = pj.arrived.getMillis / 1000,
              printer=pj.printer,
              jobId = s"s-$id"
            ))
        }
      }.getOrElse(Future.successful(None))
    )
  }

  import utils.ProtoRabbitSupport._

  def printJobByID(id: Long) =
    buildProtoFromID(id).map{ optJob =>
      optJob.map { job =>
        rabbitMqModel.rabbitMq ! Message.queue(job, queue = "contester.protoprintjobs")
      }
    }

  def processPrintJobReport(r: PrintJobReport): Future[Int] = {
    if (r.jobExpandedId.startsWith("s-")) {
      val jobId = r.jobExpandedId.substring(2).toLong
      val procTime = new DateTime(r.timestampSeconds * 1000)
      db.run(dbPrintJobs.filter(_.id === jobId).map(x => (x.printed, x.error, x.pages)).update((Some(procTime), r.errorMessage, r.numPages.toInt)))
    } else Future.successful(0)
  }

  def printJobsForTeam(contest:Int, team: Int) =
    db.run(dbPrintJobs.filter(x => ((x.contest === contest) && (x.team === team)))
      .map(x => (x.filename, x.arrived, x.printed.isDefined, x.pages)).sortBy(_._2.desc).result).map(_.map(PrintEntry.tupled))
}

object ClarificationModel extends Logging {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel._

  private[this] val clarificationsByContest = Compiled((contestID: Rep[Int]) =>
    clarificationRequests.filter(_.contest === contestID).sortBy(_.arrived.desc)
  )

  val getClarificationReq = Compiled((clarificationRequestID: Rep[Long]) =>
    clarificationRequests.filter(_.id === clarificationRequestID)
  )

  val getClarificationReqShort = Compiled((id: Rep[Long]) =>
    clarificationRequests.filter(_.id === id).map(x => (x.answer, x.answered))
  )

  def getClarificationReqs(db: JdbcBackend#DatabaseDef, contestId: Int)(implicit ec: ExecutionContext) =
    db.run(clarificationsByContest(contestId).result)

  private[this] val clarificationsByContestTeam = Compiled((contestID: Rep[Int], teamID: Rep[Int]) =>
    clarificationRequests.filter(x => x.contest === contestID && x.team === teamID).sortBy(_.arrived.desc)
  )

  def getTeamClarificationReqs(db: JdbcBackend#DatabaseDef, contestId: Int, teamId: Int)(implicit ec: ExecutionContext) =
    db.run(clarificationsByContestTeam(contestId, teamId).result)
}