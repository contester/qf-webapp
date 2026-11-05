package models

import com.github.nscala_time.time.Imports._
import com.github.tminglei.slickpg.InetString
import com.google.protobuf.ByteString
import com.spingo.op_rabbit.Message
import play.api.Logging
import play.api.libs.json.JsValue
import protos.Tickets.{BinaryJob, Computer, IdName, PrintJob, PrintJobReport, PrinterList}
import slick.basic.DatabaseConfig
import slick.jdbc.{JdbcBackend, JdbcProfile}

import scala.concurrent.{ExecutionContext, Future}

case class ShortenedPrintJob(id: Int, filename: String, contest: Int, team: Int, computerID: String,
                             computerName: String, areaID: Int, areaName: String, printer: String,
                             data: Array[Byte], arrived: DateTime)

case class PrintEntry(filename: String, arrived: DateTime, printed: Boolean, pages: Int, failed: Boolean)

class PrintingModel(dbConfig: DatabaseConfig[JdbcProfile], statusActorModel: StatusActorModel, rabbitMqModel: RabbitMqModel,
                    palevoSink: palevo.PalevoSink)(implicit ec: ExecutionContext) extends Logging {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel._

  private[this] val db = dbConfig.db


  def insertAndPrintJob(contest:Int, team:Int, filename: String, data: Array[Byte], computerAsString: String) = {
    db.run((dbPrintJobs.map(x => (x.contest, x.team, x.filename, x.data, x.computer)) returning(dbPrintJobs.map(_.id))) += (
      contest, team, filename, data, InetString(computerAsString)
    )).flatMap { id =>
      createFullAttempt(id, None).map(_.isDefined)
    }
  }

  private implicit val standardTimeout: akka.util.Timeout = {
    import scala.concurrent.duration._
    Duration(5, SECONDS)
  }

  private[this] def buildProto(id: Long, correlationId: String) = {
    val q = for {
      pj <- dbPrintJobs.filter(_.id === id)
      cl <- compLocations if cl.id === pj.computer
      area <- areas if area.id === cl.location
    } yield (pj.id, pj.filename, pj.contest, pj.team, pj.computer, cl.name, area.id, area.name, area.printer, pj.data, pj.arrived)
    db.run(q.result.headOption).flatMap( pjOpt =>
      pjOpt.map { x =>
        val pj = ShortenedPrintJob(x._1.toInt, x._2, x._3, x._4, x._5.address, x._6, x._7, x._8, x._9, x._10, x._11)
        statusActorModel.teamClient.getTeam(pj.contest, pj.team).zip(statusActorModel.getContest(pj.contest)).map {
          case (teamOpt, contestOpt) =>
            for {
              team <- teamOpt
              contest <- contestOpt
            } yield PrintJob(
              filename=pj.filename,
              contest=Some(IdName(pj.contest, contest.name)),
              team=Some(IdName(pj.team, team.teamFullName)),
              computer=Some(Computer(id=pj.computerID, name=pj.computerName)),
              area=Some(IdName(id=pj.areaID, name=pj.areaName)),
              data = ByteString.copyFrom(pj.data),
              timestampSeconds = pj.arrived.getMillis / 1000,
              printer=pj.printer,
              charset=utils.CharsetDetect.detect(pj.data),
              jobId = correlationId
            )
        }
      }.getOrElse(Future.successful(None))
    )
  }

  import utils.ProtoRabbitSupport._

  private val binaryQueue = "contester.binaryproto"

  private def printerForJob(jobId: Long): Future[Option[String]] =
    db.run((for {
      pj <- dbPrintJobs.filter(_.id === jobId)
      cl <- compLocations if cl.id === pj.computer
      area <- areas if area.id === cl.location
    } yield area.printer).result.headOption)

  private def insertAttempt(jobId: Long, kind: String, requestedBy: Option[String], printer: String,
                            renderId: Option[Long], status: String): Future[Long] =
    db.run((dbPrintAttempts returning dbPrintAttempts.map(_.id)) +=
      DBPrintAttempt(None, jobId, kind, DateTime.now, requestedBy, printer, renderId, status, None, 0, None, "", ""))

  private def failAttempt(attemptId: Long, jobId: Long, error: String): Future[Unit] = {
    announcePrintError(jobId, error)
    db.run(dbPrintAttempts.filter(_.id === attemptId).map(x => (x.status, x.finished, x.error))
      .update((PrintAttempts.Status.Failed, Some(DateTime.now), error))).map(_ => ())
  }

  /** Starts a new attempt that renders the job from its source and prints it. Returns the attempt id, or None
    * (and records a failed attempt) if the job can't be built, e.g. the computer has no known location. */
  def createFullAttempt(jobId: Long, requestedBy: Option[String]): Future[Option[Long]] =
    for {
      printer <- printerForJob(jobId)
      attemptId <- insertAttempt(jobId, "full", requestedBy, printer.getOrElse(""), None, PrintAttempts.Status.Queued)
      built <- buildProto(jobId, s"a-$attemptId")
      queued <- built match {
        case Some(job) =>
          import utils.ProtoRabbitSupport._
          rabbitMqModel.rabbitMq ! Message.queue(job, queue = "contester.protoprintjobs")
          Future.successful(Some(attemptId))
        case None =>
          logger.error(s"Can't build print job $jobId (unknown computer location, team or contest?), not queued")
          failAttempt(attemptId, jobId, "cannot queue print job: unknown computer location, team or contest").map(_ => None)
      }
    } yield queued

  /** Prints the job again from its newest stored render, skipping the render. Without a render this is a full attempt. */
  def createPrintOnlyAttempt(jobId: Long, requestedBy: Option[String]): Future[Either[String, Long]] =
    db.run(dbPrintRenders.filter(_.jobId === jobId).sortBy(_.id.desc).result.headOption).flatMap {
      case None =>
        createFullAttempt(jobId, requestedBy).map(_.toRight("can't build print job (unknown computer location, team or contest?)"))
      case Some(render) =>
        printerForJob(jobId).flatMap {
          case Some(printer) if printer.trim.nonEmpty =>
            insertAttempt(jobId, "print_only", requestedBy, printer, render.id, PrintAttempts.Status.Rendered).map { attemptId =>
              import utils.ProtoRabbitSupport._
              val bytes = ByteString.copyFrom(render.data)
              rabbitMqModel.rabbitMq ! Message.queue(BinaryJob(
                printer = printer,
                jobId = s"a-$attemptId",
                pages = render.pages,
                payload = if (render.format == "pdf") BinaryJob.Payload.Pdf(bytes) else BinaryJob.Payload.Ps(bytes)),
                queue = binaryQueue)
              Right(attemptId)
            }
          case _ => Future.successful(Left("the area of this computer has no printer"))
        }
    }

  /** Tells palevo about a print failure. Fire-and-forget: lookup failures are logged and never propagate. */
  private def announcePrintError(jobId: Long, error: String): Unit = {
    import palevo.{EventBuilders, PContest}
    val now = DateTime.now
    val f = db.run(dbPrintJobs.filter(_.id === jobId).map(x => (x.contest, x.team)).result.headOption).flatMap {
      case Some((contestId, teamId)) =>
        statusActorModel.teamClient.getTeam(contestId, teamId).zip(statusActorModel.getContest(contestId)).map {
          case (teamOpt, contestOpt) =>
            palevoSink.send(EventBuilders.printError(jobId,
              contestOpt.map(EventBuilders.contestOf).getOrElse(PContest(contestId, "")),
              Some(EventBuilders.teamOrPlaceholder(teamId, teamOpt)), error, now))
        }
      case None =>
        Future.successful(palevoSink.send(EventBuilders.printError(jobId, PContest(0, ""), None, error, now)))
    }
    f.failed.foreach(e => logger.warn(s"palevo: cannot announce print error for job $jobId: $e"))
  }

  def processPrinterList(list: PrinterList): Future[Int] = AgentPrinterList.replace(db, list)

  def processPrintJobReport(r: PrintJobReport): Future[Int] = {
    import PrintAttempts._
    parseJobId(r.jobExpandedId) match {
      case None =>
        logger.warn(s"Ignoring print report with unknown job id ${r.jobExpandedId}")
        Future.successful(0)
      case Some(AttemptRef(attemptId)) => applyReport(attemptId, r)
      case Some(LegacyJobRef(jobId)) =>
        db.run(dbPrintAttempts.filter(_.jobId === jobId).map(_.id).max.result).flatMap {
          case Some(latest) => applyReport(latest, r)
          case None =>
            logger.warn(s"Ignoring print report for job $jobId: it has no attempts")
            Future.successful(0)
        }
    }
  }

  private def applyReport(attemptId: Long, r: PrintJobReport): Future[Int] = {
    import PrintAttempts._
    val procTime = new DateTime(r.timestampSeconds * 1000)
    val work = dbPrintAttempts.filter(_.id === attemptId).result.headOption.flatMap {
      case None =>
        logger.warn(s"Ignoring print report for unknown attempt $attemptId")
        DBIO.successful((0, Option.empty[(Long, String)]))
      case Some(a) => decide(AttemptState(a.status, a.renderId.isDefined), r) match {
        case Ignore =>
          logger.info(s"Ignoring print report for attempt $attemptId (${a.status}): stage ${r.stage}")
          DBIO.successful((0, None))
        case AttachRender(render, advance) =>
          val (format, bytes) = render.payload match {
            case BinaryJob.Payload.Pdf(b) => ("pdf", b.toByteArray)
            case BinaryJob.Payload.Ps(b) => ("ps", b.toByteArray)
            case BinaryJob.Payload.Empty => ("ps", Array.emptyByteArray) // decide() never lets this through
          }
          for {
            renderId <- (dbPrintRenders returning dbPrintRenders.map(_.id)) +=
              DBPrintRender(None, a.jobId, format, render.pages.toInt, bytes, DateTime.now)
            n <- dbPrintAttempts.filter(_.id === attemptId).map(x => (x.renderId, x.status))
              .update((Some(renderId), if (advance) Status.Rendered else a.status))
          } yield (n, None)
        case Finish(printed) =>
          for {
            n <- dbPrintAttempts.filter(_.id === attemptId)
              .map(x => (x.status, x.finished, x.pagesPrinted, x.spoolerJobId, x.error, x.detail))
              .update((if (printed) Status.Printed else Status.Failed, Some(procTime), r.pagesPrinted.toInt,
                Some(r.spoolerJobId.toLong).filter(_ != 0), r.errorMessage, r.detail))
            latest <- dbPrintAttempts.filter(_.jobId === a.jobId).map(_.id).max.result
            isLatest = latest.contains(attemptId)
          } yield (n, if (isLatest && r.errorMessage.nonEmpty) Some((a.jobId, r.errorMessage)) else None)
      }
    }
    db.run(work.transactionally).map { case (n, announce) =>
      announce.foreach { case (jobId, error) => announcePrintError(jobId, error) }
      n
    }
  }

  def printJobsForTeam(contest:Int, team: Int) =
    db.run((for {
      j <- dbPrintJobs if j.contest === contest && j.team === team
      s <- dbPrintJobStates if s.jobId === j.id
    } yield (j.filename, j.arrived, s.finished.isDefined, s.pages, s.error =!= "")).sortBy(_._2.desc).result).map(_.map(PrintEntry.tupled))
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