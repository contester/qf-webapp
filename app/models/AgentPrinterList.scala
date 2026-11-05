package models

import org.joda.time.DateTime
import org.stingray.contester.dbmodel.SlickModel.AgentPrinter
import protos.Tickets.PrinterList
import slick.jdbc.JdbcBackend

import scala.concurrent.{ExecutionContext, Future}

/** The network printers that print agents report they can print to. */
object AgentPrinterList {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel.agentPrinters

  /** An agent's report as rows; a printer listed twice counts once. */
  def rows(list: PrinterList): Seq[AgentPrinter] = {
    val seen = new DateTime(list.timestampSeconds * 1000)
    list.printers.filter(_.unc.nonEmpty).map(p => AgentPrinter(list.agent, p.unc, p.server, p.share, seen))
      .groupBy(_.unc).values.map(_.head).toSeq.sortBy(_.unc)
  }

  /** The agent's report replaces what is known about the agent. */
  def replaceAction(list: PrinterList): DBIO[Int] = {
    val newRows = rows(list)
    agentPrinters.filter(_.agent === list.agent).delete
      .andThen(agentPrinters ++= newRows)
      .andThen(DBIO.successful(newRows.size))
      .transactionally
  }

  def replace(db: JdbcBackend#DatabaseDef, list: PrinterList): Future[Int] =
    if (list.agent.isEmpty) Future.successful(0) else db.run(replaceAction(list))

  /** Every printer any agent reported, by name; with several agents the freshest report wins. */
  def all(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Seq[AgentPrinter]] =
    db.run(agentPrinters.sortBy(r => (r.unc, r.seen.desc)).result).map { rs =>
      rs.groupBy(_.unc).values.map(_.maxBy(_.seen.getMillis)).toSeq.sortBy(_.unc.toLowerCase)
    }
}
