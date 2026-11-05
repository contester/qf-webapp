package models

import com.typesafe.config.ConfigFactory
import org.scalatest.funsuite.AnyFunSuite
import protos.Tickets.{PrinterList, RemotePrinter}
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

class AgentPrinterListSpec extends AnyFunSuite {
  private def list(agent: String, ts: Long, uncs: String*) =
    PrinterList(agent, ts, uncs.map(u => RemotePrinter(u, "srv", u.split('\\').last)))

  test("rows: one per printer name, in name order, stamped with the report time") {
    val rows = AgentPrinterList.rows(list("a", 100, """\\s\b""", """\\s\a""", """\\s\b""", ""))
    assert(rows.map(_.unc) == Seq("""\\s\a""", """\\s\b"""))
    assert(rows.forall(r => r.agent == "a" && r.seen.getMillis == 100000L))
  }
}

/**
  * Writes to the database, but only inside a transaction that is rolled back. Skipped unless AGENT_PRINTERS_TEST_URL
  * is set (the agent_printers table must exist; user and password in AGENT_PRINTERS_TEST_USER/_PASSWORD).
  */
class AgentPrinterListDbSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val url = sys.env.get("AGENT_PRINTERS_TEST_URL")

  private lazy val dbConfig = DatabaseConfig.forConfig[JdbcProfile]("", ConfigFactory.parseString(
    s"""profile = "org.stingray.contester.dbmodel.MyPostgresProfile$$"
       |db { url = "${url.get}", user = "${sys.env.getOrElse("AGENT_PRINTERS_TEST_USER", "")}",
       |      password = "${sys.env.getOrElse("AGENT_PRINTERS_TEST_PASSWORD", "")}", driver = "org.postgresql.Driver",
       |      connectionPool = disabled }""".stripMargin))

  private class Rollback extends Exception

  test("a report replaces the agent's printers and leaves other agents alone") {
    assume(url.isDefined)
    import org.stingray.contester.dbmodel.MyPostgresProfile.api._
    import org.stingray.contester.dbmodel.SlickModel.agentPrinters
    def l(agent: String, ts: Long, uncs: String*) =
      PrinterList(agent, ts, uncs.map(u => RemotePrinter(u, "srv", "x")))
    val test = for {
      _ <- AgentPrinterList.replaceAction(l("t1", 100, """\\s\a""", """\\s\b"""))
      _ <- AgentPrinterList.replaceAction(l("t2", 200, """\\s\b""", """\\s\c"""))
      _ <- AgentPrinterList.replaceAction(l("t1", 300, """\\s\a"""))
      t1 <- agentPrinters.filter(_.agent === "t1").map(_.unc).result
      t2 <- agentPrinters.filter(_.agent === "t2").map(_.unc).result
      _ = assert(t1 == Seq("""\\s\a""") && t2.sorted == Seq("""\\s\b""", """\\s\c"""))
      _ <- DBIO.failed(new Rollback)
    } yield ()
    val r = Await.result(dbConfig.db.run(test.transactionally).failed, 60.seconds)
    assert(r.isInstanceOf[Rollback], r)
  }
}
