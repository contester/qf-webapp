package org.stingray.contester.dbmodel

import org.scalatest.funsuite.AnyFunSuite
import slick.jdbc.PostgresProfile
import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Fails when a table or column the Slick model selects does not exist in the real schema.
  *
  * Needs a Postgres loaded with the current schema (e.g. director/doc/contest2026_10_schema.sql
  * plus the migrations). Skipped unless DBMODEL_TEST_URL is set:
  *   DBMODEL_TEST_URL=jdbc:postgresql://127.0.0.1:55432/contest DBMODEL_TEST_USER=postgres \
  *   DBMODEL_TEST_PASSWORD=x sbt test
  */
class SchemaDriftSpec extends AnyFunSuite {
  import MyPostgresProfile.api._

  private val url = sys.env.get("DBMODEL_TEST_URL")

  private lazy val db = Database.forURL(url.get, user = sys.env.getOrElse("DBMODEL_TEST_USER", ""),
    password = sys.env.getOrElse("DBMODEL_TEST_PASSWORD", ""), driver = "org.postgresql.Driver")

  import SlickModel._

  private val tables: Seq[(String, DBIO[_])] = Seq(
    "clarifications" -> clarifications.take(0).result,
    "clarification_requests" -> clarificationRequests.take(0).result,
    "clarifications_seen" -> clrSeen2.take(0).result,
    "languages" -> compilers.take(0).result,
    "contests" -> contests.take(0).result,
    "schools" -> schools.take(0).result,
    "teams" -> teams.take(0).result,
    "participants" -> participants.take(0).result,
    "logins" -> assignments.take(0).result,
    "extra_info" -> extraInfos.take(0).result,
    "messages" -> messages2Unseen.result,
    "submits" -> submits.take(0).result,
    "problems" -> problems.take(0).result,
    "testings" -> testings.take(0).result,
    "print_jobs" -> dbPrintJobs.take(0).result,
    "print_renders" -> dbPrintRenders.take(0).result,
    "print_attempts" -> dbPrintAttempts.take(0).result,
    "print_job_state (view)" -> dbPrintJobStates.take(0).result,
    "computer_locations" -> compLocations.take(0).result,
    "custom_test" -> customTests.take(0).result,
    "results" -> results.take(0).result,
    "areas" -> areas.take(0).result,
    "agent_printers" -> agentPrinters.take(0).result,
    "waiter_tasks" -> waiterTasks.take(0).result,
    "waiter_task_record" -> waiterTaskRecords.take(0).result,
    "admins" -> admins.take(0).result,
    "joined login query" -> joinedLoginQuery.take(0).result,
    "submits0 (monitor)" -> submits0.take(0).result,
    "located print jobs" -> locatedPrintJobs.take(0).result,
    "custom tests with language" -> customTestWithLang.take(0).result
  )

  tables.foreach { case (name, action) =>
    test(s"model matches schema: $name") {
      assume(url.isDefined, "DBMODEL_TEST_URL not set")
      Await.result(db.run(action), 30.seconds)
    }
  }
}
