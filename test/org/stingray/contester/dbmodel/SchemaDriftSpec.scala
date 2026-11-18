package org.stingray.contester.dbmodel

import org.scalatest.funsuite.AnyFunSuite
import slick.jdbc.PostgresProfile
import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Fails when a table or column the Slick model selects does not exist in the real schema.
  *
  * Needs the teacher-web schema (`teacherweb migrate`, plus its doc/testdata/seed.sql for the joined queries).
  * Skipped unless QF_TEST_URL is set:
  *   QF_TEST_URL=jdbc:postgresql://127.0.0.1:55433/contester QF_TEST_USER=postgres QF_TEST_PASSWORD=x sbt test
  * The printing, waiter and qf admin tables are not in that schema and are left out (TODO(task11), TODO(task19)).
  */
class SchemaDriftSpec extends AnyFunSuite {
  import MyPostgresProfile.api._

  private val url = sys.env.get("QF_TEST_URL")

  private lazy val db = Database.forURL(url.get, user = sys.env.getOrElse("QF_TEST_USER", ""),
    password = sys.env.getOrElse("QF_TEST_PASSWORD", ""), driver = "org.postgresql.Driver")

  import SlickModel._

  private val tables: Seq[(String, DBIO[_])] = Seq(
    "seasons" -> seasons.take(0).result,
    "contests" -> twContests.take(0).result,
    "contest_rounds" -> contestRounds.take(0).result,
    "schools" -> schools.take(0).result,
    "participants" -> participants.take(0).result,
    "round_credentials" -> roundCredentials.take(0).result,
    "clarifications" -> clarifications.take(0).result,
    "clarification_requests" -> clarificationRequests.take(0).result,
    "clarifications_seen" -> clrSeen2.take(0).result,
    "season_languages" -> compilers.take(0).result,
    "extra_info" -> extraInfos.take(0).result,
    "messages" -> messages2Unseen.result,
    "submits" -> submits.take(0).result,
    "problems" -> problems.take(0).result,
    "testings" -> testings.take(0).result,
    "testing_groups" -> testingGroups.take(0).result,
    "custom_tests" -> customTests.take(0).result,
    "results" -> results.take(0).result,
    "current rounds" -> currentRounds.result,
    "teams of current rounds" -> teamsOfCurrentRounds.result,
    "login query" -> loginQuery.take(0).result,
    "submits0 (monitor)" -> submits0.take(0).result,
    "custom tests with language" -> customTestWithLang.take(0).result
  )

  tables.foreach { case (name, action) =>
    test(s"model matches schema: $name") {
      assume(url.isDefined, "QF_TEST_URL not set")
      Await.result(db.run(action), 30.seconds)
    }
  }
}
