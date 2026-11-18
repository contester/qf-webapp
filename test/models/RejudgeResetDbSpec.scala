package models

import org.postgresql.PGConnection
import org.scalatest.funsuite.AnyFunSuite

import java.sql.{Connection, DriverManager}
import scala.concurrent.ExecutionContext

/**
  * The admin rejudge reset and the custom test rerun copy, against the teacher-web schema and its triggers
  * (including 00008: submit_new when submits.testing_id goes from a value to NULL). Skipped unless QF_TEST_URL is set.
  */
class RejudgeResetDbSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val fx = new ContestFixture("Rejudge spec")

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import fx.{await, exec, one}

  private def listening[T](channel: String)(body: Connection => T): T = {
    val conn = DriverManager.getConnection(fx.url.get, fx.user, fx.password)
    try {
      conn.createStatement().execute(s"LISTEN $channel")
      body(conn)
    } finally conn.close()
  }

  private def notifications(conn: Connection, waitMs: Int): Seq[String] = {
    val pg = conn.unwrap(classOf[PGConnection])
    val first = Option(pg.getNotifications(waitMs)).map(_.toSeq).getOrElse(Nil)
    val rest = Option(pg.getNotifications(300)).map(_.toSeq).getOrElse(Nil)
    (first ++ rest).map(_.getParameter)
  }

  test("reset clears the submit's testing and score in one statement, and the bridge hears submit_new") {
    assume(fx.url.isDefined)
    fx.withFixture {
      val tested = fx.insertSubmit()
      val testing = fx.insertTesting(tested)
      exec(sqlu"update testings set finished_at = now() where id = $testing")
      exec(sqlu"update submits set testing_id = $testing, tested = true, compiled = true, passed = 3, taken = 3, points = 10 where id = $tested")
      val pending = fx.insertSubmit() // never admitted: testing_id is already NULL, nothing to announce
      listening("submit_new") { conn =>
        assert(await(AdminSubmits.resetForRejudge(fx.db, Seq(tested, pending))) == 2)
        assert(notifications(conn, 2000) == Seq(tested.toString))
      }
      val row = one(sql"""select testing_id is null, tested, compiled, passed, taken, points is null from submits where id = $tested"""
        .as[(Boolean, Boolean, Boolean, Int, Int, Boolean)])
      assert(row == ((true, false, false, 0, 0, true)))
      // the old testing and its rows stay
      assert(one(sql"select count(*) from testings where id = $testing".as[Int]) == 1)
      assert(await(AdminSubmits.resetForRejudge(fx.db, Nil)) == 0)
    }
  }

  test("a custom test rerun inserts a copy with a new id, and eval_new names the copy") {
    assume(fx.url.isDefined)
    fx.withFixture {
      val orig = fx.insertCustomTest()
      exec(sqlu"update custom_tests set tested = true, finished_at = now(), output = 'out'::bytea, result_code = 1 where id = $orig")
      val copy = listening("eval_new") { conn =>
        val copy = await(AdminCustomTests.rerun(fx.db, fx.round, orig)).getOrElse(fail("no copy"))
        assert(notifications(conn, 2000) == Seq(copy.toString))
        copy
      }
      assert(copy != orig)
      type Row = (Int, Int, Int, Int, String, String, Boolean, Boolean, String, Int)
      def row(id: Long): Row = one(sql"""select season_id, round_id, participant_id, language_id, convert_from(source, 'UTF8'),
                                               convert_from(input, 'UTF8'), tested, finished_at is null, convert_from(output, 'UTF8'), result_code
                                          from custom_tests where id = $id""".as[Row])
      assert(row(copy) == ((fx.season, fx.round, fx.kid, fx.language, "src", "in", false, true, "", 0)))
      assert(row(orig) == ((fx.season, fx.round, fx.kid, fx.language, "src", "in", true, false, "out", 1)))
      // only a test of this round
      assert(await(AdminCustomTests.rerun(fx.db, fx.nullRound, orig)).isEmpty)
      assert(await(AdminCustomTests.rerun(fx.db, fx.round, orig + 1000000)).isEmpty)
    }
  }
}
