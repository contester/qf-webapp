package models

import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Contest, Team}

import scala.concurrent.ExecutionContext

/**
  * Submits.insertCustomTest (ServerSideEval.post): the submit checks without the problem. The fixture round runs
  * 2030-01-01 12:00..14:00 UTC. Skipped unless QF_TEST_URL is set.
  */
class CustomTestInsertDbSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val fx = new ContestFixture("Custom test insert spec")

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import fx.{await, exec, one}

  private val inside = new DateTime(2030, 1, 1, 13, 0, DateTimeZone.UTC)

  private def team(): LoggedInTeam = {
    val c = Contest(fx.round, "r", inside.minusHours(1), inside.plusHours(1), None, fx.season)
    LoggedInTeam("x", c, Team(fx.kid, 1, "P", "S", "C", None), Seq.empty)
  }

  private def insert(lang: Int, at: DateTime) = await(Submits.insertCustomTest(fx.db, team(), lang, "src".getBytes, "in".getBytes, at))

  test("accepted while the round runs; refused before the start and after the end") {
    assume(fx.url.isDefined)
    fx.withFixture {
      val id = insert(fx.language, inside)
      assert(id.isRight)
      assert(one(sql"select round_id, participant_id, season_id from custom_tests where id = ${id.toOption.get}".as[(Int, Int, Int)]) ==
        ((fx.round, fx.kid, fx.season)))
      assert(insert(fx.language, inside.minusHours(2)) == Left(RoundNotRunning))
      assert(insert(fx.language, inside.plusHours(1)) == Left(RoundNotRunning))
      exec(sqlu"update contest_rounds set is_finished = true where id = ${fx.round}")
      assert(insert(fx.language, inside) == Left(RoundNotRunning))
    }
  }

  test("a language of another season is UnknownLanguage, not an exception; an inactive credential is NotAdmitted") {
    assume(fx.url.isDefined)
    fx.withFixture {
      exec(sqlu"""insert into seasons (short_id, name, is_current, reg_deadline, reg_code) values ('qf-ct-old', 'Old', false, now(), 'x')""")
      try {
        exec(sqlu"""insert into season_languages (season_id, name, module_id) select id, 'ct-foreign', 'f' from seasons where short_id = 'qf-ct-old'""")
        val foreign = one(sql"select id from season_languages where name = 'ct-foreign'".as[Int])
        assert(insert(foreign, inside) == Left(UnknownLanguage))
        exec(sqlu"update round_credentials set is_active = false where round_id = ${fx.round} and participant_id = ${fx.kid}")
        assert(insert(fx.language, inside) == Left(NotAdmitted))
      } finally {
        exec(sqlu"delete from season_languages where name = 'ct-foreign'")
        exec(sqlu"delete from seasons where short_id = 'qf-ct-old'")
      }
    }
  }
}
