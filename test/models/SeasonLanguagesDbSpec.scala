package models

import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.ExecutionContext

/**
  * Copying a season's languages, between two fixture seasons of its own (both non-current: the seed's current
  * season is never touched). Skipped unless QF_TEST_URL is set (see SubmitInsertDbSpec).
  */
class SeasonLanguagesDbSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val fx = new ContestFixture("Season languages spec")

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import fx.{await, exec, one}

  private val shortIds = Seq("qf-lang-spec-from", "qf-lang-spec-to")

  private def cleanup(): Unit = shortIds.foreach { s =>
    exec(sqlu"delete from season_languages where season_id in (select id from seasons where short_id = $s)")
    exec(sqlu"delete from seasons where short_id = $s")
  }

  private def season(shortId: String): Int = {
    exec(sqlu"""insert into seasons (short_id, name, reg_deadline, reg_code)
                values ($shortId, $shortId, timestamptz '2030-01-01 00:00:00+00', 'code')""")
    one(sql"select id from seasons where short_id = $shortId".as[Int])
  }

  private def language(season: Int, name: String, module: String): Unit =
    exec(sqlu"insert into season_languages (season_id, name, module_id) values ($season, $name, $module)")

  private def names(season: Int) = await(SeasonLanguages.list(fx.db, season)).map(l => (l.name, l.moduleId))

  test("copy adds the source's languages in order, skips those already there, and a second copy adds nothing") {
    assume(fx.url.isDefined)
    cleanup()
    try {
      val from = season(shortIds(0))
      val to = season(shortIds(1))
      language(from, "GNU C++", "g++")
      language(from, "Python 3", "python3")
      language(from, "Java", "java")
      language(to, "Python 3", "python3") // already there: skipped
      language(to, "GNU C++", "clang++")  // same name, other module: a different language

      assert(await(SeasonLanguages.copyLanguages(fx.db, from, to)) == CopyLanguagesResult.Copied(2))
      assert(names(to) == Seq(("Python 3", "python3"), ("GNU C++", "clang++"), ("GNU C++", "g++"), ("Java", "java")))
      assert(names(from) == Seq(("GNU C++", "g++"), ("Python 3", "python3"), ("Java", "java")), "the source is unchanged")

      assert(await(SeasonLanguages.copyLanguages(fx.db, from, to)) == CopyLanguagesResult.Copied(0), "idempotent")
      assert(names(to).size == 4)

      assert(await(SeasonLanguages.copyLanguages(fx.db, to, to)) == CopyLanguagesResult.SameSeason)
      assert(names(to).size == 4)

      val listed = await(SeasonLanguages.seasonsWithLanguages(fx.db))
      assert(listed.filter(s => s.id == from || s.id == to).map(s => (s.id, s.languages)).toSet == Set((from, 3), (to, 4)))
      // the current season is never one of these
      val current = await(SeasonLanguages.currentSeason(fx.db)).map(_._1)
      assert(current.isDefined && !current.contains(from) && !current.contains(to))
    } finally cleanup()
  }
}
