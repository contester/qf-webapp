package models

import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.ExecutionContext

/**
  * The admin custom tests list must agree with the full rows and page through all of them, on a fixture round of its
  * own with eight custom tests. Skipped unless QF_TEST_URL is set (see SubmitInsertDbSpec).
  */
class AdminCustomTestsDbSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val fx = new ContestFixture("Admin custom tests spec")

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import fx.await

  test("list rows match the full rows, in every page size") {
    assume(fx.url.isDefined)
    fx.withFixture {
    val c = fx.round
    val ids = (1 to 8).map(_ => fx.insertCustomTest())
    fx.exec(sqlu"update custom_tests set tested = true, finished_at = now(), output = 'output'::bytea, result_code = 10 where id in (${ids(0)}, ${ids(3)})")
    val all = await(AdminCustomTests.page(fx.db, c, None, None, None, size = 100000))
    assert(all.rows.map(_.id) == all.rows.map(_.id).sorted.reverse)
    all.rows.take(20).foreach { row =>
      val full = await(AdminCustomTests.get(fx.db, c, row.id)).get
      assert(row.sourceSize == full.source.length && row.inputSize == full.input.length && row.outputSize == full.output.length)
      assert(row.resultCode == full.resultCode && row.team == full.team && row.language == full.languageName)
      assert(row.running == full.finishTime.isEmpty)
    }
    assert(await(AdminCustomTests.get(fx.db, c + 100000, all.rows.head.id)).isEmpty, "other contest's test must not leak")

    var seen = Seq[Long]()
    var page = await(AdminCustomTests.page(fx.db, c, None, None, None, size = 3))
    seen ++= page.rows.map(_.id)
    var guard = 0
    while (page.older.isDefined && guard < 1000) {
      guard += 1
      page = await(AdminCustomTests.page(fx.db, c, None, page.older, None, size = 3))
      seen ++= page.rows.map(_.id)
    }
    assert(seen == all.rows.map(_.id))
    // and back up again through `after`
    val p = await(AdminCustomTests.page(fx.db, c, None, None, Some(all.rows.last.id - 1), size = 3))
    assert(p.rows.map(_.id) == all.rows.takeRight(3).map(_.id))
    assert(all.rows.map(_.id) == ids.reverse)
    assert(all.rows.count(!_.running) == 2)
    }
  }
}
