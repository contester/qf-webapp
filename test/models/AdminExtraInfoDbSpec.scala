package models

import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.Extrainfo

import scala.concurrent.ExecutionContext

/**
  * Add, edit, renumber and delete of extra pages, on a fixture round of its own. Skipped unless QF_TEST_URL is set
  * (see SubmitInsertDbSpec).
  */
class AdminExtraInfoDbSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val fx = new ContestFixture("Admin extra info spec")

  import fx.await

  private val A = 900001
  private val B = 900002
  private val C = 900003

  private def mine = await(AdminExtraInfo.list(fx.db, fx.round)).filter(_.num >= A)

  test("add, edit, renumber, clash, delete") {
    assume(fx.url.isDefined)
    fx.withFixture {
    val c = fx.round
    try {
      assert(await(AdminExtraInfo.save(fx.db, c, None, Extrainfo(0, A, "one", "<p>1</p>"))) == AdminExtraInfo.Saved)
      assert(await(AdminExtraInfo.save(fx.db, c, None, Extrainfo(0, B, "two", "<p>2</p>"))) == AdminExtraInfo.Saved)
      assert(mine.map(_.num) == Seq(A, B), "listed in number order")
      assert(mine.forall(_.contest == c))

      assert(await(AdminExtraInfo.save(fx.db, c, None, Extrainfo(0, A, "dup", ""))) == AdminExtraInfo.NumberTaken)
      assert(await(AdminExtraInfo.get(fx.db, c, A)).get.heading == "one", "a clash must not overwrite")

      // edit in place, number unchanged
      assert(await(AdminExtraInfo.save(fx.db, c, Some(A), Extrainfo(0, A, "one!", "<p>1!</p>"))) == AdminExtraInfo.Saved)
      assert(await(AdminExtraInfo.get(fx.db, c, A)).get.data == "<p>1!</p>")

      // renumber A -> C, B untouched
      assert(await(AdminExtraInfo.save(fx.db, c, Some(A), Extrainfo(0, C, "one!", "<p>1!</p>"))) == AdminExtraInfo.Saved)
      assert(await(AdminExtraInfo.get(fx.db, c, A)).isEmpty)
      assert(mine.map(_.num) == Seq(B, C))
      assert(await(AdminExtraInfo.get(fx.db, c, B)).get.heading == "two")

      // renumber onto an existing page is refused and changes nothing
      assert(await(AdminExtraInfo.save(fx.db, c, Some(C), Extrainfo(0, B, "x", "x"))) == AdminExtraInfo.NumberTaken)
      assert(mine.map(x => (x.num, x.heading)) == Seq((B, "two"), (C, "one!")))

      // editing a page that is gone
      assert(await(AdminExtraInfo.save(fx.db, c, Some(A), Extrainfo(0, A, "ghost", ""))) == AdminExtraInfo.Missing)

      assert(await(AdminExtraInfo.delete(fx.db, c, B)))
      assert(!await(AdminExtraInfo.delete(fx.db, c, B)))
      assert(mine.map(_.num) == Seq(C))
    } finally {
      Seq(A, B, C).foreach(n => await(AdminExtraInfo.delete(fx.db, fx.round, n)))
    }
    assert(mine.isEmpty)
    }
  }
}
