package models

import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.ExecutionContext

/**
  * The paginated admin status page (and the row the live status page pushes) on a fixture round of its own with 30
  * submits of two participants, some rejudged. Skipped unless QF_TEST_URL is set (see SubmitInsertDbSpec).
  */
class AdminSubmitsDbSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val fx = new ContestFixture("Admin submits spec")

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import fx.{await, exec}

  /**
    * 30 submits, alternating between the fixture's participant and a second one; every third is left untested, the
    * rest are finished at revision 1 with 0-3 passed tests; submits 0 and 4 were rejudged to revision 2.
    * Returns (submit ids in order, the second participant).
    */
  private def seed(): (Seq[Long], Int) = {
    val other = fx.addParticipant(1)
    val ids = (0 until 30).map { i =>
      val id = fx.insertSubmitBy(if (i % 2 == 0) fx.kid else other)
      if (i % 3 != 2) fx.finishSubmit(id, passed = i % 4)
      id
    }
    Seq(0, 4).foreach(i => fx.finishSubmit(ids(i), passed = 3, revision = 2))
    (ids, other)
  }

  private def allPages(team: Option[Int], size: Int): Seq[AdminSubmitRow] = {
    var page = await(AdminSubmits.page(fx.db, fx.round, team, None, None, size))
    var rows = page.rows
    var guard = 0
    while (page.older.isDefined && guard < 100) {
      guard += 1
      page = await(AdminSubmits.page(fx.db, fx.round, team, page.older, None, size))
      rows ++= page.rows
    }
    rows
  }

  test("pages of any size walk every submit newest first, with the same verdicts as one big page") {
    assume(fx.url.isDefined)
    fx.withFixture {
      val (ids, _) = seed()
      val one = await(AdminSubmits.page(fx.db, fx.round, None, None, None))
      assert(one.older.isEmpty && one.newer.isEmpty)
      assert(one.rows.map(_.submit.submitId.id.toLong) == ids.reverse)
      for (size <- Seq(1, 2, 4, 7, 29, 30, 31)) {
        val walked = allPages(None, size)
        assert(walked.map(_.submit.submitId.id) == one.rows.map(_.submit.submitId.id), s"page size $size")
        assert(walked.map(_.result.message) == one.rows.map(_.result.message), s"page size $size")
      }
      // the verdicts: untested ones wait, the others show their current testing's points
      val byId = one.rows.map(r => r.submit.submitId.id.toLong -> r).toMap
      assert(!byId(ids(2)).submit.finished)
      assert(byId(ids(1)).result.message == "3.33 из 10")  // one test of three
      assert(byId(ids(3)).result.message == "10 из 10")
    }
  }

  test("newer pages mirror the older ones") {
    assume(fx.url.isDefined)
    fx.withFixture {
      seed()
      val first = await(AdminSubmits.page(fx.db, fx.round, None, None, None, 4))
      assert(first.newer.isEmpty && first.older.isDefined)
      val second = await(AdminSubmits.page(fx.db, fx.round, None, first.older, None, 4))
      assert(second.newer.isDefined && second.rows.size == 4)
      assert(second.rows.head.submit.submitId.id < first.rows.last.submit.submitId.id)
      val back = await(AdminSubmits.page(fx.db, fx.round, None, None, second.newer, 4))
      assert(back.rows.map(_.submit.submitId.id) == first.rows.map(_.submit.submitId.id))
      assert(back.newer.isEmpty && back.older.isDefined)
      // the oldest page has no older one
      val last = await(AdminSubmits.page(fx.db, fx.round, None, Some(first.rows.last.submit.submitId.id.toLong - 24), None, 4))
      assert(last.older.isEmpty)
    }
  }

  test("the team filter pages through that team's submits only") {
    assume(fx.url.isDefined)
    fx.withFixture {
      val (ids, other) = seed()
      val mine = allPages(Some(other), 3)
      assert(mine.map(_.submit.submitId.id.toLong) == ids.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).reverse)
      assert(mine.forall(_.submit.submitId.teamId == other))
      assert(await(AdminSubmits.page(fx.db, fx.round, Some(99999999), None, None)).rows.isEmpty)
      // the team's verdicts are the ones the full page shows
      val full = await(AdminSubmits.page(fx.db, fx.round, None, None, None)).rows.map(r => r.submit.submitId.id -> r.result.message).toMap
      assert(mine.forall(r => full(r.submit.submitId.id) == r.result.message))
    }
  }

  test("rejudged submits show their run count and history; the pushed row is the page's row") {
    assume(fx.url.isDefined)
    fx.withFixture {
      val (ids, _) = seed()
      val rows = await(AdminSubmits.page(fx.db, fx.round, None, None, None)).rows
      val byId = rows.map(r => r.submit.submitId.id.toLong -> r).toMap
      assert(byId(ids(0)).runs == 2 && byId(ids(4)).runs == 2 && byId(ids(1)).runs == 1 && byId(ids(2)).runs == 0)
      val current = byId(ids(0)).submit.testingId.get.toLong
      val runs = await(AdminSubmits.runHistory(fx.db, ids(0), Some(current)))
      assert(runs.size == 2 && runs.map(_.testingId) == runs.map(_.testingId).sorted.reverse)
      assert(runs.count(_.current) == 1 && runs.find(_.current).get.testingId == current)
      assert(runs.map(_.problemURL) == Seq("revision 2", "revision 1"))
      // AdminSubmits.row (what the live status page pushes) equals the page's row, for every submit
      rows.foreach { r =>
        val one = await(AdminSubmits.row(fx.db, r.submit.submitId.id)).getOrElse(fail(s"no row for ${r.submit.submitId.id}"))
        assert(one.result.message == r.result.message && one.runs == r.runs && one.stats == r.stats, s"submit ${r.submit.submitId.id}")
      }
      assert(await(AdminSubmits.row(fx.db, Int.MaxValue)).isEmpty)
      // the per-team-and-problem rows sent with a finished testing agree with the page
      val team = await(Submits.teamProblemSubmits(fx.db, fx.round, fx.kid, "Z"))
      assert(team.map(_.result.message).sorted == rows.filter(_.submit.submitId.teamId == fx.kid).map(_.result.message).sorted)
    }
  }

  test("revision status and the rejudge plan split the problem's submits by the revision of their last testing") {
    assume(fx.url.isDefined)
    fx.withFixture {
      val (ids, _) = seed()
      val st = await(AdminSubmits.revisionStatus(fx.db, fx.round))
      assert(st.map(_.problem) == Seq("Z"))
      val z = st.head
      // 20 tested: two at revision 2 (the newest), 18 at revision 1; 10 never tested
      assert(z.latestURL.contains("revision 2") && z.atLatest == 2 && z.outdated == 18 && z.inProgress == 0 && z.total == 30)
      assert(z.others == Seq("revision 1" -> 18))
      val plan = await(AdminSubmits.rejudgePlan(fx.db, fx.round, "Z", "revision 2"))
      assert(plan.current == 2 && plan.inProgress == 0 && plan.outdated.size == 18)
      assert(!plan.outdated.map(_._1).contains(ids(0)) && plan.outdated.forall(_._2 == "revision 1"))
      // a testing still running is not outdated but in progress
      val running = ids(1)
      exec(sqlu"update testings set finished_at = null where id = (select testing_id from submits where id = $running)")
      val plan2 = await(AdminSubmits.rejudgePlan(fx.db, fx.round, "Z", "revision 2"))
      assert(plan2.inProgress == 1 && plan2.outdated.size == 17)
      assert(await(AdminSubmits.allSubmitIds(fx.db, fx.round, "Z")) == ids)
    }
  }
}
