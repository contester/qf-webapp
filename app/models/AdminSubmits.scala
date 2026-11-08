package models

import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{Memory, SlickModel, TimeMs}
import slick.jdbc.JdbcBackend

import scala.concurrent.{ExecutionContext, Future}

/** One row of the admin status page. No per-test details: those are only needed on the submit page. */
case class AdminSubmitRow(submit: Submit, score: Option[Score], result: SubmitResult, stats: SubmitStats, runs: Int)

/**
 * A page of the admin status page, newest first. `older`/`newer` are the id cursors to pass as
 * `before`/`after` to get the neighbouring pages (None when there is no such page).
 */
case class AdminSubmitPage(rows: Seq[AdminSubmitRow], older: Option[Long], newer: Option[Long])

/** What the results of one testing add up to, computed in the database without reading the blobs. */
case class RunAggregate(testingId: Long, lastTest: Int, lastCode: Int, allOk: Boolean, stats: SubmitStats)

/** One testing of a submit, for the run history on the submit page. */
case class RunSummary(testingId: Long, started: DateTime, finished: Option[DateTime], problemURL: String,
                      verdict: String, success: Boolean, stats: SubmitStats, current: Boolean)

/**
 * What rejudging a problem to a given problem URL (with revision) would do.
 * @param outdated finished submits whose last testing used another URL, with that URL
 * @param inProgress submits that would qualify but are still being tested (left alone)
 * @param current submits whose last testing already used the URL
 */
case class RejudgePlan(outdated: Seq[(Long, String)], inProgress: Int, current: Int)

/**
 * How a problem's submits stand against what the contest definition says the problem is now.
 * @param latestURL the problem's URL in the contest definition with the newest cached revision;
 *                  None while the director has no revision of it cached
 * @param atLatest submits whose last testing used exactly that URL (same problem, same revision)
 * @param outdated finished submits tested against anything else: an older revision, or another problem
 * @param inProgress submits at anything else that are being tested right now
 * @param others what the outdated and in-progress submits were tested against, with counts
 * @param total all submits of the problem, tested or not
 */
case class ProblemRevisionStatus(problem: String, latestURL: Option[String], atLatest: Int, outdated: Int, inProgress: Int,
                                 others: Seq[(String, Int)], total: Int)

object AdminSubmits {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import org.stingray.contester.dbmodel.SlickModel.UpliftedSubmit

  val PageSize = 100

  /** The same join as SlickModel.submits0, but unsorted so that callers choose the order. */
  private def submitsQuery = for {
    s <- SlickModel.submits
    contest <- SlickModel.contests if s.contest === contest.id
    problem <- SlickModel.problems if s.problem === problem.id && s.contest === problem.contestID
    lang <- SlickModel.compilers if s.language === lang.id
  } yield SlickModel.LiftedSubmit(s.id, s.arrived, s.arrived - contest.startTime, s.arrived > contest.freezeTime,
    s.team, s.contest, s.problem, lang.moduleID, s.tested, s.compiled, s.passed, s.taken, s.testingID)

  /**
   * Per-testing aggregates over `results` without touching tester_output/tester_error.
   * Stats ignore test 0 (compilation), like Submits.getTestingStats.
   */
  def aggregates(testingIds: Seq[Long])(implicit ec: ExecutionContext): DBIO[Map[Long, RunAggregate]] =
    if (testingIds.isEmpty) DBIO.successful(Map.empty)
    else {
      val ids = testingIds.distinct.mkString(",") // Longs, nothing to escape
      sql"""select testing_id, max(test_id)::int, (array_agg(result_code order by test_id desc))[1]::int,
                   bool_and(case when test_id = 0 then result_code = 1 else result_code = 10 end),
                   coalesce(max(time_ms) filter (where test_id <> 0), 0),
                   coalesce(max(memory_bytes) filter (where test_id <> 0), 0),
                   coalesce(bool_or(result_code = 11) filter (where test_id <> 0), false)
              from results where testing_id in (#$ids) group by testing_id"""
        .as[(Long, Int, Int, Boolean, Long, Long, Boolean)]
        .map(_.map { case (tid, lastTest, lastCode, allOk, time, mem, tle) =>
          tid -> RunAggregate(tid, lastTest, lastCode, allOk, SubmitStats(TimeMs(time), Memory(mem), tle))
        }.toMap)
    }

  private def runCounts(submitIds: Seq[Long])(implicit ec: ExecutionContext): DBIO[Map[Long, Int]] =
    if (submitIds.isEmpty) DBIO.successful(Map.empty)
    else {
      val ids = submitIds.mkString(",")
      sql"select submit, count(*)::int from testings where submit in (#$ids) group by submit"
        .as[(Long, Int)].map(_.toMap)
    }

  private def lastResult(a: Option[RunAggregate]): Option[(String, Int)] =
    a.map(x => (SubmitResult.message.getOrElse(x.lastCode, "unknown"), x.lastTest))

  /**
   * The rows for the submits `shown` (newest first). `base` is the query they came from: the scores need everything
   * the same teams submitted for the same problems in it, up to the newest shown submit.
   */
  private def describe(db: JdbcBackend#DatabaseDef, base: Query[SlickModel.LiftedSubmit, UpliftedSubmit, Seq], shown: Seq[Submit])(
    implicit ec: ExecutionContext): Future[Seq[AdminSubmitRow]] = {
    val maxId = shown.head.submitId.id.toLong
    val pairs = shown.map(s => (s.submitId.teamId, s.submitId.problem.id)).distinct
    val context = base.filter(x => pairs.map { case (t, p) => x.teamID === t && x.problemID === p }.reduceLeft(_ || _))
      .filter(_.id <= maxId)
    val testingIds = shown.flatMap(_.testingId.map(_.toLong))

    db.run(context.result.zip(aggregates(testingIds)).zip(runCounts(shown.map(_.submitId.id.toLong))).map {
      case ((ctx, aggs), counts) => (ctx, aggs, counts)
    }).map { case (ctx, aggs, counts) =>
      // groupByTP orders by whole seconds and is stable, so what the query returns decides submits in the same second:
      // by id, the same for every page and for a single row.
      val scores: Map[Int, Option[Score]] = Submits.groupByTP(ctx.map(Submits.upliftSub).sortBy(_.submitId.id)).values.flatMap { subs =>
        Submits.scoreGrouped(subs, ACMCell.empty, ACMScorer)._2.map { case (s, sc) => s.submitId.id -> sc }
      }.toMap

      shown.map { s =>
        val agg = s.testingId.flatMap(t => aggs.get(t.toLong))
        AdminSubmitRow(s, scores.getOrElse(s.submitId.id, None),
          SubmitResult.fromLast(schoolMode = false, s, lastResult(agg)),
          agg.map(_.stats).getOrElse(SubmitStats(TimeMs(0), Memory(0))),
          counts.getOrElse(s.submitId.id.toLong, 0))
      }
    }
  }

  /** One submit as the status page shows it, in whatever contest it is, with no freeze applied. None: no such submit. */
  def row(db: JdbcBackend#DatabaseDef, id: Int)(implicit ec: ExecutionContext): Future[Option[AdminSubmitRow]] =
    db.run(submitsQuery.filter(_.id === id.toLong).result.headOption).flatMap {
      case None => Future.successful(None)
      case Some(found) =>
        val s = Submits.upliftSub(found)
        describe(db, submitsQuery.filter(_.contestID === s.submitId.contestId), Seq(s)).map(_.headOption)
    }

  /**
   * One page of a contest's submits, newest first.
   * @param team only this team's submits
   * @param before only submits with a smaller id (the next, older page)
   * @param after only submits with a bigger id (the previous, newer page)
   * @param canSeeAll false hides submits after the freeze, from the list and from the scores
   */
  def page(db: JdbcBackend#DatabaseDef, contest: Int, team: Option[Int], before: Option[Long], after: Option[Long],
           canSeeAll: Boolean, size: Int = PageSize)(implicit ec: ExecutionContext): Future[AdminSubmitPage] = {
    val base = submitsQuery.filter(_.contestID === contest).filter(x => if (canSeeAll) true.bind else !x.afterFreeze)
      .filterOpt(team)(_.teamID === _)

    val fetch = after match {
      case Some(a) => base.filter(_.id > a).sortBy(_.id.asc).take(size + 1)
      case None => base.filterOpt(before)(_.id < _).sortBy(_.id.desc).take(size + 1)
    }

    db.run(fetch.result).flatMap { fetched =>
      val hasMore = fetched.size > size
      // `after` fetches from the cursor upwards; flip to newest first.
      val shown = (if (after.isDefined) fetched.take(size).reverse else fetched.take(size)).map(Submits.upliftSub)
      if (shown.isEmpty) Future.successful(AdminSubmitPage(Nil, None, None))
      else {
        val maxId = shown.head.submitId.id.toLong
        val minId = shown.last.submitId.id.toLong
        describe(db, base, shown).map { rows =>
          val (older, newer) = after match {
            case Some(_) => (Some(minId), if (hasMore) Some(maxId) else None)
            case None => (if (hasMore) Some(minId) else None, before.map(_ => maxId))
          }
          AdminSubmitPage(rows, older, newer)
        }
      }
    }
  }

  private def verdict(finished: Boolean, a: Option[RunAggregate]): (String, Boolean) = a match {
    case None => if (finished) ("no results", false) else ("Running...", false)
    case Some(x) if x.lastTest == 0 && x.lastCode != 1 => ("Compilation error", false)
    case Some(x) if !finished => ("Running...", false)
    case Some(x) if x.allOk && x.lastTest > 0 => ("ACCEPTED", true)
    case Some(x) if x.lastTest == 0 => ("Compiled, no tests run", false)
    case Some(x) => (s"${SubmitResult.message.getOrElse(x.lastCode, "unknown")} on test ${x.lastTest}", false)
  }

  /** Every testing of the submit, newest first. */
  def runHistory(db: JdbcBackend#DatabaseDef, submitId: Long, currentTesting: Option[Long])(implicit ec: ExecutionContext): Future[Seq[RunSummary]] =
    db.run(SlickModel.testings.filter(_.submit === submitId).sortBy(_.id.desc).result.flatMap { ts =>
      aggregates(ts.map(_._1)).map { aggs =>
        ts.map { case (id, _, start, url, finish) =>
          val agg = aggs.get(id)
          val (text, ok) = verdict(finish.isDefined, agg)
          RunSummary(id, start, finish, url, text, ok, agg.map(_.stats).getOrElse(SubmitStats(TimeMs(0), Memory(0))),
            currentTesting.contains(id))
        }
      }
    })

  /** Submits of the problem in the contest, split by whether their last testing used `problemURL`. */
  def rejudgePlan(db: JdbcBackend#DatabaseDef, contest: Int, problem: String, problemURL: String)(implicit ec: ExecutionContext): Future[RejudgePlan] =
    db.run((for {
      s <- SlickModel.submits if s.contest === contest && s.problem === problem
      t <- SlickModel.testings if t.id === s.testingID
    } yield (s.id, t.problemURL, t.finishTime.isDefined)).sortBy(_._1).result).map { rows =>
      val (current, other) = rows.partition(_._2 == problemURL)
      val (done, running) = other.partition(_._3)
      RejudgePlan(done.map(r => (r._1, r._2)), running.size, current.size)
    }

  /**
   * Every problem of the contest definition (polygon_contest_cache, by the contest's polygon_id) against its
   * newest cached revision (polygon_problem_cache), with where the contest's submits were actually tested.
   * Rolling back inside a transaction is possible because this is a plain DBIO.
   */
  def revisionStatusAction(contest: Int)(implicit ec: ExecutionContext): DBIO[Seq[ProblemRevisionStatus]] = {
    val defined = sql"""select upper(x.idx), x.url,
                   (select max(k.problem_revision) from polygon_problem_cache k where k.problem_source_base_url = x.url)
              from contests c
              join polygon_contest_cache pc on pc.source_url = c.polygon_id,
                   xmltable('/contest/problems/problem' passing pc.contest_xml
                            columns idx text path '@index', url text path '@url') x
             where c.id = $contest
             order by 1""".as[(String, String, Option[Long])]
    val counts = sql"""select s.problem, t.problem_url, t.finish_time is not null, count(*)::int
              from submits s join testings t on t.id = s.testing_id
             where s.contest = $contest group by 1, 2, 3""".as[(String, String, Boolean, Int)]
    val totals = sql"select problem, count(*)::int from submits where contest = $contest group by 1".as[(String, Int)]
    defined.zip(counts).zip(totals).map { case ((problems, countRows), totalRows) =>
      val totalMap = totalRows.toMap
      problems.map { case (problem, base, rev) =>
        val url = rev.map(r => s"$base?revision=$r")
        val (cur, other) = countRows.filter(_._1 == problem).partition(r => url.contains(r._2))
        ProblemRevisionStatus(problem, url, cur.map(_._4).sum,
          other.filter(_._3).map(_._4).sum, other.filterNot(_._3).map(_._4).sum,
          other.groupBy(_._2).map { case (u, rs) => u -> rs.map(_._4).sum }.toSeq.sortBy(_._1),
          totalMap.getOrElse(problem, 0))
      }
    }
  }

  def revisionStatus(db: JdbcBackend#DatabaseDef, contest: Int)(implicit ec: ExecutionContext): Future[Seq[ProblemRevisionStatus]] =
    db.run(revisionStatusAction(contest))

  /** Every submit of the problem in the contest, tested or not. */
  def allSubmitIds(db: JdbcBackend#DatabaseDef, contest: Int, problem: String)(implicit ec: ExecutionContext): Future[Seq[Long]] =
    db.run(SlickModel.submits.filter(s => s.contest === contest && s.problem === problem).map(_.id).sorted.result)
}
