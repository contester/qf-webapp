package models

import org.joda.time.DateTime
import org.stingray.contester.dbmodel.{EvalEntry, Memory, SlickModel, TimeMs}
import slick.jdbc.{GetResult, JdbcBackend}

import scala.concurrent.{ExecutionContext, Future}

/** One row of the admin custom tests list. No blobs, only their sizes in bytes. */
case class AdminCustomRow(id: Long, team: Int, language: String, arrived: DateTime, finished: Option[DateTime],
                          resultCode: Int, timeMs: TimeMs, memory: Memory, returnCode: Long,
                          sourceSize: Int, inputSize: Int, outputSize: Int) {
  def running: Boolean = finished.isEmpty
}

/** A page of custom tests, newest first; `older`/`newer` are id cursors as in [[AdminSubmitPage]]. */
case class AdminCustomPage(rows: Seq[AdminCustomRow], older: Option[Long], newer: Option[Long])

object AdminCustomTests {
  import org.stingray.contester.dbmodel.MyPostgresProfile.api._

  val PageSize = 100

  private implicit val getRow: GetResult[AdminCustomRow] = GetResult { r =>
    AdminCustomRow(r.nextLong(), r.nextInt(), r.nextString(), new DateTime(r.nextTimestamp().getTime),
      r.nextTimestampOption().map(t => new DateTime(t.getTime)), r.nextInt(), TimeMs(r.nextLong()),
      Memory(r.nextLong()), r.nextLong(), r.nextInt(), r.nextInt(), r.nextInt())
  }

  /**
   * One page of a contest's custom tests, newest first, read without the source/input/output blobs.
   * @param before only tests with a smaller id (the next, older page)
   * @param after only tests with a bigger id (the previous, newer page)
   */
  def page(db: JdbcBackend#DatabaseDef, contest: Int, team: Option[Int], before: Option[Long], after: Option[Long],
           size: Int = PageSize)(implicit ec: ExecutionContext): Future[AdminCustomPage] = {
    // Ints and Longs only, nothing to escape.
    val teamCond = team.fold("true")(t => s"c.team_id = $t")
    val (cond, order) = after match {
      case Some(a) => (s"c.id > $a", "asc")
      case None => (before.fold("true")(b => s"c.id < $b"), "desc")
    }
    val limit = size + 1
    val q = sql"""select c.id, c.team_id, l.name, c.submit_time_absolute, c.finish_time, c.result_code, c.time_ms,
                         c.memory_bytes, c.return_code, octet_length(c.source), octet_length(c.input),
                         coalesce(octet_length(c.output), 0)
                  from custom_test c join languages l on l.id = c.language_id
                  where c.contest = #$contest and #$teamCond and #$cond
                  order by c.id #$order limit #$limit"""
    db.run(q.as[AdminCustomRow]).map { fetched =>
      val hasMore = fetched.size > size
      val shown = if (after.isDefined) fetched.take(size).reverse else fetched.take(size)
      if (shown.isEmpty) AdminCustomPage(Nil, None, None)
      else {
        val maxId = shown.head.id
        val minId = shown.last.id
        val (older, newer) = after match {
          case Some(_) => (Some(minId), if (hasMore) Some(maxId) else None)
          case None => (if (hasMore) Some(minId) else None, before.map(_ => maxId))
        }
        AdminCustomPage(shown, older, newer)
      }
    }
  }

  /** The whole custom test, with blobs, if it belongs to the contest. */
  def get(db: JdbcBackend#DatabaseDef, contest: Int, id: Long)(implicit ec: ExecutionContext): Future[Option[EvalEntry]] =
    db.run(SlickModel.customTestWithLang.filter(x => x.id === id && x.contest === contest).take(1).result.headOption)

  private def decode(b: Array[Byte]) = new String(b, "CP1251")
  def sourceText(e: EvalEntry): String = decode(e.source)
  def inputText(e: EvalEntry): String = decode(e.input)
  def outputText(e: EvalEntry): String = decode(e.output)
}
