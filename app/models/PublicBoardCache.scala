package models

import live.{LiveHooks, SubmitRec}
import org.stingray.contester.dbmodel.Contest

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

/**
  * The rendered public scoreboard pages, per round and language. The caller fills it only for a round it has just
  * checked is over (PublicMonitor, through BoardAccess), so a running round's board is never in it, and it is asked
  * only after that check, so nothing cached is ever served while a round runs.
  *
  * Single-flight: an entry is the render's future from the moment it starts, so any number of concurrent misses of
  * one (round, language) share exactly one database render. A failed render is dropped, so the next request retries.
  *
  * A board of a round that is over still changes when a submit judged after the end finishes, on a rejudge, on a
  * credential or participant edit, and when the round itself changes (renamed, reopened, its problems): every one of
  * these arrives as a notification and drops the round's entries, finished or in flight (the hooks below), so a
  * request after it starts a fresh render; callers already waiting on the old render still get its result. Nothing
  * expires by time and nothing is refreshed on an interval. Thread-safe.
  */
class PublicBoardCache {
  private[this] val lock = new Object
  private[this] var pages = Map.empty[(Int, String), Future[String]]
  // Bumped on each drop; a render keeps its entry only if neither moved while it ran.
  private[this] var roundGen = Map.empty[Int, Long]
  private[this] var allGen = 0L

  private[this] def stamp(round: Int): (Long, Long) = (allGen, roundGen.getOrElse(round, 0L))

  /** The cached (or in-flight) page of the round in this language, or a new render shared by everyone who misses. */
  def getOrRender(round: Int, lang: String)(render: => Future[String])(implicit ec: ExecutionContext): Future[String] = {
    val key = (round, lang)
    val started: Either[Future[String], (Promise[String], (Long, Long))] = lock.synchronized {
      pages.get(key) match {
        case Some(page) => Left(page)
        case None =>
          val p = Promise[String]()
          pages += key -> p.future
          Right((p, stamp(round)))
      }
    }
    started match {
      case Left(page) => page
      case Right((p, before)) =>
        p.completeWith(Future.fromTry(Try(render)).flatten)
        p.future.onComplete { result =>
          lock.synchronized {
            val mine = pages.get(key).contains(p.future)
            val keep = result match {
              case Success(_) => stamp(round) == before
              case Failure(_) => false
            }
            if (mine && !keep) pages -= key
          }
        }
        p.future
    }
  }

  def invalidate(round: Int): Unit = lock.synchronized {
    roundGen += round -> (roundGen.getOrElse(round, 0L) + 1)
    pages = pages.filter(_._1._1 != round)
  }

  def invalidateAll(): Unit = lock.synchronized {
    allGen += 1
    pages = Map.empty
  }

  /** The rounds with a finished, stored page (for tests). */
  def cachedRounds: Set[Int] = lock.synchronized(pages.collect { case (k, f) if f.value.exists(_.isSuccess) => k._1 }.toSet)

  /** The rounds with an entry, finished or in flight (for tests). */
  def entryRounds: Set[Int] = lock.synchronized(pages.keySet.map(_._1))

  /** The notifications that drop pages, to be combined with the other live hooks. */
  val hooks: LiveHooks = new LiveHooks {
    override def submitArrived(contest: Int, rec: SubmitRec): Unit = invalidate(contest)
    override def finished(contest: Int, id: Int, compiled: Boolean, points: Option[Double]): Unit = invalidate(contest)
    // The hook has only the submit id; a rejudge is rare enough to drop every round's pages.
    override def rejudged(id: Int): Unit = invalidateAll()
    // One drop for a whole problem rejudge.
    override def rejudgedAll(ids: Seq[Int]): Unit = if (ids.nonEmpty) invalidateAll()
    override def teamsChanged(roundId: Int): Unit = invalidate(roundId)
    override def contestChanged(contest: Contest): Unit = invalidate(contest.id)
    override def contestRemoved(roundId: Int): Unit = invalidate(roundId)
  }
}
