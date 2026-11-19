package models

import live.{LiveHooks, SubmitRec}
import org.stingray.contester.dbmodel.Contest

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
  * The rendered public scoreboard pages, per round and language. The caller fills it only for a round it has just
  * checked is over (PublicMonitor, through BoardAccess), so a running round's board is never in it, and it is asked
  * before the cache only after that check, so nothing cached is ever served while a round runs.
  *
  * A board of a round that is over still changes when a submit judged after the end finishes, on a rejudge, on a
  * credential or participant edit, and when the round itself changes (renamed, reopened, its problems): every one of
  * these arrives as a notification and drops the round's pages (the hooks below). Nothing expires by time and nothing
  * is refreshed on an interval. A render that was already running when its round was dropped is not stored.
  * Thread-safe.
  */
class PublicBoardCache {
  private[this] val lock = new Object
  private[this] var pages = Map.empty[(Int, String), String]
  // Bumped on each drop; a render stores its page only if neither moved while it ran.
  private[this] var roundGen = Map.empty[Int, Long]
  private[this] var allGen = 0L

  private[this] def stamp(round: Int): (Long, Long) = (allGen, roundGen.getOrElse(round, 0L))

  /** The cached page of the round in this language, or `render`'s (stored unless the round was dropped meanwhile). */
  def getOrRender(round: Int, lang: String)(render: => Future[String])(implicit ec: ExecutionContext): Future[String] = {
    val (hit, before) = lock.synchronized((pages.get((round, lang)), stamp(round)))
    hit match {
      case Some(page) => Future.successful(page)
      case None =>
        Future.fromTry(Try(render)).flatten.map { page =>
          lock.synchronized(if (stamp(round) == before) pages += (round, lang) -> page)
          page
        }
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

  /** The rounds with a cached page (for tests). */
  def cachedRounds: Set[Int] = lock.synchronized(pages.keySet.map(_._1))

  /** The notifications that drop pages, to be combined with the other live hooks. */
  val hooks: LiveHooks = new LiveHooks {
    override def submitArrived(contest: Int, rec: SubmitRec): Unit = invalidate(contest)
    override def finished(contest: Int, id: Int, compiled: Boolean, points: Option[Double]): Unit = invalidate(contest)
    // The hook has only the submit id; a rejudge is rare enough to drop every round's pages.
    override def rejudged(id: Int): Unit = invalidateAll()
    override def teamsChanged(roundId: Int): Unit = invalidate(roundId)
    override def contestChanged(contest: Contest): Unit = invalidate(contest.id)
    override def contestRemoved(roundId: Int): Unit = invalidate(roundId)
  }
}
