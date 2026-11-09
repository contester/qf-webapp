package live

import akka.NotUsed
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Flow, Source}
import play.api.libs.EventSource.Event
import play.api.libs.json.{JsObject, Json}

import java.util.concurrent.CopyOnWriteArrayList
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/** The kinds of live connection the contester page counts; `id` is the column name the page gets. */
sealed abstract class FeedKind(val id: String)

object FeedKind {
  case object PublicMonitor extends FeedKind("publicMonitor")
  case object TeamMonitor extends FeedKind("teamMonitor")
  case object TeamFeed extends FeedKind("teamFeed")
  /** Admin monitor of an account that cannot see everything: it gets the freezing board. */
  case object AdminMonitorRestricted extends FeedKind("adminMonitorRestricted")
  case object AdminMonitorUnrestricted extends FeedKind("adminMonitorUnrestricted")
  case object AdminFeed extends FeedKind("adminFeed")
  case object PublicPreview extends FeedKind("publicPreview")
  case object StatusRows extends FeedKind("statusRows")

  /** In the order of the columns. */
  val all: Seq[FeedKind] = Seq(PublicMonitor, TeamMonitor, TeamFeed, AdminMonitorRestricted, AdminMonitorUnrestricted,
    AdminFeed, PublicPreview, StatusRows)
}

/**
  * How many connections are open now, per contest and kind of feed. Thread-safe: streams open and close on any thread.
  * Listeners are told of every change and read the new state themselves.
  */
class ConnectionStats {
  private val lock = new Object
  private var counts = Map.empty[Int, Map[FeedKind, Int]]
  private val listeners = new CopyOnWriteArrayList[() => Unit]

  def opened(kind: FeedKind, contest: Int): Unit = change(kind, contest, 1)

  def closed(kind: FeedKind, contest: Int): Unit = change(kind, contest, -1)

  private def change(kind: FeedKind, contest: Int, by: Int): Unit = {
    val changed = lock.synchronized {
      val was = counts.getOrElse(contest, Map.empty).getOrElse(kind, 0)
      val now = math.max(0, was + by)
      if (now == was) false
      else {
        val row = counts.getOrElse(contest, Map.empty)
        val newRow = if (now == 0) row - kind else row.updated(kind, now)
        counts = if (newRow.isEmpty) counts - contest else counts.updated(contest, newRow)
        true
      }
    }
    if (changed) listeners.asScala.foreach(_())
  }

  def snapshot: Map[Int, Map[FeedKind, Int]] = lock.synchronized(counts)

  /** Counts the stream from the moment it starts until it ends, completes, fails or is cancelled by the client. */
  def track[T](kind: FeedKind, contest: Int): Flow[T, T, akka.NotUsed] =
    Flow[T].watchTermination() { (m, done) =>
      opened(kind, contest)
      done.onComplete(_ => closed(kind, contest))(ConnectionStats.sameThread)
      m
    }

  def listen(f: () => Unit): AutoCloseable = {
    listeners.add(f)
    new AutoCloseable {
      override def close(): Unit = {
        listeners.remove(f)
        ()
      }
    }
  }

  private[live] def listenerCount: Int = listeners.size

  private val pingEvent = Event("", None, Some("ping"))

  /**
    * For the contester page: a `counts` event with everything now, and another after each change, at most one in
    * `every`; a `ping` between, so the page can tell a quiet stream from a dead one.
    */
  def events(every: FiniteDuration = 1.second, ping: FiniteDuration = 15.seconds): Source[Event, NotUsed] = {
    val changes = Source.queue[Unit](1, OverflowStrategy.dropHead)
      .throttle(1, every)
      .map(_ => Event(json.toString, None, Some("counts")))
      .watchTermination() { (q, done) =>
        // The first element is the state now; whatever changes before it is read is covered by reading late.
        val l = listen(() => q.offer(()))
        q.offer(())
        done.onComplete(_ => l.close())(ConnectionStats.sameThread)
        NotUsed
      }
    changes.merge(Source.tick(ping, ping, pingEvent), eagerComplete = true)
  }

  /** `kinds` names the columns; each contest's row has one count per column, in that order. */
  def json: JsObject = {
    val snap = snapshot
    Json.obj(
      "kinds" -> FeedKind.all.map(_.id),
      "contests" -> JsObject(snap.toSeq.sortBy(_._1).map { case (cid, row) =>
        cid.toString -> Json.toJson(FeedKind.all.map(k => row.getOrElse(k, 0)))
      }))
  }
}

object ConnectionStats {
  // The callbacks only adjust a counter; they need no thread of their own.
  private[live] val sameThread: ExecutionContext = new ExecutionContext {
    override def execute(r: Runnable): Unit = r.run()
    override def reportFailure(t: Throwable): Unit = t.printStackTrace()
  }
}
