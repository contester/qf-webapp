package actors

import akka.actor.Actor.Receive
import akka.actor.{Actor, Props, Stash}
import models._
import play.api.Logger
import play.api.libs.EventSource
import play.api.libs.EventSource.{Event, EventDataExtractor, EventNameExtractor}
import play.api.libs.iteratee.{Concurrent, Enumeratee, Enumerator}
import play.api.libs.json.Json
import play.api.mvc.RequestHeader
import slick.jdbc.JdbcBackend
import utils.Ask

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.Success

// Spectator has a list of rooms it cares for.
// Task goes to the room/list of rooms.
// Spectators for these rooms get events.
// Spectators can ack their rooms.

// Events:
// - Task added (id, rooms unacked, rooms acked, message)
// - Task updated (same)
// - Task deleted (id)

import com.github.nscala_time.time.Imports.DateTime

// Task is visible:
// - to admins
// - if any room in it matches our rooms
// Task can be acked for a room:
// - by admins
// - if that room matches our list

object WaiterActor {
  def props(db: JdbcBackend#DatabaseDef) = Props(classOf[WaiterActor], db)

  case object Load
  case class Loaded(tasks: Iterable[StoredWaiterTask])

  case class NewTask(message: String, rooms: Iterable[String])
  case class AckTask(id: Long, room: String)
  case class UnackTask(id: Long, room: String)
  case class DeleteTask(id: Long)

  case class TaskAcked(id: Long, when: DateTime, room: String)
  case class TaskUnacked(id: Long, room: String)
  case class TaskDeleted(id: Long)

  case class Join(perm: WaiterPermissions, requestHeader: RequestHeader)

  case class GetSnapshot(perm: WaiterPermissions)
  case class Snapshot(tasks: Seq[AdaptedWaiterTask])

  // can be filtered against rooms
  type HeaderRows = Iterable[Iterable[String]]

  case class HeaderSource(rows: HeaderRows, added: Option[StoredWaiterTask])

  def convertHeaderSource(perm: WaiterPermissions)(implicit ec: ExecutionContext): Enumeratee[HeaderSource, Event] = Enumeratee.map { v =>
    val count = v.rows.count(_.exists(perm.filter))
    val msg = v.added.filter(_.matches(perm)).map(_.message).getOrElse("")
    WaiterTaskHeader(count, msg).toEvent(perm, null)
  }
}

// UI events:
// - update (id) -> will insert or update row.
// - delete (id) -> will delete row.

trait WaiterTaskEvent {
  def filter(perm: WaiterPermissions): Boolean
  def toEvent(perm: WaiterPermissions, requestHeader: RequestHeader): Event
}

case class WaiterTaskDeleted(id: Long) extends WaiterTaskEvent {
  override def filter(perm: WaiterPermissions): Boolean = true

  implicit val format = Json.format[WaiterTaskDeleted]

  override def toEvent(perm: WaiterPermissions, requestHeader: RequestHeader): Event =
    Event(Json.stringify(Json.toJson(this)), None, Some("waiterTaskDeleted"))
}

case class WaiterTaskUpdate(id: Long, content: String)
object WaiterTaskUpdate {
  implicit val format = Json.format[WaiterTaskUpdate]
}

case class WaiterTaskUpdated(inner: StoredWaiterTask) extends WaiterTaskEvent {
  override def filter(perm: WaiterPermissions): Boolean = inner.matches(perm)

  override def toEvent(perm: WaiterPermissions, requestHeader: RequestHeader): Event = {
    val c = views.html.admin.singlewaitertask(inner.adapt(perm))
    val e = WaiterTaskUpdate(inner.id, c.body)
    Event(Json.stringify(Json.toJson(e)), None, Some("waiterTaskUpdated"))
  }
}

case class WaiterTaskHeader(outstanding: Int, text: String) extends WaiterTaskEvent {
  override def filter(perm: WaiterPermissions): Boolean = true

  implicit val format = Json.format[WaiterTaskHeader]
  override def toEvent(perm: WaiterPermissions, requestHeader: RequestHeader): Event =
    Event(Json.stringify(Json.toJson(this)), None, Some("waiterTaskHeader"))
}

class WaiterActor(db: JdbcBackend#DatabaseDef) extends Actor with Stash {
  val tasks = mutable.Map[Long, StoredWaiterTask]()

  import scala.concurrent.ExecutionContext.Implicits.global

  import WaiterActor._

  @throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    super.preStart()
    self ! Load
  }

  private val (waiterOut, waiterChannel) = Concurrent.broadcast[WaiterTaskEvent]

  private val (headerOut, headerChannel) = Concurrent.broadcast[HeaderSource]

  private def filterByRooms(perm: WaiterPermissions) = Enumeratee.filter[WaiterTaskEvent](_.filter(perm))

  import scala.language.implicitConversions

  private def wt2event2(perm: WaiterPermissions, requestHeader: RequestHeader): Enumeratee[WaiterTaskEvent, Event] =
    Enumeratee.map { e => e.toEvent(perm, requestHeader)}

  private def storedSnapshot: Iterable[WaiterTaskEvent] = tasks.values.map(WaiterTaskUpdated)

  private def getHeaderRows = tasks.values.map(_.unacked)

  private def getActual: HeaderSource = {
    HeaderSource(getHeaderRows, None)
  }

  override def receive: Receive = {
    case Load => {
      val f = WaiterModel.load(db)

      f.onSuccess {
        case loaded => self ! Loaded(loaded)
      }
      f.onFailure {
        case x =>
          Logger.error("failed to load", x)
          // reload in 5 seconds
          import scala.concurrent.duration._
          import scala.language.postfixOps
          context.system.scheduler.scheduleOnce(5 seconds, self, Load)
      }
    }

    case Loaded(newtasks) => {
      newtasks.foreach { task =>
        tasks.put(task.id, task)
      }
      context.become(initialized)
    }
  }

  def initialized: Receive = {
    case GetSnapshot(perm) => {
      import com.github.nscala_time.time.Imports._
      val r = tasks.values.map(_.adapt(perm)).toSeq.sortBy(_.when).reverse
      sender ! Success(Snapshot(r))
    }

    case NewTask(message, rooms) => {
      val saved = sender()
      WaiterModel.addNewTask(db, message, rooms).map { s =>
        self ! s
        s
      }.onComplete(Ask.respond(saved, _))
    }

    case task: StoredWaiterTask => {
      tasks.put(task.id, task)
      waiterChannel.push(WaiterTaskUpdated(task))
      headerChannel.push(HeaderSource(getHeaderRows, Some(task)))
    }

    case AckTask(id, room) =>
      WaiterModel.markDone(db, id, room).onSuccess {
        case (when) => self ! TaskAcked(id, when, room)
      }

    case TaskAcked(id, when, room) => {
      tasks.get(id).foreach { stored =>
        val newMsg = StoredWaiterTask(stored.id, stored.when, stored.message, stored.rooms, stored.acked.updated(room, when))
        tasks.put(id, newMsg)
        waiterChannel.push(WaiterTaskUpdated(newMsg))
        headerChannel.push(getActual)
      }
    }

    case UnackTask(id, room) => {
      WaiterModel.unmarkDone(db, id, room).onSuccess {
        case _ =>
          self ! TaskUnacked(id, room)
      }
    }

    case TaskUnacked(id, room) => {
      tasks.get(id).foreach { stored =>
        val newMsg = StoredWaiterTask(stored.id, stored.when, stored.message, stored.rooms, stored.acked - room)
        tasks.put(id, newMsg)
        waiterChannel.push(WaiterTaskUpdated(newMsg))
        headerChannel.push(getActual)
      }
    }

    case Join(perm, requestHeader) => {
      val r: Enumerator[Event] = Enumerator.enumerate(storedSnapshot)
        .andThen(waiterOut) &> filterByRooms(perm) &>
        wt2event2(perm, requestHeader)
      val r2 = Enumerator(getActual).andThen(headerOut) &> convertHeaderSource(perm)
      sender ! Success(Enumerator.interleave(r, r2))
    }

    case DeleteTask(id) => {
      WaiterModel.delete(db, id).onComplete {
        case _ => self ! TaskDeleted(id)
      }
    }

    case TaskDeleted(id) => {
      tasks.remove(id)
      waiterChannel.push(WaiterTaskDeleted(id))
    }
  }
}