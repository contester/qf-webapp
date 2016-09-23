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
  case class Loaded(tasks: List[StoredWaiterTask])

  case class NewTask(message: String, rooms: List[String])
  case class AckTask(id: Long, room: String)
  case class UnackTask(id: Long, room: String)
  case class DeleteTask(id: Long)

  case class TaskAcked(id: Long, when: DateTime, room: String)
  case class TaskUnacked(id: Long, room: String)
  case class TaskDeleted(id: Long)

  case class Join(rooms: List[String], requestHeader: RequestHeader)
  //case class Joined(enum: Enumerator[Event])

  case class GetSnapshot(rooms: List[String])
  case class Snapshot(tasks: Seq[AdaptedWaiterTask])
}

// UI events:
// - update (id) -> will insert or update row.
// - delete (id) -> will delete row.

trait WaiterTaskEvent {
  def filter(vrooms: Set[String]): Boolean
  def toEvent(vrooms: Set[String], requestHeader: RequestHeader): Event
}

case class WaiterTaskDeleted(id: Long) extends WaiterTaskEvent {
  override def filter(vrooms: Set[String]): Boolean = true

  implicit val format = Json.format[WaiterTaskDeleted]

  override def toEvent(vrooms: Set[String], requestHeader: RequestHeader): Event =
    Event(Json.stringify(Json.toJson(this)), None, Some("waiterTaskDeleted"))
}

case class WaiterTaskUpdate(id: Long, content: String)
object WaiterTaskUpdate {
  implicit val format = Json.format[WaiterTaskUpdate]
}

case class WaiterTaskUpdated(inner: StoredWaiterTask) extends WaiterTaskEvent {
  override def filter(vrooms: Set[String]): Boolean = inner.matches(vrooms)

  override def toEvent(vrooms: Set[String], requestHeader: RequestHeader): Event = {
    val c = views.html.admin.singlewaitertask(inner.adapt(vrooms.toList))
    val e = WaiterTaskUpdate(inner.id, c.body)
    Event(Json.stringify(Json.toJson(e)), None, Some("waiterTaskUpdated"))
  }
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

  private def filterByRooms(rooms: Set[String]) = Enumeratee.filter[WaiterTaskEvent](_.filter(rooms))

  import scala.language.implicitConversions

  private def wt2event2(vrooms: Set[String], requestHeader: RequestHeader): Enumeratee[WaiterTaskEvent, Event] =
    Enumeratee.map { e => e.toEvent(vrooms, requestHeader)}

  private def storedSnapshot: Iterable[WaiterTaskEvent] = tasks.values.map(WaiterTaskUpdated)

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
    case GetSnapshot(vrooms: List[String]) => {
      import com.github.nscala_time.time.Imports._
      val r = tasks.values.map(_.adapt(vrooms)).toSeq.sortBy(_.when).reverse
      sender ! Snapshot(r)
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
      }
    }

    case Join(rooms, requestHeader) => {
      val r: Enumerator[Event] = Enumerator.enumerate(storedSnapshot).andThen(waiterOut) &> filterByRooms(rooms.toSet) &>
        wt2event2(rooms.toSet, requestHeader)
      sender ! Success(r)
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