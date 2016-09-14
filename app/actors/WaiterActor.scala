package actors

import akka.actor.Actor.Receive
import akka.actor.{Actor, Props, Stash}
import models._
import play.api.libs.EventSource
import play.api.libs.EventSource.{Event, EventDataExtractor, EventNameExtractor}
import play.api.libs.iteratee.{Concurrent, Enumeratee, Enumerator}
import play.api.libs.json.Json
import slick.jdbc.JdbcBackend

import scala.collection.mutable

// Spectator has a list of rooms it cares for.
// Task goes to the room/list of rooms.
// Spectators for these rooms get events.
// Spectators can ack their rooms.

// Events:
// - Task added (id, rooms unacked, rooms acked, message)
// - Task updated (same)
// - Task deleted (id)

import com.github.nscala_time.time.Imports.DateTime


object WaiterActor {
  def props(db: JdbcBackend#DatabaseDef) = Props(classOf[WaiterActor], db)

  case object Load
  case class Loaded(tasks: List[StoredWaiterTask])

  case class NewTask(message: String, rooms: List[String])
  case class AckTask(id: Long, room: String)
  case class UnackTask(id: Long, room: String)
  case class DeleteTask(id: Long)

  case class TaskAcked(id: Long, when: DateTime, room: String)
  case class TaskDeleted(id: Long) extends WaiterTaskEvent {
    override def matches(rooms: Set[String]): Boolean = true
    implicit val format = Json.format[TaskDeleted]

    override def toEvent: Event = Event(Json.stringify(Json.toJson(this)), None, Some("waiterTaskDeleted"))
  }

  case class Join(rooms: List[String])
  case class Joined(enum: Enumerator[Event])
}


class WaiterActor(db: JdbcBackend#DatabaseDef) extends Actor with Stash {
  val tasks = mutable.Map[Long, StoredWaiterTask]()

  import scala.concurrent.ExecutionContext.Implicits.global

  import WaiterActor._

  private val (waiterOut, waiterChannel) = Concurrent.broadcast[WaiterTaskEvent]

  private def filterByRooms(rooms: Set[String]) = Enumeratee.filter[WaiterTaskEvent](_.matches(rooms))

  private def storedSnapshot: Iterable[WaiterTaskEvent] = tasks.values

  import scala.language.implicitConversions

  implicit def wt2event(x: WaiterTaskEvent): Event =
    x.toEvent

  private val wt2event2: Enumeratee[WaiterTaskEvent, Event] =
    Enumeratee.map { e => e.toEvent}

  override def receive: Receive = {
    case Load => WaiterModel.load(db).onSuccess {
      case loaded => self ! Loaded(loaded)
    }

    case Loaded(newtasks) => {
      newtasks.foreach { task =>
        tasks.put(task.id, task)
      }
      context.become(initialized)
    }
  }

  def initialized: Receive = {
    case NewTask(message, rooms) =>
      WaiterModel.addNewTask(db, message, rooms).map { s =>
        self ! s
        sender ! s
      }

    case task: StoredWaiterTask => {
      tasks.put(task.id, task)
      waiterChannel.push(task)
    }

    case AckTask(id, room) =>
      WaiterModel.markDone(db, id, room).onSuccess {
        case (when) => self ! TaskAcked(id, when, room)
      }

    case TaskAcked(id, when, room) => {
      tasks.get(id).foreach { stored =>
        val newMsg = StoredWaiterTask(stored.id, stored.when, stored.message, stored.rooms, stored.acked.updated(room, when))
        tasks.put(id, newMsg)
        waiterChannel.push(newMsg)
      }
    }

    case Join(rooms) => {
      val r: Enumerator[Event] = Enumerator.enumerate(storedSnapshot).andThen(waiterOut) &> filterByRooms(rooms.toSet) &>
        wt2event2
      sender ! Joined(r)
    }
  }
}