package actors

import akka.actor.Actor.Receive
import akka.actor.{Actor, Stash}
import models.{Contest, StoredWaiterTask, WaiterModel, WaiterTaskMessage}
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
  case object Load
  case class Loaded(tasks: List[StoredWaiterTask])

  case class NewTask(message: String, rooms: List[String])
  case class AckTask(id: Long, room: String)
  case class UnackTask(id: Long, room: String)
  case class DeleteTask(id: Long)

  case class TaskAcked(id: Long, when: DateTime, room: String)
  case class TaskDeleted(id: Long) extends WaiterTaskMessage {
    override val roomsActive: Set[String] = Set.empty
  }

  object TaskDeleted {
    implicit val format = Json.format[TaskDeleted]
    implicit val eventNameExtractor = EventNameExtractor[TaskDeleted](_ => Some("waiterTaskDeleted"))
    implicit val eventDataExtractor = EventDataExtractor[TaskDeleted] { state =>
      Json.stringify(Json.toJson(state))
    }
  }

  case class Join(rooms: List[String])
  case class Joined(enum: Enumerator[Event])

  case class WaiterTaskEventPayload(when: DateTime, message: String, roomsActive: List[String], roomsAcked: List[String])

  case class WaiterTaskEventMessage()
}


class WaiterActor(db: JdbcBackend#DatabaseDef) extends Actor with Stash {
  val tasks = mutable.Map[Long, StoredWaiterTask]()

  import scala.concurrent.ExecutionContext.Implicits.global

  import WaiterActor._

  private val (waiterOut, waiterChannel) = Concurrent.broadcast[WaiterTaskMessage]

  private def filterByRooms(rooms: Set[String]) = Enumeratee.filter[WaiterTaskMessage](_.matches(rooms))

  private def storedSnapshot: Iterable[WaiterTaskMessage] = tasks.values

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
        val newMsg = StoredWaiterTask(stored.id, stored.when, stored.message, stored.roomsActive, stored.roomsAcked.updated(room, when))
        tasks.put(id, newMsg)
        waiterChannel.push(newMsg)
      }
    }

    case Join(rooms) => {
      val r = Enumerator.enumerate(storedSnapshot).andThen(waiterOut) &> filterByRooms(rooms.toSet) &> EventSource()
      sender ! Joined(r)
    }

    case UnackTask(id, rooms) =>

  }
}