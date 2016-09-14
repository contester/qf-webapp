package models

import slick.jdbc.JdbcBackend

import scala.concurrent.{ExecutionContext, Future}
import com.github.nscala_time.time.Imports.DateTime
import play.api.libs.EventSource.{EventDataExtractor, EventNameExtractor}
import play.api.libs.json.Json

abstract trait WaiterTaskMessage {
  def id: Int
}

case class StoredWaiterTask(id: Int, when: com.github.nscala_time.time.Imports.DateTime,
                            message: String, roomsActive: Set[String], roomsAcked: Set[String]) extends WaiterTaskMessage

object StoredWaiterTask {
  implicit val format = Json.format[StoredWaiterTask]
  implicit val eventNameExtractor = EventNameExtractor[StoredWaiterTask](_ => Some("waiterTask"))
  implicit val eventDataExtractor = EventDataExtractor[StoredWaiterTask] { state =>
    Json.stringify(Json.toJson(state))
  }
}

object WaiterModel {
  import slick.driver.MySQLDriver.api._

  def maybeMakeRooms(db: JdbcBackend#DatabaseDef, roomsActive: Set[String])(implicit ec: ExecutionContext): Future[Set[String]] = {
    if (roomsActive.isEmpty) {
      makeRooms(db)
    } else Future.successful(roomsActive)
  }

  def makeRooms(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[List[String]] = {
    db.run(sql"""select Name from Areas""".as[String]).map(_.toList)
  }

  def addNewTask(db: JdbcBackend#DatabaseDef, message: String, roomsActive: Set[String])(implicit ec: ExecutionContext): Future[StoredWaiterTask] =
  maybeMakeRooms(db, roomsActive).flatMap { newRa =>
    val ra = newRa.mkString(",")
    val now = DateTime.now
    db.run(sqlu"""insert into WaiterTasks (TS, Message, Rooms) values ($now, $message, $ra)
                  """.andThen(sql"select last_insert_id()".as[Int]).withPinnedSession).map(_.headOption.get).map { lastInsertId =>
      StoredWaiterTask(lastInsertId, now, message, newRa.toSet, Set.empty)
    }
  }

  def markDone(db: JdbcBackend#DatabaseDef, id: Int, room: String)(implicit ec: ExecutionContext): Future[com.github.nscala_time.time.Imports.DateTime] = {
    val now = DateTime.now
    db.run(sqlu"""insert into WaiterTasksRecord (ID, Room, TS) values ($id, $room, $now)""").map(_ => now)
  }

  def getAllRecords(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Map[Int, Set[String]]] =
    db.run(sql"""select ID, Room from WaiterTaskRecord""".as[(Int, String)]).map { records =>
      records.toList.groupBy(_._1).mapValues(x => x.map(_._2).toSet)
    }

  def getAllTasks(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[List[StoredWaiterTask]] =
    db.run(sql"""select ID, TS, Message, Rooms from WaiterTasks""".as[(Int, DateTime, String, String)]).map { records =>
      records.map {
        case (id, ts, message, roomStr) =>
          val rooms = roomStr.split(",").toSet
          StoredWaiterTask(id, ts, message, rooms, Set.empty)
      }.toList
    }

  def load(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[List[StoredWaiterTask]] = {
    getAllRecords(db).zip(getAllTasks(db)).map {
      case (records, tasks) =>
        tasks.map {
          case StoredWaiterTask(id, ts, message, roomsActive, _) =>
            StoredWaiterTask(id, ts, message, roomsActive, records.get(id).getOrElse(Set.empty))
        }
    }
  }
}