package models


import com.github.nscala_time.time.Imports.DateTime
import play.api.libs.EventSource.Event
import play.api.libs.json.Json
import slick.jdbc.{GetResult, JdbcBackend}

import scala.concurrent.{ExecutionContext, Future}

// Model: stored things.

trait WaiterTaskEvent {
  def matches(rooms: Set[String]): Boolean
  def toEvent: Event
}

case class StoredWaiterTask(id: Long, when: DateTime, message: String, rooms: Set[String],
                            acked: Map[String, DateTime]) extends WaiterTaskEvent {
  override def matches(vrooms: Set[String]): Boolean = vrooms.contains("*") || !rooms.intersect(vrooms).isEmpty
  implicit val format = Json.format[StoredWaiterTask]

  override def toEvent: Event = Event(Json.stringify(Json.toJson(this)), None, Some("waiterTask"))
}

object WaiterModel {
  import slick.driver.MySQLDriver.api._
  import utils.Db._

  case class WaiterTaskRecord(id: Long, room: String, ts: DateTime)

  class WaiterTaskRecords(tag: Tag) extends Table[WaiterTaskRecord](tag, "WaiterTaskRecord") {
    def id = column[Long]("ID")
    def room = column[String]("Room")
    def ts = column[DateTime]("TS")

    override def * = (id, room, ts) <> (WaiterTaskRecord.tupled, WaiterTaskRecord.unapply)
  }

  val waiterTaskRecords = TableQuery[WaiterTaskRecords]

  private def maybeMakeRooms(db: JdbcBackend#DatabaseDef, roomsActive: List[String])(implicit ec: ExecutionContext): Future[List[String]] = {
    if (roomsActive.isEmpty) {
      makeRooms(db)
    } else Future.successful(roomsActive)
  }

  private def makeRooms(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[List[String]] = {
    db.run(sql"""select Name from Areas""".as[String]).map(_.toList)
  }

  def addNewTask(db: JdbcBackend#DatabaseDef, message: String, roomsActive: List[String])(implicit ec: ExecutionContext): Future[StoredWaiterTask] =
  maybeMakeRooms(db, roomsActive).flatMap { newRa =>
    val ra = newRa.mkString(",")
    val now = DateTime.now
    db.run(sqlu"""insert into WaiterTasks (TS, Message, Rooms) values ($now, $message, $ra)
                  """.andThen(sql"select last_insert_id()".as[Int]).withPinnedSession).map(_.headOption.get).map { lastInsertId =>
      StoredWaiterTask(lastInsertId, now, message, newRa.toSet, Map.empty)
    }
  }

  def markDone(db: JdbcBackend#DatabaseDef, id: Long, room: String)(implicit ec: ExecutionContext): Future[DateTime] = {
    val now = DateTime.now
    db.run(DBIO.seq(waiterTaskRecords += WaiterTaskRecord(id, room, now))).map(_ => now)
  }

  def unmarkDone(db: JdbcBackend#DatabaseDef, id: Long, room: String)(implicit ec: ExecutionContext): Future[Unit] = {
    db.run(DBIO.seq(waiterTaskRecords.filter(x => x.id === id && x.room === room).delete))
  }

  private def getAllRecords(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[Map[Long, Map[String, DateTime]]] =
    db.run(waiterTaskRecords.result).map { records =>
      records.toList.groupBy(_.id).mapValues(x => x.map(y => y.room -> y.ts).toMap)
    }

  implicit private val getStoredTask = GetResult(r =>
    StoredWaiterTask(r.nextLong(), new DateTime(r.nextTimestamp()), r.nextString(), r.nextString().split(",").toSet, Map.empty)
  )

  private def getAllTasks(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[List[StoredWaiterTask]] = {
    db.run(sql"""select ID, TS, Message, Rooms from WaiterTasks""".as[StoredWaiterTask]).map(_.toList)
  }

  def load(db: JdbcBackend#DatabaseDef)(implicit ec: ExecutionContext): Future[List[StoredWaiterTask]] = {
    getAllRecords(db).zip(getAllTasks(db)).map {
      case (records, tasks) =>
        tasks.map {
          case StoredWaiterTask(id, ts, message, roomsActive, _) =>
            StoredWaiterTask(id, ts, message, roomsActive, records.get(id).getOrElse(Map.empty))
        }
    }
  }
}