package models

import play.api.libs.json.Json
import slick.jdbc.{GetResult, JdbcBackend}

import scala.concurrent.{Future, ExecutionContext}

case class Area(id: Int, name: String, printer: String)
object Area {
  implicit val format = Json.format[Area]
}

case class Location(id: Long, area: Area, name: String)

object Location {
  implicit val format = Json.format[Location]

  implicit val getResult = GetResult(r =>
    Location(r.nextLong(), Area(r.nextInt, r.nextString(), r.nextString()), r.nextString())
  )
}

object Locator {
  import slick.jdbc.MySQLProfile.api._

  def locate(db: JdbcBackend#DatabaseDef, remoteAddress: String)(implicit ec: ExecutionContext): Future[Option[Location]] =
    db.run(
      sql"""select CompLocations.ID, Areas.ID, Areas.Name, Areas.Printer, CompLocations.Name
            from CompLocations, Areas where Areas.ID = CompLocations.Location and
            CompLocations.ID = INET_ATON(${remoteAddress})""".as[Location]).map(_.headOption)
}