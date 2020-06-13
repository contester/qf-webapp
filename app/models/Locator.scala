package models

import inet.ipaddr.IPAddressString
import inet.ipaddr.ipv4.IPv4Address
import play.api.libs.json.Json
import slick.jdbc.{GetResult, JdbcBackend}

import scala.concurrent.{ExecutionContext, Future}

case class Area(id: Int, name: String, printer: String)
case class Location(id: IPv4Address, area: Area, name: String)

object Locator {
  def locate(db: JdbcBackend#DatabaseDef, remoteAddress: String)(implicit ec: ExecutionContext): Future[Option[Location]] = {
    import utils.MyPostgresProfile.api._
    import SlickModel._

    val cAddr = new IPAddressString(remoteAddress).getAddress.toIPv4

    val q = (for {
      c <- compLocations if c.id === cAddr
      a <- areas if a.id === c.location
    } yield (c.id, a.id, a.name, a.printer, c.name)).take(1)

    db.run(q.result.headOption).map(_.map { x =>
      Location(x._1, Area(x._2, x._3, x._4), x._5)
    })
  }
}