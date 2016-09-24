package models

import controllers.routes
import slick.jdbc.{GetResult, JdbcBackend}
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class AdminId(username: String, passwordHash: String) {
  override def toString = s"$username:$passwordHash"
}

trait WaiterPermissions {
  def canCreateTasks: Boolean
  def filter(room: String): Boolean
}

case class Admin(username: String, passwordHash: String, spectator: Set[Int], administrator: Set[Int],
                 locations: Set[String]) extends WaiterPermissions {
  override def toString = s"$username:$passwordHash"

  def toId = AdminId(username, passwordHash)

  def canSpectate(contestId: Int) =
    spectator.contains(contestId) || spectator.contains(-1) || canModify(contestId)

  def canModify(contestId: Int) =
    administrator.contains(contestId) || administrator.contains(-1)

  val canCreateTasks =
    locations.contains("*")

  override def filter(room: String): Boolean =
    canCreateTasks || locations.contains(room)
}

object AdminId {
  def fromString(s: String) = {
    val splits = s.split(':')
    if (splits.length == 2)
      Some(AdminId(splits(0), splits(1)))
    else
      None
  }
}

object Admin {
  import slick.driver.MySQLDriver.api._

  private def parseSingleAcl(s: String): Option[Int] =
    if (s == "*")
      Some(-1)
    else Try(s.toInt).toOption

  private def parseAcl(s: String): Set[Int] =
    s.split(',').map(parseSingleAcl).flatten.toSet

  private def parseStringAcl(s: String): Set[String] =
    s.split(',').toSet

  private def buildAdmin(x: AdminModel.AdminEntry): Admin =
    Admin(x.username, x.password, parseAcl(x.spectator), parseAcl(x.administrator), parseStringAcl(x.locations))

  def query(db: JdbcBackend#DatabaseDef, username: String, passwordHash: String)(implicit ec: ExecutionContext) =
    db.run(AdminModel.admins.filter(x => x.username === username && x.password === passwordHash).take(1).result)
      .map(_.headOption.map(buildAdmin))
}

object AdminModel {
  import slick.driver.MySQLDriver.api._
  import utils.Db._

  case class AdminEntry(username: String, password: String, spectator: String, administrator: String, locations: String)

  case class Admins(tag: Tag) extends Table[AdminEntry](tag, "admins") {
    def username = column[String]("Username", O.PrimaryKey)
    def password = column[String]("Password")
    def spectator = column[String]("Spectator")
    def administrator = column[String]("Administrator")
    def locations = column[String]("Locations")

    override def * = (username, password, spectator, administrator, locations) <> (AdminEntry.tupled, AdminEntry.unapply)
  }

  val admins = TableQuery[Admins]
}

object AdminPermissions {
  def canModify(contestId: Int)(account: Admin): Future[Boolean] =
    Future.successful(account.canModify(contestId))

  def canSpectate(contestId: Int)(account: Admin): Future[Boolean] =
    Future.successful(account.canSpectate(contestId))

  def canCreateTasks(account: Admin): Future[Boolean] =
    Future.successful(account.canCreateTasks)
}

object AdminNavlinkMatch {
  def apply(tab: String): Function[Int, play.api.mvc.Call] =
    tab match {
      case "status" => routes.AdminApplication.submits(_)
      case "monitor" => routes.AdminApplication.monitor(_)
      case "qanda" => routes.AdminApplication.showQandA(_)
      case "rejudge" => routes.AdminApplication.rejudgePage(_)
      case "tasks" => routes.AdminApplication.tasks(_)
      case _ => routes.AdminApplication.submits(_)
    }
}