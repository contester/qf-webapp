package models

import controllers.routes
import slick.jdbc.GetResult

import scala.concurrent.Future
import scala.util.Try

case class AdminId(username: String, passwordHash: String) {
  override def toString = s"$username:$passwordHash"
}

case class Admin(username: String, passwordHash: String, spectator: Set[Int], administrator: Set[Int],
                 locations: Set[String]) {
  override def toString = s"$username:$passwordHash"

  def toId = AdminId(username, passwordHash)

  def canSpectate(contestId: Int) =
    spectator.contains(contestId) || spectator.contains(-1) || canModify(contestId)

  def canModify(contestId: Int) =
    administrator.contains(contestId) || administrator.contains(-1)

  def canCreateTasks =
    locations.contains("*")
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

  implicit private val getAdmin = GetResult(r =>
    Admin(r.nextString(), r.nextString(), parseAcl(r.nextString()), parseAcl(r.nextString()), parseStringAcl(r.nextString()))
  )

  private def parseSingleAcl(s: String): Option[Int] =
    if (s == "*")
      Some(-1)
    else Try(s.toInt).toOption

  private def parseAcl(s: String): Set[Int] =
    s.split(',').map(parseSingleAcl).flatten.toSet

  private def parseStringAcl(s: String): Set[String] =
    s.split(',').toSet

  def query(username: String, passwordHash: String) =
    sql"""select Username, Password, Spectator, Administrator, Locations from admins where Username = $username and Password = $passwordHash""".as[Admin]
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
      case _ => routes.AdminApplication.submits(_)
    }
}