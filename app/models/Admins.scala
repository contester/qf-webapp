package models

import slick.jdbc.GetResult

import scala.concurrent.Future
import scala.util.Try

case class AdminId(username: String, passwordHash: String) {
  override def toString = s"$username:$passwordHash"
}

case class Admin(username: String, passwordHash: String, spectator: Set[Int], administrator: Set[Int]) {
  override def toString = s"$username:$passwordHash"

  def toId = AdminId(username, passwordHash)

  def canSpectate(contestId: Int) =
    spectator.contains(contestId) || spectator.contains(-1) || canModify(contestId)

  def canModify(contestId: Int) =
    administrator.contains(contestId) || administrator.contains(-1)
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
    Admin(r.nextString(), r.nextString(), parseAcl(r.nextString()), parseAcl(r.nextString()))
  )

  private def parseSingleAcl(s: String): Option[Int] =
    if (s == "*")
      Some(-1)
    else Try(s.toInt).toOption

  private def parseAcl(s: String): Set[Int] =
    s.split(',').map(parseSingleAcl).flatten.toSet

  def query(username: String, passwordHash: String) =
    sql"""select Username, Password, Spectator, Administrator from admins where Username = $username and Password = $passwordHash""".as[Admin]
}

object AdminPermissions {
  def canModify(contestId: Int)(account: Admin): Future[Boolean] =
    Future.successful(account.canModify(contestId))

  def canSpectate(contestId: Int)(account: Admin): Future[Boolean] =
    Future.successful(account.canSpectate(contestId))
}