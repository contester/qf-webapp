package models

import slick.jdbc.GetResult

case class Admin(username: String, passwordHash: String) {
  override def toString = s"$username:$passwordHash"
}

object Admin {
  import slick.driver.MySQLDriver.api._

  implicit private val getAdmin = GetResult(r =>
    Admin(r.nextString(), r.nextString())
  )

  def query(username: String, passwordHash: String) =
    sql"""select Username, Password from admins where Username = $username and Password = $passwordHash""".as[Admin]

  def fromString(s: String) = {
    val splits = s.split(':')
    if (splits.length == 2)
      Some(Admin(splits(0), splits(1)))
    else
      None
  }
}