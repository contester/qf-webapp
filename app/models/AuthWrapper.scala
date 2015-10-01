package controllers

import java.security.MessageDigest
import javax.inject.{Inject, Singleton}

import models.{AdminId, Admin, Users}
import play.api.Configuration
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile

import scala.concurrent.ExecutionContext

object Hasher {
  private val hasher = MessageDigest.getInstance("SHA-1")

  def bytesToString(x: Array[Byte]) = x.map("%02X" format _).mkString

  def getSha1(x: Array[Byte]): String =
    bytesToString(hasher.digest(x)).toLowerCase

  def getSha1(x: String): String =
    getSha1(x.getBytes)
}

@Singleton
class AuthWrapper @Inject() (dbConfigProvider: DatabaseConfigProvider, configuration: Configuration) {
  val db = dbConfigProvider.get[JdbcProfile].db

  def resolve(username: String)(implicit ctx: ExecutionContext) =
    db.run(Users.resolveQuery(username)).map(_.headOption)

  def authenticateUser(username: String, password: String)(implicit ctx: ExecutionContext) =
    if (configuration.getBoolean("qfauth.users").getOrElse(true))
      Users.authenticate(db, username, password)
    else
      resolve(username)

  def resolveAdmin(admin: AdminId)(implicit ctx: ExecutionContext) =
    db.run(Admin.query(admin.username, admin.passwordHash)).map(_.headOption)

  def authAdmin(username: String, password: String)(implicit ctx: ExecutionContext) =
    resolveAdmin(AdminId(username, Hasher.getSha1(password)))
}