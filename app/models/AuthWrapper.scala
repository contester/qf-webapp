package controllers

import java.security.MessageDigest
import javax.inject.{Inject, Singleton}

import models.{Admin, Users}
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile

import scala.concurrent.ExecutionContext

object Hasher {
  val hasher = MessageDigest.getInstance("SHA-1")

  def bytesToString(x: Array[Byte]) = x.map("%02X" format _).mkString

  def getSha1(x: Array[Byte]): String =
    bytesToString(hasher.digest(x)).toLowerCase

  def getSha1(x: String): String =
    getSha1(x.getBytes)
}

@Singleton
class AuthWrapper @Inject() (dbConfigProvider: DatabaseConfigProvider) {
  val db = dbConfigProvider.get[JdbcProfile].db

  def resolve(username: String)(implicit ctx: ExecutionContext) =
    db.run(Users.resolveQuery(username)).map(_.headOption)

  def resolveAdmin(admin: Admin)(implicit ctx: ExecutionContext) =
    db.run(Admin.query(admin.username, admin.passwordHash)).map(_.headOption)

  def authAdmin(username: String, password: String)(implicit ctx: ExecutionContext) =
    resolveAdmin(Admin(username, Hasher.getSha1(password)))
}