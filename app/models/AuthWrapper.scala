package controllers

import java.security.MessageDigest
import javax.inject.{Inject, Singleton}

import models.{AdminId, Admin, Users}
import play.api.Configuration
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.JdbcProfile

import org.mindrot.jbcrypt.BCrypt

import scala.concurrent.ExecutionContext
import scala.util.Try

/** Legacy unsalted SHA-1. Kept only to verify (and then upgrade) old admins.password values. */
object Hasher {
  private def bytesToString(x: Array[Byte]) = x.map("%02x" format _).mkString

  // MessageDigest is not thread-safe, so don't share one instance.
  def getSha1(x: Array[Byte]): String =
    bytesToString(MessageDigest.getInstance("SHA-1").digest(x))

  def getSha1(x: String): String =
    getSha1(x.getBytes("UTF-8"))
}

/**
  * admins.password holds a bcrypt hash ("$2a$..."); director and cttools read the same column and
  * understand the same two formats. Old 40-hex SHA-1 values are still accepted and are rewritten as
  * bcrypt on the next successful login.
  */
object PasswordHash {
  val Cost = 12

  def hash(password: String): String =
    BCrypt.hashpw(password, BCrypt.gensalt(Cost))

  def isLegacy(stored: String): Boolean = !stored.startsWith("$2")

  def verify(password: String, stored: String): Boolean =
    if (isLegacy(stored))
      MessageDigest.isEqual(Hasher.getSha1(password).getBytes("UTF-8"), stored.toLowerCase.getBytes("UTF-8"))
    else
      Try(BCrypt.checkpw(password, stored)).getOrElse(false)

  // Burn comparable time when the user does not exist.
  private[this] lazy val dummy = hash("dummy")
  def verifyDummy(password: String): Unit = { BCrypt.checkpw(password, dummy); () }

  /** Goes into the session cookie instead of the hash: changing the password still ends old sessions. */
  def fingerprint(stored: String): String =
    MessageDigest.getInstance("SHA-256").digest(stored.getBytes("UTF-8")).take(8).map("%02x" format _).mkString
}
