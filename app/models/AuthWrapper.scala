package controllers

import org.mindrot.jbcrypt.BCrypt

import scala.util.Try

/**
  * admins.password_hash is written by teacher-web (golang.org/x/crypto/bcrypt, prefix 2a); jBCrypt verifies
  * the 2a, 2b and 2y prefixes.
  */
object PasswordHash {
  val Cost = 12

  def hash(password: String): String =
    BCrypt.hashpw(password, BCrypt.gensalt(Cost))

  def verify(password: String, stored: String): Boolean =
    stored.startsWith("$2") && Try(BCrypt.checkpw(password, stored)).getOrElse(false)

  // Burn comparable time when the user does not exist.
  private[this] lazy val dummy = hash("dummy")
  def verifyDummy(password: String): Unit = { BCrypt.checkpw(password, dummy); () }

  /** Goes into the session cookie instead of the hash: changing the password still ends old sessions. */
  def fingerprint(stored: String): String =
    java.security.MessageDigest.getInstance("SHA-256").digest(stored.getBytes("UTF-8")).take(8).map("%02x" format _).mkString
}
