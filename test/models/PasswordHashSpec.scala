package models

import controllers.PasswordHash
import org.scalatest.funsuite.AnyFunSuite

class PasswordHashSpec extends AnyFunSuite {
  // Hashes made by jBCrypt (this app) and by golang.org/x/crypto/bcrypt (director, cttools) for the
  // same password; every consumer of admins.password must accept both.
  private val password = "pässword-secret"
  private val javaHash = "$2a$10$KLu.cero5TzabuRpTyK3ae9rMdWM0c/xsATL5lDzxAVLPirtyGqYm"
  private val goHash = "$2a$10$1TpQT4UJBzoHaqdDUpaNx.DbVqs6k0wMBnuruq2CijmsX644bCCH2"

  test("hash round-trips and is salted") {
    val a = PasswordHash.hash("hunter2")
    val b = PasswordHash.hash("hunter2")
    assert(a != b)
    assert(PasswordHash.verify("hunter2", a) && PasswordHash.verify("hunter2", b))
    assert(!PasswordHash.verify("hunter3", a))
  }

  test("accepts hashes produced by Java and by Go") {
    assert(PasswordHash.verify(password, javaHash))
    assert(PasswordHash.verify(password, goHash))
    assert(!PasswordHash.verify("wrong", goHash))
  }

  test("a Go bcrypt hash of cost 12 (teacher-web) verifies") {
    val seed = "$2a$12$LAgcv5L5iIfgMlMmLuaHNO4wpaeZB5T7TaSPOZTJFKNANrrMLVFmC"
    assert(PasswordHash.verify("secret", seed))
    assert(!PasswordHash.verify("Secret", seed))
  }

  test("a SHA-1 value or other non-bcrypt text never verifies") {
    assert(!PasswordHash.verify("secret", "e5e9fa1ba31ecd1ae84f75caaa474f3a663f05f4"))
  }

  test("garbage stored values never verify") {
    assert(!PasswordHash.verify("x", ""))
    assert(!PasswordHash.verify("x", "$2a$broken"))
  }

  test("fingerprint changes with the hash and does not leak it") {
    val f = PasswordHash.fingerprint(javaHash)
    assert(f != PasswordHash.fingerprint(goHash))
    assert(!javaHash.contains(f))
  }

  test("session key round-trips, including usernames with a colon") {
    val id = AdminId("a:b", "0123456789abcdef")
    assert(AdminId.fromString(id.toString).contains(id))
    assert(AdminId.fromString("nocolon").isEmpty)
    assert(AdminId.fromString("user:").isEmpty)
  }
}
