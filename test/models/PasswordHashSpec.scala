package models

import controllers.{Hasher, PasswordHash}
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
    assert(!PasswordHash.isLegacy(a))
  }

  test("accepts hashes produced by Java and by Go") {
    assert(PasswordHash.verify(password, javaHash))
    assert(PasswordHash.verify(password, goHash))
    assert(!PasswordHash.verify("wrong", goHash))
  }

  test("legacy sha1 still verifies, in either hex case") {
    val legacy = "e5e9fa1ba31ecd1ae84f75caaa474f3a663f05f4"
    assert(Hasher.getSha1("secret") == legacy)
    assert(PasswordHash.isLegacy(legacy))
    assert(PasswordHash.verify("secret", legacy))
    assert(PasswordHash.verify("secret", legacy.toUpperCase))
    assert(!PasswordHash.verify("Secret", legacy))
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
