package palevo.query

import org.scalatest.funsuite.AnyFunSuite

class ScopeSpec extends AnyFunSuite {
  test("public audience with all contests") {
    assert(Scope.parse(Some("public"), Some("*")) == Right(Scope(Public, None)))
  }

  test("audience all with a comma list of contests") {
    assert(Scope.parse(Some("all"), Some("3,5")) == Right(Scope(Jury, Some(Set(3, 5)))))
  }

  test("a single contest id") {
    assert(Scope.parse(Some("public"), Some("7")) == Right(Scope(Public, Some(Set(7)))))
  }

  test("missing or unknown audience is an error") {
    assert(Scope.parse(None, Some("*")).isLeft)
    assert(Scope.parse(Some("jury"), Some("*")).isLeft)
    assert(Scope.parse(Some(""), Some("*")).isLeft)
    assert(Scope.parse(Some("PUBLIC"), Some("*")).isLeft)
  }

  test("missing, empty or malformed contests is an error") {
    assert(Scope.parse(Some("public"), None).isLeft)
    assert(Scope.parse(Some("public"), Some("")).isLeft)
    assert(Scope.parse(Some("public"), Some("a,b")).isLeft)
    assert(Scope.parse(Some("public"), Some("1,,2")).isLeft)
    assert(Scope.parse(Some("public"), Some("1,")).isLeft)
    assert(Scope.parse(Some("public"), Some("*,1")).isLeft)
    assert(Scope.parse(Some("public"), Some("99999999999")).isLeft)
  }

  test("errors carry a short text") {
    assert(Scope.parse(None, Some("*")).left.exists(_.nonEmpty))
    assert(Scope.parse(Some("public"), Some("a")).left.exists(_.nonEmpty))
  }
}
