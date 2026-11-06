package palevo.query

import org.scalatest.funsuite.AnyFunSuite

class MatchingSpec extends AnyFunSuite {
  test("computers: number and optional letter prefix") {
    for (q <- Seq("46", "046", "wi46", "wi 46", "WI46")) assert(Matching.computerMatches(q, "wi46"), q)
    assert(!Matching.computerMatches("wi46", "wi146"))
    assert(!Matching.computerMatches("wi46", "lin46"))
    assert(Matching.computerMatches("46", "lin46"))
    assert(!Matching.computerMatches("46", "wi146"))
    for (q <- Seq("", "!!", "abc")) assert(!Matching.computerMatches(q, "wi46"), q)
    assert(!Matching.computerMatches("46", "wi"))
  }

  private val sgu1 = TeamNames("СГУ", "Саратовский госуниверситет", 1, "Парафраз: Черабаев, Лёвина, Юнусов")
  private val sgu2 = sgu1.copy(num = 2)
  private val chees = TeamNames("MSU", "Moscow", 3, "cheesedom: Lepa, Sibiryakov, Garaev")

  test("teams: school + number") {
    for (q <- Seq("СГУ1", "сгу 1", "CГУ1")) {
      assert(Matching.teamScore(q, sgu1).isDefined, q)
      assert(Matching.teamScore(q, sgu2).isEmpty, q)
    }
  }

  test("teams: names, surnames, yo, latin") {
    for (q <- Seq("парафраз", "Черабаев", "лёвина", "левина")) assert(Matching.teamScore(q, sgu1).isDefined, q)
    for (q <- Seq("cheesedom", "Sibiryakov")) assert(Matching.teamScore(q, chees).isDefined, q)
    assert(Matching.teamScore("zzz", chees).isEmpty)
  }

  test("scores: exact > prefix > substring") {
    assert(Matching.teamScore("левина", sgu1) == Some(3))
    assert(Matching.teamScore("лев", sgu1) == Some(2))
    assert(Matching.teamScore("евин", sgu1) == Some(1))
  }

  test("ranking") {
    val a = TeamNames("X", "x", 1, "Zebra: Ivanov")
    val b = TeamNames("X", "x", 2, "Q: Ivanovsky")
    val c = TeamNames("X", "x", 3, "Q: Petrov-Ivanov")
    assert(Matching.rankTeams("ivanov", Seq(c, b, a))(x => x) == Seq(a, b, c))
    val many = (1 to 9).map(i => TeamNames("S", "s", i, "n: aaa"))
    assert(Matching.rankTeams("aaa", many)(x => x) == many.take(5))
  }

  test("hostile input never throws or matches all") {
    for (q <- Seq("", "%", ".*", "((", "x" * 5000, "😀", "\\", "[")) {
      Matching.computerMatches(q, "wi46")
      assert(Matching.teamScore(q, sgu1).isEmpty, q)
      assert(Matching.rankTeams(q, Seq(sgu1, chees))(x => x).isEmpty, q)
    }
  }

  test("long query is cut to 100 chars") {
    assert(Matching.teamScore("парафраз" + "!" * 200 + "zzz", sgu1).isDefined)
  }
}
