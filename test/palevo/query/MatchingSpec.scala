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

  test("latin o folds to cyrillic o") {
    val t = TeamNames("МГУ", "x", 1, "Мoсква: Иванов")
    assert(Matching.normalize("o") == "о")
    assert(Matching.teamScore("Мoсква", t).isDefined) // Latin o in query
    assert(Matching.teamScore("Москва", t) == Matching.teamScore("Мoсква", t))
    assert(Matching.teamScore("Москва", TeamNames("МГУ", "x", 1, "Q: Мoсква")).isDefined)
    assert(Matching.computerMatches("лo46", "ло46"))
    assert(Matching.computerMatches("ло46", "лo46"))
    assert(Matching.teamScore("cheesedom", chees).isDefined)
    assert(Matching.teamScore("Sibiryakov", chees).isDefined)
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

  test("rankTeamsScored carries the scores, best first, and agrees with rankTeams") {
    val s1 = TeamNames("СГУ", "x", 1, "A")
    val s10 = TeamNames("СГУ", "x", 10, "B")
    val s11 = TeamNames("СГУ", "x", 11, "C")
    val s2 = TeamNames("СГУ", "x", 2, "D")
    val items = Seq(s10, s2, s11, s1)
    assert(Matching.rankTeamsScored("СГУ1", items)(x => x) == Seq(s1 -> 3, s10 -> 2, s11 -> 2))
    assert(Matching.rankTeams("СГУ1", items)(x => x) == Seq(s1, s10, s11))
    assert(Matching.rankTeamsScored("СГУ", items)(x => x).map(_._2) == Seq(3, 3, 3, 3))
    assert(Matching.rankTeamsScored("zzz", items)(x => x).isEmpty)
    val many = (1 to 9).map(i => TeamNames("S", "s", i, "n: aaa"))
    assert(Matching.rankTeamsScored("aaa", many)(x => x) == many.take(5).map(_ -> 3))
  }

  test("normalize: composed and decomposed letters are equal") {
    val composed = "й" // й
    val decomposed = "й" // и + combining breve
    assert(composed != decomposed)
    assert(Matching.normalize(decomposed) == Matching.normalize(composed))
    assert(Matching.normalize(decomposed) == "й")
    val t = TeamNames("МГУ", "x", 1, "Q: Зуй")
    assert(Matching.teamScore("Зу" + decomposed, t) == Some(3))
    assert(Matching.normalize("Ё") == Matching.normalize("е"))
  }

  test("normalize does not depend on the default locale") {
    val saved = java.util.Locale.getDefault
    try {
      java.util.Locale.setDefault(java.util.Locale.forLanguageTag("tr"))
      assert(Matching.normalize("WI46") == Matching.normalize("wi46"))
      assert(Matching.computerMatches("WI46", "wi46"))
    } finally java.util.Locale.setDefault(saved)
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
