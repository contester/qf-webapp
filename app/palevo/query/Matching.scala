package palevo.query

case class TeamNames(schoolShort: String, schoolFull: String, num: Int, teamName: String)

/** Pure, plain-string name matching. Never builds a regex from user input. */
object Matching {
  private val MaxQuery = 100
  private val MaxResults = 5

  // Latin look-alikes folded to Cyrillic; applied to both queries and stored names.
  private val fold: Map[Int, Int] =
    "aceopxykmhtb".zip("асеорхукмнтв").map { case (l, c) => l.toInt -> c.toInt }.toMap + ('ё'.toInt -> 'е'.toInt)

  def normalize(s: String): String = {
    val sb = new java.lang.StringBuilder
    s.toLowerCase.codePoints.forEach { cp =>
      if (Character.isLetterOrDigit(cp)) sb.appendCodePoint(fold.getOrElse(cp, cp))
    }
    sb.toString
  }

  private def cut(q: String): String = if (q.length > MaxQuery) q.substring(0, MaxQuery) else q

  private def stripZeros(d: String): String = {
    val t = d.dropWhile(_ == '0')
    if (t.isEmpty && d.nonEmpty) "0" else t
  }

  def computerMatches(query: String, computerName: String): Boolean = {
    val q = normalize(cut(query))
    val qDigits = q.filter(_.isDigit)
    if (qDigits.isEmpty) false
    else {
      val qLetters = q.filterNot(_.isDigit)
      val n = normalize(computerName)
      val nDigits = n.reverse.takeWhile(_.isDigit).reverse
      val nLetters = n.dropRight(nDigits.length)
      nDigits.nonEmpty && stripZeros(nDigits) == stripZeros(qDigits) && nLetters.startsWith(qLetters)
    }
  }

  private def fields(t: TeamNames): Seq[String] = {
    val i = t.teamName.indexOf(':')
    val (self, rest) = if (i < 0) (t.teamName, "") else (t.teamName.substring(0, i), t.teamName.substring(i + 1))
    val school = normalize(t.schoolShort)
    (Seq(school + t.num, school, normalize(self)) ++ rest.split(',').toSeq.map(normalize)).filter(_.nonEmpty)
  }

  def teamScore(query: String, t: TeamNames): Option[Int] = {
    val q = normalize(cut(query))
    if (q.isEmpty) None
    else {
      val scores = fields(t).map { f =>
        if (f == q) 3 else if (f.startsWith(q)) 2 else if (f.contains(q)) 1 else 0
      }
      val best = if (scores.isEmpty) 0 else scores.max
      if (best > 0) Some(best) else None
    }
  }

  def rankTeams[A](query: String, items: Seq[A])(names: A => TeamNames): Seq[A] =
    items.zipWithIndex
      .flatMap { case (a, i) => teamScore(query, names(a)).map(s => (a, s, i)) }
      .sortBy { case (_, s, i) => (-s, i) }
      .take(MaxResults)
      .map(_._1)
}
