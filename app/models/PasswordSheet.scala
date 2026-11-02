package models

import java.io.ByteArrayOutputStream

import org.apache.pdfbox.pdmodel.{PDDocument, PDPage, PDPageContentStream}
import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.pdmodel.font.PDType0Font

/** One password slip. `name` is teams.name: "ShortName: Surnames" or "Surnames". */
case class PasswordCard(contestName: String, school: String, teamNum: Int, name: String, login: String, password: String)

/**
 * Renders password slips as a PDF. Pagination is fixed: a page holds exactly
 * [[PasswordSheet.CardsPerPage]] cards at fixed positions with whitespace between them,
 * so a card is never split between pages and the slips can be cut apart by hand.
 */
object PasswordSheet {
  private val Mm = 72f / 25.4f

  val CardsPerPage = 9
  val Website = "qf.sgu.ru"
  private val Margin = 10 * Mm
  private val CardHeight = 26 * Mm
  private val Gap = 5 * Mm
  private val Padding = 3 * Mm

  /** "School #N", or just "School" when the school has a single team among `all`. */
  def schoolLine(card: PasswordCard, all: Seq[PasswordCard]): String =
    if (card.teamNum == 0 || all.count(c => c.school == card.school && c.contestName == card.contestName) < 2) card.school
    else s"${card.school} #${card.teamNum}"

  /** A card together with the ids it can be filtered by. */
  case class Row(teamId: Int, schoolId: Int, card: PasswordCard)

  private val ServiceSchools = Set(InitialImportTools.JurySchoolFullName, InitialImportTools.CoachSchoolFullName)

  /**
   * The cards to print. Without a filter that is everyone except the jury and coach teams;
   * asking for a school or a team explicitly prints it even if it is a jury or coach one.
   */
  def select(rows: Seq[Row], school: Option[Int], team: Option[Int]): Seq[PasswordCard] =
    rows.filter { r =>
      if (school.isEmpty && team.isEmpty) !ServiceSchools.contains(r.card.school)
      else school.forall(_ == r.schoolId) && team.forall(_ == r.teamId)
    }.map(_.card)

  def sorted(cards: Seq[PasswordCard]): Seq[PasswordCard] =
    cards.sortBy(c => (c.school, c.teamNum, c.login))

  /**
   * `all` is every card of the contest, used to decide whether a school has several teams;
   * `selected` is what gets printed (e.g. a single team).
   */
  def render(selected: Seq[PasswordCard], all: Seq[PasswordCard]): Array[Byte] = {
    val doc = new PDDocument()
    try {
      val regular = load(doc, "fonts/LiberationSans-Regular.ttf")
      val bold = load(doc, "fonts/LiberationSans-Bold.ttf")
      val page = PDRectangle.A4

      sorted(selected).grouped(CardsPerPage).foreach { pageCards =>
        val p = new PDPage(page)
        doc.addPage(p)
        val cs = new PDPageContentStream(doc, p)
        try {
          pageCards.zipWithIndex.foreach { case (card, i) =>
            val top = page.getHeight - Margin - i * (CardHeight + Gap)
            drawCard(cs, regular, bold, page.getWidth, top, card, schoolLine(card, all))
          }
        } finally cs.close()
      }
      if (doc.getNumberOfPages == 0) doc.addPage(new PDPage(page))

      val out = new ByteArrayOutputStream()
      doc.save(out)
      out.toByteArray
    } finally doc.close()
  }

  private def load(doc: PDDocument, resource: String): PDType0Font = {
    val in = getClass.getClassLoader.getResourceAsStream(resource)
    require(in != null, s"missing font resource $resource")
    try PDType0Font.load(doc, in) finally in.close()
  }

  private def drawCard(cs: PDPageContentStream, regular: PDType0Font, bold: PDType0Font,
                       pageWidth: Float, top: Float, card: PasswordCard, school: String): Unit = {
    val left = Margin
    val cardWidth = pageWidth - 2 * Margin
    val textWidth = cardWidth - 2 * Padding

    cs.setLineWidth(0.5f)
    cs.setStrokingColor(0.6f, 0.6f, 0.6f)
    cs.setLineDashPattern(Array(3f, 3f), 0)
    cs.addRect(left, top - CardHeight, cardWidth, CardHeight)
    cs.stroke()

    var y = top - Padding
    def line(font: PDType0Font, size: Float, text: String): Unit = {
      y -= size
      cs.beginText()
      cs.setFont(font, size)
      cs.newLineAtOffset(left + Padding, y)
      cs.showText(text)
      cs.endText()
      y -= size * 0.25f
    }

    line(regular, 7.5f, fit(card.contestName, regular, 7.5f, 6f, textWidth, 1)._2.head)
    val (schoolSize, schoolLines) = fit(school, regular, 12f, 8f, textWidth, 1)
    schoolLines.foreach(line(regular, schoolSize, _))
    val (nameSize, nameLines) = fit(card.name, regular, 10f, 7f, textWidth, 2)
    nameLines.foreach(line(regular, nameSize, _))

    // site, login and password; the values are bold
    val pieces = Seq(
      regular -> "Website: ", bold -> Website,
      regular -> "   User name: ", bold -> card.login,
      regular -> "   Password: ", bold -> card.password
    ).map { case (f, t) => f -> sanitize(t, f) }
    val total = pieces.map { case (f, t) => width(f, t, 11f) }.sum
    val size = if (total <= textWidth) 11f else 11f * textWidth / total
    y -= 2f + size
    var x = left + Padding
    pieces.foreach { case (font, text) =>
      cs.beginText()
      cs.setFont(font, size)
      cs.newLineAtOffset(x, y)
      cs.showText(text)
      cs.endText()
      x += width(font, text, size)
    }
  }

  private def width(font: PDType0Font, text: String, size: Float): Float =
    font.getStringWidth(text) / 1000f * size

  /** Replaces control characters and characters the font has no glyph for. */
  private[models] def sanitize(text: String, font: PDType0Font): String =
    text.codePoints().toArray.map { cp =>
      val ok = !Character.isISOControl(cp) && font.hasGlyph(cp)
      new String(Character.toChars(if (ok) cp else '?'.toInt))
    }.mkString

  /** Greedy word wrap; a word longer than the width is broken by characters. */
  private[models] def wrap(text: String, font: PDType0Font, size: Float, maxWidth: Float): Seq[String] = {
    val lines = Seq.newBuilder[String]
    var cur = ""
    def fits(s: String) = width(font, s, size) <= maxWidth
    text.split(' ').filter(_.nonEmpty).foreach { word =>
      var w = word
      val candidate = if (cur.isEmpty) w else s"$cur $w"
      if (fits(candidate)) cur = candidate
      else {
        if (cur.nonEmpty) { lines += cur; cur = "" }
        while (!fits(w)) {
          val n = (1 to w.length).takeWhile(k => fits(w.take(k))).lastOption.getOrElse(1)
          lines += w.take(n)
          w = w.drop(n)
        }
        cur = w
      }
    }
    if (cur.nonEmpty) lines += cur
    val r = lines.result()
    if (r.isEmpty) Seq("") else r
  }

  /**
   * Largest font size in [minSize, maxSize] at which `text` wraps into at most `maxLines`
   * lines. At minSize, anything beyond maxLines is cut off with an ellipsis.
   */
  private[models] def fit(text: String, font: PDType0Font, maxSize: Float, minSize: Float,
                          maxWidth: Float, maxLines: Int): (Float, Seq[String]) = {
    val clean = sanitize(text, font)
    var size = maxSize
    while (size > minSize && wrap(clean, font, size, maxWidth).size > maxLines) size -= 0.5f
    val lines = wrap(clean, font, size, maxWidth)
    if (lines.size <= maxLines) (size, lines)
    else {
      var last = lines(maxLines - 1)
      while (last.nonEmpty && width(font, last + "…", size) > maxWidth) last = last.init
      (size, lines.take(maxLines - 1) :+ (last + "…"))
    }
  }
}
