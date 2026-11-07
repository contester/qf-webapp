package palevo.query

import play.api.libs.json.{JsObject, Json}

/**
  * How `/computers` and `/printing` rows are shown to an audience. Both endpoints answer the public too, minus what
  * only helps someone poking at the network: a print agent's name and a computer's raw address. The jury sees the
  * rows as the source gives them.
  */
object AudienceView {
  private val IPv4 = """\d{1,3}(\.\d{1,3}){3}(/\d{1,2})?""".r.pattern

  /** An IPv4 address (with an optional prefix length), or anything with a colon (IPv6, with or without a zone).
    * Computer names in the location table have neither form. */
  def looksLikeAddress(s: String): Boolean = {
    val t = s.trim
    IPv4.matcher(t).matches || t.contains(':')
  }

  /** A print job. For the public, a computer given as an address (one missing from the location table) is "". */
  def printJob(j: PrintJobRow, audience: Audience): JsObject = {
    val computer = if (audience == Public && looksLikeAddress(j.computer)) "" else j.computer
    Json.obj("id" -> j.id, "team" -> j.team, "computer" -> computer, "area" -> j.area, "printed" -> j.printed,
      "error" -> j.error, "pages" -> j.pages, "arrived" -> QueryJson.iso(j.arrived))
  }

  /** A printer problem: `printer` and `condition`, and for the jury also the reporting `agent`. */
  def printer(p: PrinterProblem, audience: Audience): JsObject = {
    val shown = Json.obj("printer" -> p.printer, "condition" -> p.condition)
    audience match {
      case Jury => Json.obj("agent" -> p.agent) ++ shown
      case Public => shown
    }
  }

  /** A computer: `name`, `area` and `printer`, the same for both audiences (the row carries no address). */
  def computer(c: ComputerRow): JsObject = Json.obj("name" -> c.name, "area" -> c.area, "printer" -> c.printer)
}
