package palevo.query

import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.funsuite.AnyFunSuite
import play.api.libs.json.Json

class AudienceViewSpec extends AnyFunSuite {
  private val at = new DateTime(2026, 11, 6, 12, 0, 0, DateTimeZone.UTC)
  private def job(computer: String, area: String = "") =
    PrintJobRow(7, 1, 11, computer, area, printed = false, error = "jam", pages = 2, arrived = at)

  test("addresses are recognised, computer names are not") {
    for (a <- Seq("10.0.0.5", "192.168.1.46", " 10.0.0.5 ", "10.0.0.5/32", "fe80::1", "::1", "2001:db8::7",
      "fe80::1%eth0", "::ffff:10.0.0.5"))
      assert(AudienceView.looksLikeAddress(a), a)
    for (n <- Seq("wi46", "lin01", "", "WI-46", "pc46.local", "46"))
      assert(!AudienceView.looksLikeAddress(n), n)
  }

  test("print job: the public loses a raw address, keeps everything else") {
    val pub = AudienceView.printJob(job("10.0.0.5"), Public)
    assert(pub == Json.obj("id" -> 7, "team" -> 11, "computer" -> "", "area" -> "", "printed" -> false,
      "error" -> "jam", "pages" -> 2, "arrived" -> "2026-11-06T12:00:00Z"))
    assert((AudienceView.printJob(job("fe80::1"), Public) \ "computer").as[String] == "")
    assert((AudienceView.printJob(job("wi46", "Windows room"), Public) \ "computer").as[String] == "wi46")
  }

  test("print job: the jury sees the job as the source gives it") {
    assert((AudienceView.printJob(job("10.0.0.5"), Jury) \ "computer").as[String] == "10.0.0.5")
    assert(AudienceView.printJob(job("wi46"), Jury) == AudienceView.printJob(job("wi46"), Public))
  }

  test("printer problem: agent only for the jury") {
    val p = PrinterProblem("agent1", "HP-1", "paper out")
    assert(AudienceView.printer(p, Public) == Json.obj("printer" -> "HP-1", "condition" -> "paper out"))
    assert(AudienceView.printer(p, Jury) == Json.obj("agent" -> "agent1", "printer" -> "HP-1", "condition" -> "paper out"))
  }

  test("computer row: name, area and printer for everyone") {
    assert(AudienceView.computer(ComputerRow("wi46", "Windows room", "HP-1")) ==
      Json.obj("name" -> "wi46", "area" -> "Windows room", "printer" -> "HP-1"))
  }
}
