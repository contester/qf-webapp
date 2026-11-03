package models

import java.nio.file.{Files, Paths}

import org.apache.pdfbox.Loader
import org.apache.pdfbox.text.PDFTextStripper
import org.scalatest.funsuite.AnyFunSuite

class PasswordSheetSpec extends AnyFunSuite {
  private def card(school: String, num: Int, name: String, i: Int) =
    PasswordCard("2025-2026 ICPC, Northern Eurasia", school, num, name, s"team1_$i", f"${i * 7919L}%011d")

  private val cards =
    (1 to 6).map(i => card("Innopolis University", i, s"Team $i: Иванов, Петров, Сидоров", i)) ++
      Seq(
        card("Chuvash State University", 1, "2048: Fisunov, Kuznetsov, Fisunov", 20),
        card("Autonomous professional educational non-profit organization", 1,
          "Singularity: Glazov, Plotnikov, Petrov Petrov Petrov Petrov Petrov Petrov Petrov Petrov Petrov Petrov", 21),
        card("Autonomous professional educational non-profit organization", 2, "Kalyushin, Lukin, Nekrasov", 22),
        card("X", 1, "Q: A_B, C&D, E%F", 23),
        card("Школа №5", 0, "Иванов, Петров, Сидоров", 24),
      )

  private def pages(pdf: Array[Byte]): Int = {
    val d = Loader.loadPDF(pdf)
    try d.getNumberOfPages finally d.close()
  }

  test("exactly nine cards per page, never split") {
    assert(PasswordSheet.CardsPerPage == 9)
    assert(pages(PasswordSheet.render(cards, cards)) == 2) // 11 cards
    assert(pages(PasswordSheet.render(cards.take(9), cards)) == 1)
    assert(pages(PasswordSheet.render(cards.take(10), cards)) == 2)
  }

  test("empty selection still gives a valid one-page pdf") {
    assert(pages(PasswordSheet.render(Nil, cards)) == 1)
  }

  test("school line hides #N for a single team and keeps it otherwise") {
    assert(PasswordSheet.schoolLine(cards(0), cards) == "Innopolis University #1")
    assert(PasswordSheet.schoolLine(cards(6), cards) == "Chuvash State University")
    // a single selected card still counts the school's teams in the whole contest
    assert(PasswordSheet.schoolLine(cards(0), cards.take(1)) == "Innopolis University")
    assert(PasswordSheet.schoolLine(cards.last, cards) == "Школа №5")
  }

  test("text and credentials land in the pdf") {
    val d = Loader.loadPDF(PasswordSheet.render(cards, cards))
    val text = try new PDFTextStripper().getText(d) finally d.close()
    assert(text.contains("Иванов, Петров, Сидоров"))
    assert(text.contains("team1_21"))
    assert(text.contains("Chuvash State University"))
  }

  test("numero sign is not turned into a question mark") {
    val d = Loader.loadPDF(PasswordSheet.render(Seq(card("Школа №5", 0, "Команда №1: Иванов", 30)), cards))
    val text = try new PDFTextStripper().getText(d) finally d.close()
    assert(!text.contains("?"))
    assert(text.contains("Школа №5") && text.contains("Команда №1: Иванов"))
    // characters the font really lacks still become '?'
    val d2 = Loader.loadPDF(PasswordSheet.render(Seq(card("A\u4e2dB", 0, "n", 31)), cards))
    val text2 = try new PDFTextStripper().getText(d2) finally d2.close()
    assert(text2.contains("A?B"))
  }

  test("writes a sample for eyeballing") {
    val out = Paths.get("target", "test-password-sheet.pdf")
    Files.write(out, PasswordSheet.render(cards, cards))
  }

  test("default selection leaves out jury and coach teams, explicit filters do not") {
    def row(teamId: Int, schoolId: Int, school: String) =
      PasswordSheet.Row(teamId, schoolId, card(school, 1, "n", teamId))
    val rows = Seq(
      row(1, 10, "Innopolis University"),
      row(2, 11, InitialImportTools.JurySchoolFullName),
      row(3, 12, InitialImportTools.CoachSchoolFullName),
      row(4, 10, "Innopolis University"))
    assert(PasswordSheet.select(rows, None, None).map(_.login) == Seq("team1_1", "team1_4"))
    assert(PasswordSheet.select(rows, Some(11), None).map(_.login) == Seq("team1_2"))
    assert(PasswordSheet.select(rows, None, Some(3)).map(_.login) == Seq("team1_3"))
    assert(PasswordSheet.select(rows, Some(10), Some(4)).map(_.login) == Seq("team1_4"))
  }
}

class ServiceTeamsSpec extends AnyFunSuite {
  test("jury and coach teams are numbered from 1 with the established names") {
    val jury = InitialImportTools.JuryTeams.teams(3)
    assert(jury.map(_.teamID) == Seq(1, 2, 3))
    assert(jury.map(_.teamName) == Seq("Test 1", "Test 2", "Test 3"))
    assert(jury.forall(_.school.schoolFullName == "Contest Jury"))
    assert(InitialImportTools.CoachTeams.teams(2).map(_.teamName) == Seq("Coach1", "Coach2"))
  }
}
