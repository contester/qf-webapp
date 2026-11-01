package models

import org.scalatest.funsuite.AnyFunSuite

class ComposeTeamNameSpec extends AnyFunSuite {
  test("custom name goes before the surnames") {
    assert(InitialImportTools.composeTeamName("Best Team", Seq("Ivanov", "Petrov", "Sidorov")) == "Best Team: Ivanov, Petrov, Sidorov")
  }

  test("no custom name leaves only the surnames") {
    assert(InitialImportTools.composeTeamName("", Seq("Ivanov", "Petrov", "Sidorov")) == "Ivanov, Petrov, Sidorov")
  }
}

class PickSurnameSpec extends AnyFunSuite {
  test("english: last token") {
    assert(InitialImportTools.pickSurnameEn("Ivan Petrov") == "Petrov")
  }

  test("russian: first token") {
    assert(InitialImportTools.pickSurnameRu("Петров Иван Сергеевич") == "Петров")
  }
}
