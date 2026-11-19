package controllers

import models.UnratedChoice
import org.scalatest.funsuite.AnyFunSuite
import org.stingray.contester.dbmodel.{Disqualified, NotRated, Unrated}
import play.api.mvc.Results

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/** The unrated toggle's responses without the controller wiring (AdminPagesHttpSpec drives the real routes). */
class UnratedToggleSpec extends AnyFunSuite {
  private implicit val ec: ExecutionContext = ExecutionContext.global

  test("the three values and only those") {
    assert(UnratedChoice.parse("none").contains(None))
    assert(UnratedChoice.parse("not_rated").contains(Some(NotRated)))
    assert(UnratedChoice.parse("disqualified").contains(Some(Disqualified)))
    for (bad <- Seq("", "None", "NOT_RATED", "dsq", "null", "rated")) assert(UnratedChoice.parse(bad).isEmpty, bad)
    assert(UnratedChoice.all.map(UnratedChoice.value) == Seq("none", "not_rated", "disqualified"))
  }

  private def run(value: Option[String], rows: Int) = {
    val writes = new AtomicInteger
    var written: Option[Option[Unrated]] = None
    val r = Await.result(UnratedToggle.respond(value, u => { writes.incrementAndGet(); written = Some(u); Future.successful(rows) },
      _ => Results.SeeOther("/back")), 10.seconds)
    (r.header.status, writes.get, written)
  }

  test("an unknown or missing value is a 400 and writes nothing") {
    assert(run(Some("bogus"), 1) == ((400, 0, None)))
    assert(run(None, 1) == ((400, 0, None)))
  }

  test("a participant without a credential for the round is a 404") {
    assert(run(Some("not_rated"), 0) == ((404, 1, Some(Some(NotRated)))))
  }

  test("a written value goes back to the page; none writes NULL") {
    assert(run(Some("disqualified"), 1) == ((303, 1, Some(Some(Disqualified)))))
    assert(run(Some("none"), 1) == ((303, 1, Some(None))))
  }

  test("every value has a label in both message files") {
    def keys(f: String) = new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(f)), "UTF-8")
      .split('\n').map(_.takeWhile(_ != '=')).toSet
    for (f <- Seq("conf/messages", "conf/messages.ru"); u <- UnratedChoice.all) assert(keys(f).contains(UnratedToggle.labelKey(u)), s"$f $u")
  }
}
