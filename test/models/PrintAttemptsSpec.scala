package models

import com.google.protobuf.ByteString
import org.scalatest.funsuite.AnyFunSuite
import protos.Tickets.{BinaryJob, PrintJobReport}

class PrintAttemptsSpec extends AnyFunSuite {
  import PrintAttempts._

  private val render = BinaryJob(jobId = "a-1", pages = 2, payload = BinaryJob.Payload.Pdf(ByteString.copyFromUtf8("x")))

  private def rendered(r: Option[BinaryJob] = Some(render)) =
    PrintJobReport(jobExpandedId = "a-1", stage = PrintJobReport.Stage.RENDERED, render = r)

  private def finalReport(error: String) = PrintJobReport(jobExpandedId = "a-1", errorMessage = error)

  test("parseJobId understands attempt and legacy ids") {
    assert(parseJobId("a-12") == Some(AttemptRef(12)))
    assert(parseJobId("s-3") == Some(LegacyJobRef(3)))
  }

  test("parseJobId rejects everything else") {
    for (bad <- Seq("a-", "x-1", "a-1x", "", "a--1", "s-"))
      assert(parseJobId(bad) == None, bad)
  }

  test("terminal statuses") {
    assert(isTerminal(Status.Printed) && isTerminal(Status.Failed))
    assert(!isTerminal(Status.Queued) && !isTerminal(Status.Rendered))
  }

  test("RENDERED on a fresh attempt attaches the render and advances the status") {
    assert(decide(AttemptState(Status.Queued, hasRender = false), rendered()) == AttachRender(render, advanceStatus = true))
  }

  test("a redelivered RENDERED is ignored") {
    assert(decide(AttemptState(Status.Rendered, hasRender = true), rendered()) == Ignore)
  }

  test("RENDERED without a render is ignored") {
    assert(decide(AttemptState(Status.Queued, hasRender = false), rendered(None)) == Ignore)
  }

  test("RENDERED after the final report attaches the render but keeps the status") {
    assert(decide(AttemptState(Status.Printed, hasRender = false), rendered()) == AttachRender(render, advanceStatus = false))
  }

  test("FINAL without an error means printed") {
    assert(decide(AttemptState(Status.Rendered, hasRender = true), finalReport("")) == Finish(printed = true))
  }

  test("FINAL with an error means failed, even when nothing was rendered") {
    assert(decide(AttemptState(Status.Queued, hasRender = false), finalReport("boom")) == Finish(printed = false))
  }

  test("FINAL on a finished attempt is ignored") {
    assert(decide(AttemptState(Status.Failed, hasRender = true), finalReport("")) == Ignore)
    assert(decide(AttemptState(Status.Printed, hasRender = true), finalReport("boom")) == Ignore)
  }

  test("a stage this version doesn't know is ignored, not taken for a final result") {
    val future = PrintJobReport(jobExpandedId = "a-1", stage = PrintJobReport.Stage.Unrecognized(42))
    assert(decide(AttemptState(Status.Queued, hasRender = false), future) == Ignore)
  }
}
