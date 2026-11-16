package models

import com.google.protobuf.ByteString
import org.scalatest.funsuite.AnyFunSuite
import protos.Assets.{Asset, TestRecord, TestingRecord}

class OutputsSpec extends AnyFunSuite {
  private def asset(name: String, body: String, truncated: Boolean = false, origSize: Long = -1L): Asset = {
    val bytes = ByteString.copyFromUtf8(body)
    val sz = if (origSize >= 0) origSize else bytes.size.toLong
    Asset(name = name, truncated = truncated, data = bytes, originalSize = sz)
  }

  test("parseAllAssets decodes compile and per-test diagnostic streams with truncation") {
    val rec = TestingRecord(
      solution = Some(asset("source", "int main(){}")),
      test = Seq(
        TestRecord(
          testId = 0,
          testerOutput = Some(asset("stdout", "comp-out")),
          testerError = Some(asset("stderr", "comp-err", truncated = true, origSize = 200))
        ),
        TestRecord(
          testId = 1,
          input = Some(asset("input", "1 2\n")),
          output = Some(asset("output", "3\n")),
          answer = Some(asset("answer", "3\n")),
          testerOutput = Some(asset("tester_output", "ok")),
          testerError = Some(asset("tester_error", "checker-warn")),
          solutionStderr = Some(asset("stderr", "dbg", truncated = true, origSize = 9000))
        )
      )
    )

    val parsed = Outputs.parseAllAssets(rec.toByteArray)
    assert(parsed.keySet == Set(0, 1))

    val c0 = parsed(0)
    assert(c0.testerOutput.get.data.toStringUtf8 == "comp-out")
    assert(c0.testerError.get.truncated)
    assert(c0.testerError.get.originalSize == 200L)

    val t1 = parsed(1)
    assert(t1.input.get.data.toStringUtf8 == "1 2\n")
    assert(t1.output.get.data.toStringUtf8 == "3\n")
    assert(t1.answer.get.data.toStringUtf8 == "3\n")
    assert(t1.testerOutput.get.data.toStringUtf8 == "ok")
    assert(t1.testerError.get.data.toStringUtf8 == "checker-warn")
    assert(t1.solutionStderr.get.truncated)
    assert(t1.solutionStderr.get.originalSize == 9000L)
  }

  test("parseCompileAssets extracts compile output and formats truncation marker") {
    val rec = TestingRecord(
      test = Seq(
        TestRecord(
          testId = 0,
          testerOutput = Some(asset("stdout", "line1")),
          testerError = Some(asset("stderr", "err1", truncated = true, origSize = 4096))
        )
      )
    )

    val compiled = Outputs.parseCompileAssets(rec.toByteArray)
    assert(compiled.isDefined)
    assert(compiled.get.combinedText == "line1\nerr1\n... [truncated, 4096 bytes total]")
  }
}
