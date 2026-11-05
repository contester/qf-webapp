package live

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import org.apache.commons.io.FileUtils
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.duration._

class StaticThrottleSpec extends AnyFunSuite {
  test("not due when clean") {
    val t = new StaticThrottle(10.seconds)
    assert(!t.due(0L))
    assert(!t.due(1000000L))
  }

  test("due immediately after first markDirty when never written") {
    val t = new StaticThrottle(10.seconds)
    t.markDirty(5L)
    assert(t.due(5L))
  }

  test("after written, markDirty within interval is due only at t + interval, then once") {
    val t = new StaticThrottle(10.seconds)
    t.written(1000L)
    t.markDirty(3000L)
    assert(!t.due(3000L))
    assert(!t.due(10999L))
    assert(t.due(11000L))
    assert(t.due(12000L))
    t.written(12000L)
    assert(!t.due(30000L))
  }

  test("markDirty after interval elapsed is due immediately") {
    val t = new StaticThrottle(10.seconds)
    t.written(0L)
    t.markDirty(20000L)
    assert(t.due(20000L))
  }

  test("written clears dirty; unmarked stays clean") {
    val t = new StaticThrottle(10.seconds)
    t.markDirty(0L)
    t.written(0L)
    assert(!t.due(100000L))
  }

  test("StaticFiles.write leaves no .new file and final has content") {
    val dir = Files.createTempDirectory("staticfiles").toFile
    try {
      StaticFiles.write(dir, 7, "<html>привет</html>")
      StaticFiles.write(dir, 7, "<html>second</html>")
      assert(dir.list().toSeq.sorted == Seq("7.html"))
      assert(FileUtils.readFileToString(new File(dir, "7.html"), StandardCharsets.UTF_8) == "<html>second</html>")
    } finally FileUtils.deleteDirectory(dir)
  }
}
