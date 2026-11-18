package live

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import org.apache.commons.io.FileUtils
import org.scalatest.funsuite.AnyFunSuite

class StaticFilesSpec extends AnyFunSuite {
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
