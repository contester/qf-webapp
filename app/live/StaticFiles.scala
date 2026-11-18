package live

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardCopyOption}
import org.apache.commons.io.FileUtils

// The interval throttle that lived here is gone: the live monitor writes the static file when its board changes,
// never on a timer (TODO(task17): the public board is rendered on demand from the database).
object StaticFiles {
  /** Atomically writes `<contestId>.html` in dir via a `.new` temp file. */
  def write(dir: File, contestId: Int, html: String): Unit = {
    val tmpFile = new File(dir, s"$contestId.html.new")
    FileUtils.writeStringToFile(tmpFile, html, StandardCharsets.UTF_8)
    Files.move(tmpFile.toPath, new File(dir, s"$contestId.html").toPath, StandardCopyOption.ATOMIC_MOVE)
  }
}
