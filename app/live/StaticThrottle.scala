package live

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardCopyOption}
import org.apache.commons.io.FileUtils
import scala.concurrent.duration.FiniteDuration

/**
 * Rate limiter for rewriting the static monitor file. Single-threaded; clock is in millis.
 *
 * The caller must call markDirty only when the freezing board actually changed;
 * unchanged boards must not mark dirty. A change arriving within `interval` of the
 * last write is deferred (trailing write) until lastWritten + interval.
 */
class StaticThrottle(interval: FiniteDuration) {
  private val intervalMs = interval.toMillis
  private var dirty = false
  private var lastWritten: Option[Long] = None

  def markDirty(now: Long): Unit = dirty = true

  def due(now: Long): Boolean =
    dirty && lastWritten.forall(t => now >= t + intervalMs)

  def written(now: Long): Unit = {
    dirty = false
    lastWritten = Some(now)
  }
}

object StaticFiles {
  /** Atomically writes `<contestId>.html` in dir via a `.new` temp file. */
  def write(dir: File, contestId: Int, html: String): Unit = {
    val tmpFile = new File(dir, s"$contestId.html.new")
    FileUtils.writeStringToFile(tmpFile, html, StandardCharsets.UTF_8)
    Files.move(tmpFile.toPath, new File(dir, s"$contestId.html").toPath, StandardCopyOption.ATOMIC_MOVE)
  }
}
