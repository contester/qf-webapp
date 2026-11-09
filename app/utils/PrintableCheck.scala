package utils

/** Decides whether an uploaded file is something the printing pipeline should be given:
  * plain text or source code, short enough for roughly 10 pages. */
object PrintableCheck {
  sealed trait Result
  case object Ok extends Result
  case object Binary extends Result
  case object TooLong extends Result

  /** The limits are configurable (printing.max_lines, printing.max_bytes) so they can be changed without a rebuild. */
  case class Limits(maxLines: Int, maxBytes: Int)

  /** 12pt typewriter on A4 fits about 48 lines a page, the cover block eats a part of the first one.
    * A full 450 lines of 73 columns is about 33 KB. */
  val DefaultLimits = Limits(maxLines = 450, maxBytes = 32000)

  def limitsFrom(cfg: play.api.Configuration): Limits = Limits(
    cfg.getOptional[Int]("printing.max_lines").getOrElse(DefaultLimits.maxLines),
    cfg.getOptional[Int]("printing.max_bytes").getOrElse(DefaultLimits.maxBytes))

  private def isBinaryByte(b: Byte): Boolean = {
    val c = b & 0xff
    // Tab, LF, FF, CR and ^Z (old DOS end-of-file marker) are fine; so is everything from space up (UTF-8, cp1251).
    c < 0x20 && c != 0x09 && c != 0x0a && c != 0x0c && c != 0x0d && c != 0x1a
  }

  def check(data: Array[Byte], limits: Limits = DefaultLimits): Result =
    if (data.exists(isBinaryByte)) Binary
    else if (data.length > limits.maxBytes || data.count(_ == '\n'.toByte) + 1 > limits.maxLines) TooLong
    else Ok
}
