package utils

/** Decides whether an uploaded file is something the printing pipeline should be given:
  * plain text or source code, short enough for roughly 10 pages. */
object PrintableCheck {
  sealed trait Result
  case object Ok extends Result
  case object Binary extends Result
  case object TooLong extends Result

  /** 12pt typewriter on A4 fits about 48 lines a page, the cover block eats a part of the first one. */
  val MaxLines = 450
  /** Sanity cap for the size; a full 450 lines of 73 columns is about 33 KB. */
  val MaxBytes = 32000

  private def isBinaryByte(b: Byte): Boolean = {
    val c = b & 0xff
    // Tab, LF, FF, CR and ^Z (old DOS end-of-file marker) are fine; so is everything from space up (UTF-8, cp1251).
    c < 0x20 && c != 0x09 && c != 0x0a && c != 0x0c && c != 0x0d && c != 0x1a
  }

  def check(data: Array[Byte]): Result =
    if (data.exists(isBinaryByte)) Binary
    else if (data.length > MaxBytes || data.count(_ == '\n'.toByte) + 1 > MaxLines) TooLong
    else Ok
}
