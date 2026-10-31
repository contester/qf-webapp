package utils

import java.nio.ByteBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}

/** Guesses the encoding of an uploaded source file for the printing pipeline.
  * The returned names are Python codec names (pygmentize's `encoding` option). */
object CharsetDetect {
  private def isValidUtf8(data: Array[Byte]): Boolean =
    try {
      StandardCharsets.UTF_8.newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
        .decode(ByteBuffer.wrap(data))
      true
    } catch {
      case _: java.nio.charset.CharacterCodingException => false
    }

  private def hasUtf8Bom(data: Array[Byte]) =
    data.length >= 3 && data(0) == 0xEF.toByte && data(1) == 0xBB.toByte && data(2) == 0xBF.toByte

  /** Valid UTF-8 (including pure ASCII) is utf-8, anything else is assumed to be Windows-1251. */
  def detect(data: Array[Byte]): String =
    if (!isValidUtf8(data)) "windows-1251"
    else if (hasUtf8Bom(data)) "utf-8-sig"
    else "utf-8"
}
