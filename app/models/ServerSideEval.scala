package models

import com.github.nscala_time.time.Imports._
import org.apache.commons.codec.Charsets
import slick.jdbc.GetResult

case class EvalEntry(id: Long, contest: Int, team: Int, languageName: String, source: Array[Byte], input: Array[Byte],
                     output: Array[Byte], arrived: DateTime, finishTime: Option[DateTime], resultCode: Int,
                     timex: TimeMs, memory: Memory, returnCode: Long) {
  private[this] def trimStr(s: Array[Byte]) = {
    val x = new String(input, Charsets.toCharset("CP1251"))
    if (x.length > 1024) {
      x.substring(0, 1024) + "\n..."
    } else x
  }

  def sourceStr = new String(source, "CP1251")
  def inputStr = trimStr(input)
  def outputStr = trimStr(output)

  def resultStr = if (finishTime.isDefined)
    SubmitResult.message.getOrElse(resultCode, "???")
  else
    "Выполняется"
}