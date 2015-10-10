package models

import com.github.nscala_time.time.Imports._
import slick.jdbc.GetResult

case class EvalEntry(id: Int, touched: DateTime, ext: String, source: Array[Byte], input: Array[Byte],
                     output: Option[Array[Byte]], timex: TimeMs, memory: Memory, info: Long, result: Int, contest: Int,
                     team: Int, processed: Boolean, arrived: DateTime) {
  def sourceStr = new String(source, "UTF-8")
  def inputStr = new String(input, "UTF-8")
  def outputStr = output.map(new String(_, "UTF-8"))

  def resultStr = if (processed)
    SubmitResult.message.getOrElse(result, "???")
  else
    "Выполняется"
}

object EvalEntry {
  implicit val getResult = GetResult(r =>
    EvalEntry(r.nextInt(), new DateTime(r.nextTimestamp()), r.nextString(), r.nextBytes(), r.nextBytes(),
      r.nextBytesOption(), TimeMs(r.nextInt()), Memory(r.nextLong()), r.nextLong(), r.nextInt(), r.nextInt(),
      r.nextInt(), r.nextBoolean(), new DateTime(r.nextTimestamp()))
  )
}