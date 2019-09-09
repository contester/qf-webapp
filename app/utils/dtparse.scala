package utils

import com.github.nscala_time.time
import com.github.nscala_time.time.Imports
import com.github.nscala_time.time.Imports._
import org.joda.time.format.ISODateTimeFormat
import play.api.data.FormError
import play.api.data.format.Formatter

object DateParse {
  def nscalaTimeFormat(pattern: String): Formatter[DateTime] =
    new Formatter[Imports.DateTime] {
      private val fmt = DateTimeFormat.forPattern(pattern)
      private val p0 = ISODateTimeFormat.dateTimeParser()

      import play.api.data.format.Formats._

      private def timestampParse(data: String) = p0.parseDateTime(data)

      override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], time.Imports.DateTime] =
        parsing(timestampParse, "error.nscalatime", Nil)(key, data)

      override def unbind(key: String, value: time.Imports.DateTime): Map[String, String] = Map(key -> fmt.print(value))
    }

  implicit val nscalaDTFormat = nscalaTimeFormat("yyyy-MM-dd'T'HH:mm:ss")

  import play.api.data.Forms._

  val nscalaDateTime = of[DateTime](nscalaDTFormat)
}