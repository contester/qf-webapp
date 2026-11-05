package utils

import org.joda.time.{DateTime, DateTimeZone}

object ShortTime {
  /** "2026-11-05 10:00", or "2026-11-05 10:00:30" when the seconds are not zero; in the server's time zone. */
  def format(d: DateTime): String = {
    val local = d.withZone(DateTimeZone.getDefault)
    local.toString(if (local.getSecondOfMinute == 0) "yyyy-MM-dd HH:mm" else "yyyy-MM-dd HH:mm:ss")
  }
}
