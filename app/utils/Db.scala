package utils

import java.sql.Timestamp

import com.github.nscala_time.time.Imports
import slick.jdbc.{PositionedParameters, SetParameter}

object Db {
  implicit object SetByteArray extends SetParameter[Array[Byte]] {
    override def apply(v1: Array[Byte], v2: PositionedParameters): Unit = v2.setBytes(v1)
  }

  import com.github.nscala_time.time.Imports._

  implicit object SetJodaDateTime extends SetParameter[DateTime] {
    override def apply(v1: Imports.DateTime, v2: PositionedParameters): Unit = v2.setTimestamp(new Timestamp(v1.getMillis))
  }

  import slick.driver.MySQLDriver.api._

  implicit val datetimeColumnType = MappedColumnType.base[DateTime, Timestamp](
    x => new Timestamp(x.getMillis),
    x => new DateTime(x)
  )

  // TODO: implement fsequence (future sequence)
  // TODO: new fork-join-executor for database access?
}