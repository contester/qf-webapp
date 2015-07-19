package utils

import slick.jdbc.{PositionedParameters, SetParameter}

object Db {
  implicit object SetByteArray extends SetParameter[Array[Byte]] {
    override def apply(v1: Array[Byte], v2: PositionedParameters): Unit = v2.setBytes(v1)
  }
}