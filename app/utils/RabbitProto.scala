package utils

import com.spingo.op_rabbit.{RabbitMarshaller, RabbitUnmarshaller}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

object ProtoRabbitSupport {
  implicit def protoMarshaller[T <: GeneratedMessage with Message[T]](implicit cmp: GeneratedMessageCompanion[T]): RabbitMarshaller[T] =
    new RabbitMarshaller[T] {
      override def marshall(value: T): Array[Byte] = value.toByteArray

      override protected def contentType: String = "application/vnd.google.protobuf"

      override protected def contentEncoding: Option[String] = None
    }

  implicit def protoUnmarshaller[T <: GeneratedMessage with Message[T]](implicit cmp: GeneratedMessageCompanion[T]): RabbitUnmarshaller[T] =
    (value: Array[Byte], contentType: Option[String], contentEncoding: Option[String]) => cmp.parseFrom(value)
}