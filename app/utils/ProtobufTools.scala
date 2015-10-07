package utils

import java.io.InputStream

import com.google.protobuf.Message

object ProtobufTools {
  private[this] val ARRAY_OF_BYTE_ARRAY = Array[Class[_]](classOf[Array[Byte]])
  private[this] val ARRAY_OF_INPUT_STREAM = Array[Class[_]](classOf[InputStream])

  def createProtobuf[I <: Message](bytes: Array[Byte])(implicit manifest: Manifest[I]): I = {
    manifest.runtimeClass.getDeclaredMethod("parseFrom", ARRAY_OF_BYTE_ARRAY: _*).invoke(null, bytes).asInstanceOf[I]
  }

  /** Create protobuf from InputStream.
    *
    * @param input Stream to parse.
    * @param manifest Manifest for I
    * @tparam I Protobuf message.
    * @return Parsed proto.
    */
  def createProtobuf[I <: Message](input: InputStream)(implicit manifest: Manifest[I]): I = {
    manifest.runtimeClass.getDeclaredMethod("parseFrom", classOf[InputStream]).invoke(null, input).asInstanceOf[I]
  }

  private def createEmpty[I <: Message](implicit manifest: Manifest[I]): I = {
    manifest.runtimeClass.getDeclaredMethod("getDefaultInstance").invoke(null).asInstanceOf[I]
  }

  /** Create protobuf from Option[Array[Byte]].
    * Will return default proto if argument was None.
    *
    * @param opt Array[Byte] with proto, or not.
    * @param manifest
    * @tparam I
    * @return
    */
  def createProtobuf[I <: Message](opt: Option[Array[Byte]])(implicit manifest: Manifest[I]): I =
    opt.map(createProtobuf[I](_)).getOrElse(createEmpty[I])
}
