package utils

import java.nio.charset.StandardCharsets

import akka.actor.ActorContext
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{BroadcastHub, Keep, Sink, Source}
import org.apache.commons.io.FileUtils
import play.api.libs.Files
import play.api.mvc.MultipartFormData

object Concur {
  def broadcast[T]()(implicit ac: ActorContext) = {
    implicit val materializer = ActorMaterializer()

    val (ch, out) = Source.queue[T](1024, OverflowStrategy.dropHead)
      .toMat(BroadcastHub.sink)(Keep.both).run()
    out.runWith(Sink.ignore)
    (out, ch)
  }
}

object FormUtil {
  def inlineOrFile(inline: String, fileOpt: Option[MultipartFormData.FilePart[Files.TemporaryFile]]): Option[Array[Byte]] =
    if (!inline.isEmpty) {
      Some(inline.getBytes(StandardCharsets.UTF_8))
    } else fileOpt.map { f =>
      FileUtils.readFileToByteArray(f.ref.file)
    }

  val emptyBytes = new Array[Byte](0)
}