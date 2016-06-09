package utils

import java.io.InputStream
import java.nio.charset.Charset
import java.util.Arrays

import akka.stream._
import akka.util.ByteString
import org.apache.commons.codec.Charsets
import play.api.Logger
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}

case class GridfsContent(content: Array[Byte], truncated: Boolean) {
  def asString(charset: Charset) = new String(content, charset)

  override def toString: String = asString(GridfsContent.CP1251) + (if (truncated) "\n..." else "")
}

object GridfsContent {
  val CP1251 = Charsets.toCharset("CP1251")
}

import akka.stream.stage._

class ByteLimiter(val maximumBytes: Long) extends GraphStage[FlowShape[ByteString, ByteString]] {
  val in = Inlet[ByteString]("ByteLimiter.in")
  val out = Outlet[ByteString]("ByteLimiter.out")
  override val shape = FlowShape.of(in, out)

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = new GraphStageLogic(shape) {
    private var count = 0

    setHandlers(in, out, new InHandler with OutHandler {

      override def onPull(): Unit = {
        pull(in)
      }

      override def onPush(): Unit = {
        val chunk = grab(in)
        val newCount = count + chunk.size
        if (newCount > maximumBytes) {
          val newChunk = chunk.take((maximumBytes - count).toInt)
          push(out, newChunk)
          completeStage()
        } else {
          count = newCount
          push(out, chunk)
        }
      }
    })
  }
}

object GridfsTools {
  private def readTruncated(is: InputStream, sizeLimit: Int, origSizeOpt: Option[Long]): GridfsContent = {
    val buffer = new Array[Byte](sizeLimit)
    val read = is.read(buffer)
    if (read <= 0)
      GridfsContent(new Array[Byte](0), false)
    else {
      val result = Arrays.copyOf(buffer, read)
      if (read < sizeLimit)
        GridfsContent(result, false)
      else
        GridfsContent(result, is.available() > 0)
    }
  }

  def getFile(ws: WSClient, name: String, sizeLimit: Long)(implicit ec: ExecutionContext, mat: Materializer): Future[Option[GridfsContent]] = {
    Logger.info(s"name=$name")
    ws.url(name).withMethod("GET").stream().flatMap { resp =>
      resp.headers.status match {
        case 404 => Future.successful(None)
        case 200 =>
          val origSize = resp.headers.headers.get("X-Fs-Content-Length").map(_.head).map(_.toLong).getOrElse(0L)
          resp.body.via(new ByteLimiter(sizeLimit)).runReduce((x, y) => x ++ y).map { x =>
            Some(GridfsContent(x.toByteBuffer.array(), origSize > sizeLimit))
          }
      }
    }
  }
}