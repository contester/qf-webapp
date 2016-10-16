package utils

import java.nio.charset.Charset

import akka.stream._
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

object GridfsTools {
  def getFile(ws: WSClient, name: String, sizeLimit: Long)(implicit ec: ExecutionContext, mat: Materializer): Future[Option[GridfsContent]] = {
    ws.url(name).withMethod("GET").withHeaders("X-Fs-Limit" -> sizeLimit.toString).get()
      .flatMap { resp =>
        resp.status match {
        case 404 => Future.successful(None)
        case 200 =>
          val truncated = resp.header("X-Fs-Truncated").map(_ == "true").getOrElse(false)
          Future.successful(Some(GridfsContent(resp.bodyAsBytes.toArray, truncated)))
        case _ =>
          Logger.info(s"getFile($name): ${resp.status} ${resp.statusText}")
            Future.successful(None)
      }
    }
  }
}