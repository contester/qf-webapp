package utils

import java.nio.charset.Charset

import akka.stream._
import org.apache.commons.codec.Charsets
import play.api.Logger
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class GridfsContent(content: String, truncated: Boolean, size: Option[Long]) {
  def sizeStr = size.map(x => s"(${x}b)").getOrElse("")
  def truncStr = if (truncated) "[обрезан]" else ""
}

object GridfsTools {
  def getFile(ws: WSClient, name: String, sizeLimit: Long)(implicit ec: ExecutionContext): Future[Option[GridfsContent]] = {
    ws.url(name).withMethod("GET").withHeaders("X-Fs-Limit" -> sizeLimit.toString).get()
      .map { resp =>
        resp.status match {
        case 200 =>
          val truncated = resp.header("X-Fs-Truncated").map(_ == "true").getOrElse(false)
          val origSize = resp.header("X-Fs-Content-Length").flatMap(x => Try(x.toLong).toOption)
          Some(GridfsContent(resp.body, truncated, origSize))
        case _ =>
          Logger.info(s"getFile($name): ${resp.status} ${resp.statusText}")
          None
      }
    }
  }
}