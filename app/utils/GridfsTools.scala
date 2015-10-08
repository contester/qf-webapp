package utils

import java.io.InputStream
import java.nio.charset.{Charset}
import java.util.Arrays
import java.util.zip.{InflaterInputStream, Inflater}

import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.gridfs.GridFS
import org.apache.commons.codec.Charsets
import org.apache.commons.io.IOUtils
import play.api.Logger

import scala.concurrent.{ExecutionContext, Future}

case class GridfsContent(content: Array[Byte], truncated: Boolean) {
  def asString(charset: Charset) = new String(content, charset)

  override def toString: String = asString(GridfsContent.CP1251) + (if (truncated) "\n..." else "")
}

object GridfsContent {
  val CP1251 = Charsets.toCharset("CP1251")
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

  def getFile(fs: GridFS, name: String, sizeLimit: Int)(implicit ec: ExecutionContext): Future[Option[GridfsContent]] =
    Future {
      fs.findOne(name).map { file =>
        val metadata = new MongoDBObject(file.metaData)
        val ctype = metadata.getAsOrElse[String]("compressionType", "")
        val origSize = metadata.getAs[Long]("originalSize")
        val istream = if (ctype == "ZLIB") new InflaterInputStream(file.inputStream) else file.inputStream
        readTruncated(istream, sizeLimit, origSize)
     }
    }

}