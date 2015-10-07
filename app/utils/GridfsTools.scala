package utils

import java.util.Arrays
import java.util.zip.Inflater

import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.gridfs.GridFS
import org.apache.commons.io.IOUtils
import play.api.Logger

import scala.concurrent.{ExecutionContext, Future}

object GridfsTools {
  private def decompress(x: Array[Byte], origSize: Int): Array[Byte] = {
    val decompressor = new Inflater()
    decompressor.setInput(x)
    val result = new Array[Byte](origSize)
    val size = decompressor.inflate(result)
    result
  }

  def getFile(fs: GridFS, name: String)(implicit ec: ExecutionContext): Future[Option[Array[Byte]]] =
    Future {
      fs.findOne(name).map { file =>
        val metadata = new MongoDBObject(file.metaData)
        val ctype = metadata.getAsOrElse[String]("compressionType", "")
        val orig = IOUtils.toByteArray(file.inputStream)
        if (ctype == "ZLIB") {
          val originalSize = metadata.getAsOrElse[Long]("originalSize", 0)
          decompress(orig, originalSize.toInt)
        } else orig
      }
    }

  def getFileString(fs: GridFS, name: String)(implicit ec: ExecutionContext) =
    getFile(fs, name).map(_.map(x => new String(x, "CP1251")))
}