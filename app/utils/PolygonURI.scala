package utils

import java.net.URL
import java.nio.charset.StandardCharsets

import org.apache.http.NameValuePair
import org.apache.http.client.utils.URLEncodedUtils
import org.apache.http.message.BasicNameValuePair

object PolygonURL {
  private[this] def getParams(url: URL): Map[String, String] = {
    import collection.JavaConverters._
    URLEncodedUtils.parse(url.toURI, StandardCharsets.UTF_8).asScala.map(x => x.getName -> x.getValue).toMap
  }

  private[this] def stripQuery(url: URL) =
    new URL(url.getProtocol, url.getHost, url.getPort, url.getPath)

  def withQuery(url: URL, params: Map[String, String]) =
    if (params.isEmpty)
      url
    else {
      import collection.JavaConverters._
      val newParams = asJavaIterable((getParams(url) ++ params).map(x => new BasicNameValuePair(x._1, x._2).asInstanceOf[NameValuePair]))
      val newQuery = URLEncodedUtils.format(newParams, StandardCharsets.UTF_8)
      new URL(stripQuery(url), "?" + newQuery)
    }

  def apply(url: URL): PolygonProblemHandle = {
    val params = getParams(url)
    val revision = params.get("revision").map(_.toInt)

    new PolygonProblemHandle(stripQuery(url), revision)
  }

  def apply(urlString: String): PolygonProblemHandle =
    apply(new URL(urlString))

  def getPathPart(url: URL) =
    url.getPath.stripPrefix("/").stripSuffix("/")

  def getPdbPath(url: URL): String =
    ("polygon" :: url.getProtocol :: url.getHost :: (if (url.getPort != -1) url.getPort.toString :: getPathPart(url) :: Nil else getPathPart(url) :: Nil)).mkString("/")

}

class PolygonProblemHandle(val url: URL, val revision: Option[Int]) {
  override def toString = "PolygonProblemHandle(\"%s\"%s)".format(url, revision.map(", " + _).getOrElse(""))

  val prefix = "problem/" + PolygonURL.getPdbPath(url) + "/" + revision.get.toString
}
