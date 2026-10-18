package utils

import java.net.{URI, URL}
import java.nio.charset.StandardCharsets
import org.apache.http.NameValuePair
import org.apache.http.client.utils.URLEncodedUtils
import org.apache.http.message.BasicNameValuePair

object PolygonURL {
  private[this] def getParams(url: URL): Map[String, String] = {
    import collection.JavaConverters._
    URLEncodedUtils.parse(url.toURI, StandardCharsets.UTF_8).asScala.map(x => x.getName -> x.getValue).toMap
  }

  // Reassemble protocol/host/port/path without the query string. Built as a
  // string and parsed by URI so an already-encoded path is passed through
  // verbatim (the multi-arg URI constructors would re-quote it).
  private[this] def strippedString(url: URL): String = {
    val sb = new StringBuilder(url.getProtocol).append("://").append(url.getHost)
    if (url.getPort != -1) sb.append(":").append(url.getPort)
    sb.append(url.getPath)
    sb.toString
  }

  private[this] def stripQuery(url: URL): URL =
    new URI(strippedString(url)).toURL

  def withQuery(url: URL, params: Map[String, String]) =
    if (params.isEmpty)
      url
    else {
      import collection.JavaConverters._
      val newParams = asJavaIterable((getParams(url) ++ params).map(x => new BasicNameValuePair(x._1, x._2).asInstanceOf[NameValuePair]))
      val newQuery = URLEncodedUtils.format(newParams, StandardCharsets.UTF_8)
      new URI(strippedString(url) + "?" + newQuery).toURL
    }

  def apply(url: URL): PolygonProblemHandle = {
    val params = getParams(url)
    val revision = params.get("revision").map(_.toInt)

    new PolygonProblemHandle(stripQuery(url), revision)
  }

  def apply(urlString: String): PolygonProblemHandle =
    apply(new URI(urlString).toURL)

  def getPathPart(url: URL) =
    url.getPath.stripPrefix("/").stripSuffix("/")

  def getPdbPath(url: URL): String =
    ("polygon" :: url.getProtocol :: url.getHost :: (if (url.getPort != -1) url.getPort.toString :: getPathPart(url) :: Nil else getPathPart(url) :: Nil)).mkString("/")

}

class PolygonProblemHandle(val url: URL, val revision: Option[Int]) {
  override def toString = "PolygonProblemHandle(\"%s\"%s)".format(url, revision.map(", " + _).getOrElse(""))

  val prefix = "problem/" + PolygonURL.getPdbPath(url) + "/" + revision.get.toString
}
