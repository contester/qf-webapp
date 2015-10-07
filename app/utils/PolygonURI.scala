package utils

import java.net.{URI, URL}

import org.apache.commons.io.Charsets
import org.apache.http.NameValuePair
import org.apache.http.client.utils.URLEncodedUtils
import org.apache.http.message.BasicNameValuePair

object PolygonURL {
  def getParams(url: URL) = {
    import collection.JavaConversions._
    URLEncodedUtils.parse(url.toURI, "UTF-8").map(x => x.getName -> x.getValue).toMap
  }

  def stripQuery(url: URL) =
    new URL(url.getProtocol, url.getHost, url.getPort, url.getPath)

  def withQuery(url: URL, params: Map[String, String]) =
    if (params.isEmpty)
      url
    else {
      import collection.JavaConversions._
      val newParams = asJavaIterable((getParams(url) ++ params).map(x => new BasicNameValuePair(x._1, x._2).asInstanceOf[NameValuePair]))
      val newQuery = URLEncodedUtils.format(newParams, Charsets.UTF_8)
      new URL(stripQuery(url), "?" + newQuery)
    }

  def apply(url: URL): PolygonProblemHandle = {
    val params = getParams(url)
    val revision = params.get("revision").map(_.toInt)

    new PolygonProblemHandle(stripQuery(url), revision)
  }

  def apply(urlString: String): PolygonProblemHandle =
    apply(new URL(urlString))

  def shortId(url: URL) =
    url.getPath.split("/").takeRight(2).mkString("/")
}

class PolygonProblemHandle(val url: URL, val revision: Option[Int]) {
  val objectUrl = new URL(url, "problem.xml")
  val params = revision.map("revision" -> _.toString).toIterable

  override def toString = "PolygonProblemHandle(\"%s\"%s)".format(url, revision.map(", " + _).getOrElse(""))

  def uri: URI =
    new URI(PolygonURL.withQuery(url, params.toMap).toString)
}
