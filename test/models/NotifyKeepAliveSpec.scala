package models

import jdk.net.ExtendedSocketOptions
import org.scalatest.funsuite.AnyFunSuite
import play.api.Configuration

import java.net.StandardSocketOptions

class NotifyKeepAliveSpec extends AnyFunSuite {
  import NotifyListener.{ConnectionSettings, KeepAlive}

  test("the socket factory sets SO_KEEPALIVE and the configured idle, interval and count") {
    val f = new KeepAliveSocketFactory(KeepAlive(45, 7, 4).factoryArg)
    val s = f.createSocket()
    try {
      assert(s.getOption(StandardSocketOptions.SO_KEEPALIVE))
      assume(s.supportedOptions().contains(ExtendedSocketOptions.TCP_KEEPIDLE), "no extended keep-alive options here")
      assert(s.getOption(ExtendedSocketOptions.TCP_KEEPIDLE) == 45)
      assert(s.getOption(ExtendedSocketOptions.TCP_KEEPINTERVAL) == 7)
      assert(s.getOption(ExtendedSocketOptions.TCP_KEEPCOUNT) == 4)
    } finally s.close()
  }

  test("without an argument the factory uses 60 s / 10 s / 6") {
    val s = new KeepAliveSocketFactory().createSocket()
    try {
      assume(s.supportedOptions().contains(ExtendedSocketOptions.TCP_KEEPIDLE))
      assert((s.getOption(ExtendedSocketOptions.TCP_KEEPIDLE): Int, s.getOption(ExtendedSocketOptions.TCP_KEEPINTERVAL): Int,
        s.getOption(ExtendedSocketOptions.TCP_KEEPCOUNT): Int) == ((60, 10, 6)))
    } finally s.close()
  }

  test("a bad factory argument is rejected") {
    intercept[IllegalArgumentException](new KeepAliveSocketFactory("60,x,6"))
    intercept[IllegalArgumentException](new KeepAliveSocketFactory("60,10"))
  }

  private val ka = KeepAlive(60, 10, 6)
  private val serverOptions = "-c tcp_keepalives_idle=60 -c tcp_keepalives_interval=10 -c tcp_keepalives_count=6"

  test("connect properties carry the socket factory, its argument and the server keep-alive options") {
    val (url, p) = NotifyListener.connectProperties(ConnectionSettings("jdbc:postgresql://h:5432/db", "u", "pw", "app", ka))
    assert(url == "jdbc:postgresql://h:5432/db")
    assert(p.getProperty("socketFactory") == classOf[KeepAliveSocketFactory].getName)
    assert(p.getProperty("socketFactoryArg") == "60,10,6")
    assert(p.getProperty("options") == serverOptions)
    assert(p.getProperty("tcpKeepAlive") == "true")
    assert((p.getProperty("user"), p.getProperty("password"), p.getProperty("ApplicationName")) == (("u", "pw", "app")))
  }

  test("options already in the URL are merged (theirs last, so they win) and taken out of the URL") {
    val (url, p) = NotifyListener.connectProperties(ConnectionSettings(
      "jdbc:postgresql://h/db?sslmode=disable&options=-c%20search_path%3Dx&connectTimeout=5", "u", "pw", "app", ka))
    assert(url == "jdbc:postgresql://h/db?sslmode=disable&connectTimeout=5")
    assert(p.getProperty("options") == s"$serverOptions -c search_path=x")
    // the merged value survives pgjdbc's own URL parsing
    val parsed = org.postgresql.Driver.parseURL(url, p)
    assert(parsed.getProperty("options") == s"$serverOptions -c search_path=x")
    assert(parsed.getProperty("sslmode") == "disable")
  }

  test("ConnectionSettings.fromConfig: url at the top level, keep-alive defaults") {
    val s = ConnectionSettings.fromConfig(Configuration("slick.dbs.default.db.url" -> "jdbc:postgresql://a/b",
      "slick.dbs.default.db.user" -> "u", "slick.dbs.default.db.password" -> "p"))
    assert(s == ConnectionSettings("jdbc:postgresql://a/b", "u", "p", NotifyListener.DefaultApplicationName, KeepAlive(60, 10, 6)))
  }

  test("ConnectionSettings.fromConfig: url and credentials under properties, keep-alive from notify.keepalive") {
    val s = ConnectionSettings.fromConfig(Configuration("slick.dbs.default.db.properties.url" -> "jdbc:postgresql://a/b",
      "slick.dbs.default.db.properties.user" -> "u", "slick.dbs.default.db.properties.password" -> "p",
      "notify.keepalive.idle" -> 30, "notify.keepalive.interval" -> 5, "notify.keepalive.count" -> 3))
    assert(s == ConnectionSettings("jdbc:postgresql://a/b", "u", "p", NotifyListener.DefaultApplicationName, KeepAlive(30, 5, 3)))
  }

  test("ConnectionSettings.fromConfig: a missing url fails at once with a clear message") {
    val e1 = intercept[IllegalArgumentException](ConnectionSettings.fromConfig(Configuration("slick.dbs.default.db.user" -> "u")))
    assert(e1.getMessage.contains("slick.dbs.default.db.url"))
    val e2 = intercept[IllegalArgumentException](ConnectionSettings.fromConfig(Configuration("x" -> 1)))
    assert(e2.getMessage.contains("slick.dbs.default.db.url"))
  }
}
