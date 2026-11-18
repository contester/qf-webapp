package models

import jdk.net.ExtendedSocketOptions
import play.api.Logging

import java.net.{InetAddress, Socket, StandardSocketOptions}
import java.util.concurrent.atomic.AtomicBoolean
import javax.net.SocketFactory

/**
  * The socket factory of the notification connection (pgjdbc `socketFactory`): every socket has SO_KEEPALIVE with
  * TCP_KEEPIDLE / TCP_KEEPINTERVAL / TCP_KEEPCOUNT set, so the kernel notices a silently dead server within
  * idle + interval * count seconds and the blocked read fails; the listener then reconnects. No timer of ours runs.
  *
  * pgjdbc instantiates it by class name and passes `socketFactoryArg` ("idle,interval,count", in seconds) to the
  * String constructor; the no-arg constructor uses 60,10,6. Where the JDK does not support the extended options a
  * warning is logged once and only SO_KEEPALIVE (with the OS defaults) is set.
  */
class KeepAliveSocketFactory(arg: String) extends SocketFactory with Logging {
  def this() = this(NotifyListener.KeepAlive().factoryArg)

  private val (idle, interval, count) = KeepAliveSocketFactory.parse(arg)

  private def configure(s: Socket): Socket = {
    s.setOption(StandardSocketOptions.SO_KEEPALIVE, java.lang.Boolean.TRUE)
    val supported = s.supportedOptions()
    if (supported.contains(ExtendedSocketOptions.TCP_KEEPIDLE) && supported.contains(ExtendedSocketOptions.TCP_KEEPINTERVAL) &&
      supported.contains(ExtendedSocketOptions.TCP_KEEPCOUNT)) {
      s.setOption(ExtendedSocketOptions.TCP_KEEPIDLE, Int.box(idle))
      s.setOption(ExtendedSocketOptions.TCP_KEEPINTERVAL, Int.box(interval))
      s.setOption(ExtendedSocketOptions.TCP_KEEPCOUNT, Int.box(count))
    } else if (KeepAliveSocketFactory.warned.compareAndSet(false, true))
      logger.warn("TCP_KEEPIDLE/TCP_KEEPINTERVAL/TCP_KEEPCOUNT are not supported here; " +
        "the notification connection uses SO_KEEPALIVE with the OS defaults")
    s
  }

  override def createSocket(): Socket = configure(new Socket())

  override def createSocket(host: String, port: Int): Socket = {
    val s = createSocket()
    s.connect(new java.net.InetSocketAddress(host, port))
    s
  }

  override def createSocket(host: String, port: Int, localHost: InetAddress, localPort: Int): Socket = {
    val s = createSocket()
    s.bind(new java.net.InetSocketAddress(localHost, localPort))
    s.connect(new java.net.InetSocketAddress(host, port))
    s
  }

  override def createSocket(host: InetAddress, port: Int): Socket = {
    val s = createSocket()
    s.connect(new java.net.InetSocketAddress(host, port))
    s
  }

  override def createSocket(address: InetAddress, port: Int, localAddress: InetAddress, localPort: Int): Socket = {
    val s = createSocket()
    s.bind(new java.net.InetSocketAddress(localAddress, localPort))
    s.connect(new java.net.InetSocketAddress(address, port))
    s
  }
}

object KeepAliveSocketFactory {
  private val warned = new AtomicBoolean(false)

  def parse(arg: String): (Int, Int, Int) =
    arg.split(',').map(_.trim) match {
      case Array(a, b, c) =>
        try {
          val v = (a.toInt, b.toInt, c.toInt)
          require(v._1 > 0 && v._2 > 0 && v._3 > 0, s"keep-alive values must be positive: '$arg'")
          v
        } catch {
          case _: NumberFormatException => throw new IllegalArgumentException(s"bad keep-alive argument '$arg' (want idle,interval,count)")
        }
      case _ => throw new IllegalArgumentException(s"bad keep-alive argument '$arg' (want idle,interval,count)")
    }
}
