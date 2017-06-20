package utils

import akka.actor.ActorContext
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{BroadcastHub, Keep, Sink, Source}

object Concur {
  def broadcast[T]()(implicit ac: ActorContext) = {
    implicit val materializer = ActorMaterializer()

    val (ch, out) = Source.queue[T](1024, OverflowStrategy.dropHead)
      .toMat(BroadcastHub.sink)(Keep.both).run()
    out.runWith(Sink.ignore)
    (out, ch)
  }
}