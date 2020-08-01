package utils

import akka.actor.ActorRef

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Ask {
  def apply[T](ref: ActorRef, msg: AnyRef)(implicit timeout: akka.util.Timeout, ec: ExecutionContext) = {
    import akka.pattern.ask
    ref.ask(msg).mapTo[Try[T]].flatMap {
      case Success(x) => Future.successful(x)
      case Failure(x) => Future.failed(x)
    }
  }

  def respond[T](ref: ActorRef, v: Try[T]) =
    ref ! v
}

object Selectable {
  def forSelect(x: Seq[(String, String)], top: String) =
    Seq(("", top)) ++ x
}