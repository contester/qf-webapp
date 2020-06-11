package utils

import java.sql.Timestamp

import actors.WaiterActor
import akka.actor.ActorRef
import com.github.nscala_time.time.Imports
import play.api.libs.json.{JsValue, Json}
import slick.jdbc.{JdbcCapabilities, PositionedParameters, SetParameter}
import slick.basic.Capability
import slick.driver.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Db {
  implicit object SetByteArray extends SetParameter[Array[Byte]] {
    override def apply(v1: Array[Byte], v2: PositionedParameters): Unit = v2.setBytes(v1)
  }


//  import slick.jdbc.PostgresProfile.api._
//
//  implicit val jsValueColumnType = MappedColumnType.base[JsValue, String](
//    x => Json.stringify(x),
//    x => Json.parse(x)
//  )
}

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

import com.github.tminglei.slickpg._

trait MyPostgresProfile extends ExPostgresProfile
  with PgArraySupport
  with PgDate2Support
  with PgRangeSupport
  with PgHStoreSupport
  with PgPlayJsonSupport
  with PgSearchSupport
  with PgNetSupport
  with PgLTreeSupport {
  def pgjson = "jsonb" // jsonb support is in postgres 9.4.0 onward; for 9.3.x use "json"

  // Add back `capabilities.insertOrUpdate` to enable native `upsert` support; for postgres 9.5+
  override protected def computeCapabilities: Set[Capability] =
    super.computeCapabilities + JdbcCapabilities.insertOrUpdate

  override val api = MyAPI

  object MyAPI extends API with ArrayImplicits
    with DateTimeImplicits
    with JsonImplicits
    with NetImplicits
    with LTreeImplicits
    with RangeImplicits
    with HStoreImplicits
    with SearchImplicits
    with SearchAssistants {
    implicit val strListTypeMapper = new SimpleArrayJdbcType[String]("text").to(_.toList)
    implicit val playJsonArrayTypeMapper =
      new AdvancedArrayJdbcType[JsValue](pgjson,
        (s) => utils.SimpleArrayUtils.fromString[JsValue](Json.parse(_))(s).orNull,
        (v) => utils.SimpleArrayUtils.mkString[JsValue](_.toString())(v)
      ).to(_.toList)
  }
}

object MyPostgresProfile extends MyPostgresProfile