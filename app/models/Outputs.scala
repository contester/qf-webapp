package models

import play.api.Logging
import play.api.libs.ws.WSClient
import protos.Assets.{Asset, TestingRecord}
import utils.PolygonProblemHandle

import scala.concurrent.ExecutionContext

case class ResultAssets(test: Int, input: Option[Asset], output: Option[Asset], answer: Option[Asset])

object Outputs extends Logging {
  def getAllAssets2(ws: WSClient, prefix: String, shortName: String, submitId: Int, testingId: Int, tests: Seq[Int], handle: PolygonProblemHandle)(implicit ec: ExecutionContext) = {
    import scala.concurrent.duration._
    ws.url(s"${prefix}protopackage/")
      .withQueryStringParameters(
        "problem" -> handle.prefix,
        "submit" -> submitId.toString,
        "testing" -> testingId.toString,
        "contest" -> shortName
      ).withRequestTimeout(30.seconds).get().map {
      resp =>
        resp.status match {
          case 200 =>
            Some(TestingRecord.parseFrom(resp.bodyAsBytes.toArray))
          case _ =>
            logger.info(s"get($prefix, $shortName, $submitId, $testingId, $handle): ${resp.status} ${resp.statusText}")
            None
        }
    }.map { optRecord =>
      optRecord.map { tr =>
        tr.test.map { otr =>
          ResultAssets(otr.testId.toInt, otr.input, otr.output, otr.answer)
        }
      }.getOrElse(Seq()).map(r => r.test -> r).toMap
    }
  }
}