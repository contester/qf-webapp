package models

import play.api.Logging
import play.api.libs.ws.WSClient
import protos.Assets.{Asset, TestingRecord}
import utils.{GridfsContent, PolygonProblemHandle}

import scala.concurrent.ExecutionContext

case class ResultAssets(test: Int, input: Option[GridfsContent], output: Option[GridfsContent], answer: Option[GridfsContent])

object Outputs extends Logging {
  private def assetToGc(a: Asset): GridfsContent = {
    GridfsContent(a.data.toStringUtf8, a.truncated, Some(a.originalSize))
  }

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
          ResultAssets(otr.testId.toInt, otr.input.map(assetToGc(_)), otr.output.map(assetToGc(_)), otr.answer.map(assetToGc(_)))
        }
      }.getOrElse(Seq()).map(r => r.test -> r).toMap
    }
  }
}