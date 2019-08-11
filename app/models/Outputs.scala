package models

import controllers.Assets
import play.api.Logging
import play.api.libs.ws.WSClient
import protos.Assets.{Asset, TestingRecord}
import utils.GridfsTools.logger
import utils.{GridfsContent, GridfsTools, PolygonProblemHandle}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class ResultAssets(test: Int, input: Option[GridfsContent], output: Option[GridfsContent], answer: Option[GridfsContent])

object Outputs extends Logging {
  def assetToGc(a: Asset): GridfsContent = {
    GridfsContent(a.data.toStringUtf8, a.truncated, Some(a.originalSize))
  }

  def getOutput(ws: WSClient, prefix: String, shortName: String, submitId: Int, testingId: Int, test: Int)(implicit ec: ExecutionContext): Future[Option[GridfsContent]] =
    GridfsTools.getFile(ws, s"${prefix}submit/${shortName}/${submitId}/${testingId}/$test/output", 1024)

  def getInput(ws: WSClient, prefix: String, handle: PolygonProblemHandle, test: Int)(implicit ec: ExecutionContext): Future[Option[GridfsContent]] =
    GridfsTools.getFile(ws, prefix + handle.inputName(test), 1024)

  def getAnswer(ws: WSClient, prefix: String, handle: PolygonProblemHandle, test: Int)(implicit ec: ExecutionContext): Future[Option[GridfsContent]] =
    GridfsTools.getFile(ws, prefix + handle.answerName(test), 1024)

  def getAssets(ws: WSClient, prefix: String, shortName: String, submitId: Int, testingId: Int, test: Int, handle: PolygonProblemHandle)(implicit ec: ExecutionContext) = {
    getOutput(ws, prefix, shortName, submitId, testingId, test).zip(getInput(ws, prefix, handle, test)).zip(getAnswer(ws, prefix, handle, test)).map {
      case ((optOutput, optInput), optAnswer) => ResultAssets(test, optInput, optOutput, optAnswer)
    }
  }

  def getAllAssets(ws: WSClient, prefix: String, shortName: String, submitId: Int, testingId: Int, tests: Seq[Int], handle: PolygonProblemHandle)(implicit ec: ExecutionContext) = {
    Future.sequence(tests.map(test => getAssets(ws, prefix, shortName, submitId, testingId, test, handle))).map(_.map(r => r.test -> r).toMap)
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