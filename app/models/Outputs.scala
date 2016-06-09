package models

import akka.stream.Materializer
import play.api.libs.ws.WSClient
import utils.{GridfsContent, GridfsTools, PolygonProblemHandle}

import scala.concurrent.Future

case class ResultAssets(test: Int, input: Option[GridfsContent], output: Option[GridfsContent], answer: Option[GridfsContent])

object Outputs {
  import Contexts.gridfsExecutionContext
  def getOutput(ws: WSClient, prefix: String, shortName: String, submitId: Int, testingId: Int, test: Int)(implicit mat: Materializer): Future[Option[GridfsContent]] =
    GridfsTools.getFile(ws, s"${prefix}submit/${shortName}/${submitId}/${testingId}/$test/output", 1024)

  def getInput(ws: WSClient, prefix: String, handle: PolygonProblemHandle, test: Int)(implicit mat: Materializer): Future[Option[GridfsContent]] =
    GridfsTools.getFile(ws, prefix + handle.inputName(test), 1024)

  def getAnswer(ws: WSClient, prefix: String, handle: PolygonProblemHandle, test: Int)(implicit mat: Materializer): Future[Option[GridfsContent]] =
    GridfsTools.getFile(ws, prefix + handle.answerName(test), 1024)

  def getAssets(ws: WSClient, prefix: String, shortName: String, submitId: Int, testingId: Int, test: Int, handle: PolygonProblemHandle)(implicit mat: Materializer) = {
    getOutput(ws, prefix, shortName, submitId, testingId, test).zip(getInput(ws, prefix, handle, test)).zip(getAnswer(ws, prefix, handle, test)).map {
      case ((optOutput, optInput), optAnswer) => ResultAssets(test, optInput, optOutput, optAnswer)
    }
  }

  def getAllAssets(ws: WSClient, prefix: String, shortName: String, submitId: Int, testingId: Int, tests: Seq[Int], handle: PolygonProblemHandle)(implicit mat: Materializer) = {
    Future.sequence(tests.map(test => getAssets(ws, prefix, shortName, submitId, testingId, test, handle))).map(_.map(r => r.test -> r).toMap)
  }
}