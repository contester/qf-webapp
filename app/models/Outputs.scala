package models

import com.mongodb.casbah.gridfs.GridFS
import play.api.Logger
import utils.{PolygonProblemHandle, GridfsTools, GridfsContent}

import scala.concurrent.Future

case class ResultAssets(test: Int, input: Option[GridfsContent], output: Option[GridfsContent], answer: Option[GridfsContent])

object Outputs {
  def getOutput(fs: GridFS, shortName: String, submitId: Int, testingId: Int, test: Int): Future[Option[GridfsContent]] =
    GridfsTools.getFile(fs, s"submit/${shortName}/${submitId}/${testingId}/$test/output.txt", 1024)(Contexts.gridfsExecutionContext)

  def getInput(fs: GridFS, handle: PolygonProblemHandle, test: Int): Future[Option[GridfsContent]] =
    GridfsTools.getFile(fs, handle.inputName(test), 1024)(Contexts.gridfsExecutionContext)

  def getAnswer(fs: GridFS, handle: PolygonProblemHandle, test: Int): Future[Option[GridfsContent]] =
    GridfsTools.getFile(fs, handle.answerName(test), 1024)(Contexts.gridfsExecutionContext)

  def getAssets(fs: GridFS, shortName: String, submitId: Int, testingId: Int, test: Int, handle: PolygonProblemHandle) = {
    import Contexts.gridfsExecutionContext
    getOutput(fs, shortName, submitId, testingId, test).zip(getInput(fs, handle, test)).zip(getAnswer(fs, handle, test)).map {
      case ((optOutput, optInput), optAnswer) => ResultAssets(test, optInput, optOutput, optAnswer)
    }
  }

  def getAllAssets(fs: GridFS, shortName: String, submitId: Int, testingId: Int, tests: Seq[Int], handle: PolygonProblemHandle) = {
    import Contexts.gridfsExecutionContext
    Future.sequence(tests.map(test => getAssets(fs, shortName, submitId, testingId, test, handle))).map(_.map(r => r.test -> r).toMap)
  }
}