package models

import play.api.Logging
import play.api.libs.ws.WSClient
import protos.Assets.{Asset, TestingRecord}

import scala.concurrent.{ExecutionContext, Future}

case class ResultAssets(test: Int,
                        input: Option[Asset],
                        output: Option[Asset],
                        answer: Option[Asset],
                        testerOutput: Option[Asset],
                        testerError: Option[Asset],
                        solutionStderr: Option[Asset])

case class CompileOutput(stdout: Option[Asset], stderr: Option[Asset]) {
  private def renderAsset(a: Asset): Option[String] = {
    val body = a.data.toStringUtf8
    if (body.isEmpty && !a.truncated) None
    else if (a.truncated) Some(s"$body\n... [truncated, ${a.originalSize} bytes total]")
    else Some(body)
  }

  def combinedText: String =
    Seq(stdout.flatMap(renderAsset), stderr.flatMap(renderAsset)).flatten.mkString("\n")
}

object Outputs extends Logging {
  private[models] def parseAllAssets(bytes: Array[Byte]): Map[Int, ResultAssets] =
    TestingRecord.parseFrom(bytes).test.map { otr =>
      ResultAssets(
        otr.testId.toInt,
        otr.input,
        otr.output,
        otr.answer,
        otr.testerOutput,
        otr.testerError,
        otr.solutionStderr
      )
    }.map(r => r.test -> r).toMap

  private[models] def parseCompileAssets(bytes: Array[Byte]): Option[CompileOutput] =
    TestingRecord.parseFrom(bytes).test.find(_.testId == 0).map { t =>
      CompileOutput(t.testerOutput, t.testerError)
    }

  /** The bridge's /protopackage/ proxy finds the testing by its id alone (`contest` and `submit` are informational). */
  def getAllAssets2(ws: WSClient, prefix: String, shortName: String, submitId: Int, testingId: Int)
                   (implicit ec: ExecutionContext): Future[Map[Int, ResultAssets]] = {
    import scala.concurrent.duration._
    ws.url(s"${prefix}protopackage/")
      .withQueryStringParameters(
        "submit" -> submitId.toString,
        "testing" -> testingId.toString,
        "contest" -> shortName
      ).withRequestTimeout(30.seconds).get().map { resp =>
      resp.status match {
        case 200 => parseAllAssets(resp.bodyAsBytes.toArray)
        case _ =>
          logger.info(s"getAllAssets2($prefix, $shortName, $submitId, $testingId): ${resp.status} ${resp.statusText}")
          Map.empty[Int, ResultAssets]
      }
    }.recover { case e =>
      logger.warn(s"getAllAssets2($prefix, $shortName, $submitId, $testingId) failed", e)
      Map.empty[Int, ResultAssets]
    }
  }

  def getCompileAssets(ws: WSClient, prefix: String, shortName: String, submitId: Int, testingId: Int)
                      (implicit ec: ExecutionContext): Future[Option[CompileOutput]] = {
    import scala.concurrent.duration._
    ws.url(s"${prefix}protopackage/")
      .withQueryStringParameters(
        "submit" -> submitId.toString,
        "testing" -> testingId.toString,
        "contest" -> shortName,
        "test" -> "0"
      ).withRequestTimeout(15.seconds).get().map { resp =>
      resp.status match {
        case 200 => parseCompileAssets(resp.bodyAsBytes.toArray)
        case _ =>
          logger.info(s"getCompileAssets($prefix, $shortName, $submitId, $testingId): ${resp.status} ${resp.statusText}")
          None
      }
    }.recover { case e =>
      logger.warn(s"getCompileAssets($prefix, $shortName, $submitId, $testingId) failed", e)
      None
    }
  }
}