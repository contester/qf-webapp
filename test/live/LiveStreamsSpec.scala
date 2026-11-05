package live

import actors.LiveMonitorActor.JoinAdmin
import controllers.LiveStreams
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path, Paths}
import scala.collection.JavaConverters._

class LiveStreamsSpec extends AnyFunSuite {
  test("only an account that can see everything gets the real admin stream") {
    assert(LiveStreams.adminJoin(7, canSeeAll = true) == JoinAdmin(7, real = true))
    assert(LiveStreams.adminJoin(7, canSeeAll = false) == JoinAdmin(7, real = false))
  }

  test("no code outside the actor and LiveStreams builds a real-board join") {
    val root = Paths.get("app")
    assert(Files.isDirectory(root), "tests must run from the project root")
    val files = Files.walk(root).iterator().asScala.filter(p => p.toString.endsWith(".scala") || p.toString.endsWith(".html")).toList
    assert(files.size > 50)
    val allowed = Set("app/actors/LiveMonitorActor.scala", "app/controllers/LiveStreams.scala")
    val offenders = files.filter { p =>
      val text = new String(Files.readAllBytes(p), "UTF-8")
      text.contains("LiveMonitorActor.JoinAdmin") || text.contains("JoinAdmin(") && text.contains("LiveMonitorActor")
    }.map(_.toString).filterNot(allowed)
    assert(offenders.isEmpty, offenders.toString)
    // The one place that builds it is called with the account's own permission.
    val admin = new String(Files.readAllBytes(Paths.get("app/controllers/AdminApplication.scala")), "UTF-8")
    assert(admin.contains("LiveStreams.adminJoin(contestId, request.identity.canSeeAll(contestId))"))
  }
}
