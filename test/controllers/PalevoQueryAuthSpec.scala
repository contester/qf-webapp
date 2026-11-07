package controllers

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.typesafe.config.ConfigFactory
import org.joda.time.DateTime
import org.scalatest.BeforeAndAfterAll
import org.stingray.contester.dbmodel.Contest
import org.scalatest.funsuite.AnyFunSuite
import palevo.query.{FakeSource, QueryConfig}
import play.api.Configuration
import play.api.http.{DefaultFileMimeTypes, FileMimeTypesConfiguration}
import play.api.i18n.{DefaultLangs, DefaultMessagesApi}
import play.api.libs.json.Json
import play.api.libs.typedmap.TypedMap
import play.api.mvc._
import play.api.mvc.request.{RemoteConnection, RequestFactory, RequestTarget}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class PalevoQueryAuthSpec extends AnyFunSuite with BeforeAndAfterAll {
  private val system = ActorSystem("PalevoQueryAuthSpec", ConfigFactory.parseString(
    """
      |akka.loglevel = WARNING
      |akka.stdout-loglevel = OFF
      |akka.use-slf4j = off
      |""".stripMargin))
  override def afterAll(): Unit = Await.result(system.terminate(), 5.seconds)

  private implicit val mat: Materializer = Materializer(system)
  private implicit val ec: ExecutionContext = system.dispatcher

  private val cc: ControllerComponents = {
    val parsers = PlayBodyParsers()
    DefaultControllerComponents(DefaultActionBuilder(parsers.defaultBodyParser), parsers, new DefaultMessagesApi(),
      new DefaultLangs(), new DefaultFileMimeTypes(FileMimeTypesConfiguration()), ec)
  }

  private val token = "s3cret-token"

  private def controller(token: Option[String]) =
    new PalevoQuery(cc, QueryConfig(token, (1 to 10).toSet), new FakeSource(contestList = Seq(Contest(1, "Contest 1", now.minusHours(2), now.minusHours(1), now.plusHours(1),
      now.plusHours(2), None, "en", schoolMode = false))))

  private def get(path: String, headers: (String, String)*): Request[AnyContent] =
    RequestFactory.plain.createRequest(RemoteConnection("127.0.0.1", secure = false, None), "GET",
      RequestTarget(path, path, Map.empty), "HTTP/1.1", Headers(headers: _*), TypedMap.empty, AnyContentAsEmpty)

  private def await[T](f: Future[T]): T = Await.result(f, 5.seconds)

  private val now = DateTime.now

  private def callContest(c: PalevoQuery, headers: (String, String)*): Result = {
    val path = "/api/palevo/v1/contest"
    val q = Map("audience" -> Seq("public"), "contests" -> Seq("*"))
    val r = RequestFactory.plain.createRequest(RemoteConnection("127.0.0.1", secure = false, None), "GET",
      RequestTarget(path + "?audience=public&contests=*", path, q), "HTTP/1.1", Headers(headers: _*), TypedMap.empty,
      AnyContentAsEmpty)
    await(c.contest(r))
  }

  private def body(r: Result): String = await(r.body.consumeData).utf8String

  test("no token configured: 404 with an empty body, even with a bearer header") {
    val c = controller(None)
    val r = callContest(c)
    assert(r.header.status == 404)
    assert(body(r).isEmpty)
    val r2 = callContest(c, "Authorization" -> s"Bearer $token")
    assert(r2.header.status == 404)
    assert(body(r2).isEmpty)
  }

  test("token set, no Authorization header: 401") {
    assert(callContest(controller(Some(token))).header.status == 401)
  }

  test("token set, wrong bearer: 401") {
    assert(callContest(controller(Some(token)), "Authorization" -> "Bearer wrong").header.status == 401)
  }

  test("token set, bearer of a different length: 401 without an exception") {
    val c = controller(Some(token))
    assert(callContest(c, "Authorization" -> s"Bearer ${token}x").header.status == 401)
    assert(callContest(c, "Authorization" -> s"Bearer ${token.dropRight(1)}").header.status == 401)
    assert(callContest(c, "Authorization" -> s"Bearer ${token * 50}").header.status == 401)
    assert(callContest(c, "Authorization" -> "Bearer ").header.status == 401)
    assert(callContest(c, "Authorization" -> "Bearer").header.status == 401)
    assert(callContest(c, "Authorization" -> token).header.status == 401)
  }

  test("token set, correct bearer: 200 JSON") {
    val r = callContest(controller(Some(token)), "Authorization" -> s"Bearer $token")
    assert(r.header.status == 200)
    assert(r.body.contentType.exists(_.startsWith("application/json")))
    assert((Json.parse(body(r)) \ "contests").as[Seq[play.api.libs.json.JsObject]].nonEmpty)
  }

  test("401 carries a JSON error and no token") {
    val r = callContest(controller(Some(token)), "Authorization" -> "Bearer wrong")
    val b = body(r)
    assert((Json.parse(b) \ "error").asOpt[String].exists(_.nonEmpty))
    assert(!b.contains(token))
  }

  test("QueryConfig: blank or missing token is None; contests default to 1..10") {
    assert(QueryConfig(Configuration.empty) == QueryConfig(None, (1 to 10).toSet))
    assert(QueryConfig(Configuration("palevo.query.token" -> "  ")).token.isEmpty)
    assert(QueryConfig(Configuration("palevo.query.token" -> "")).token.isEmpty)
    assert(QueryConfig(Configuration("palevo.query.token" -> "abc")).token.contains("abc"))
    assert(QueryConfig(Configuration("palevo.query.contests" -> Seq(2, 3))).allowedContests == Set(2, 3))
  }
}
