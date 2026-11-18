package models

import actors.StatusActor
import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.testkit.{TestKit, TestProbe}
import models.ContesterResults.{CustomTestResult, FinishedTesting, SubmitObject}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.{Contest, Team}
import org.stingray.qf.actors.TeamStateActor

import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
  * NotifyListener against the teacher-web schema and its NOTIFY triggers. Skipped unless QF_TEST_URL is set
  * (see SubmitInsertDbSpec). Probes stand in for the status and team actors. Uses a round of its own.
  */
class NotifyListenerDbSpec extends TestKit(ActorSystem("NotifyListenerDbSpec")) with AnyFunSuiteLike with BeforeAndAfterAll {
  private implicit val ec: ExecutionContext = system.dispatcher
  private val fx = new ContestFixture("Notify spec")

  import org.stingray.contester.dbmodel.MyPostgresProfile.api._
  import fx.{exec, one}

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  private val appName = "qf-notify-spec"

  private def backends(): Seq[Int] =
    fx.await(fx.db.run(sql"select pid from pg_stat_activity where application_name = $appName".as[Int]))

  case class Rig(listener: ActorRef, status: TestProbe, teams: TestProbe, contests: ConcurrentLinkedQueue[Contest],
                 listening: TestProbe)

  /** Starts a listener with probes as targets, waits until it listens, runs the body, then stops it. */
  private def withListener(body: Rig => Unit): Unit = {
    assume(fx.url.isDefined)
    fx.withFixture {
      val status = TestProbe()
      val teams = TestProbe()
      val listening = TestProbe()
      system.eventStream.subscribe(listening.ref, classOf[NotifyListener.Listening])
      val contests = new ConcurrentLinkedQueue[Contest]()
      val settings = NotifyListener.ConnectionSettings(fx.url.get, fx.user, fx.password, appName)
      val listener = system.actorOf(NotifyListener.props(settings, fx.db,
        NotifyListener.Targets(status.ref, teams.ref, c => { contests.add(c); () })))
      try {
        listening.expectMsgType[NotifyListener.Listening](10.seconds)
        body(Rig(listener, status, teams, contests, listening))
      } finally {
        system.eventStream.unsubscribe(listening.ref)
        listener ! PoisonPill
        // the dedicated connection goes away with the actor
        awaitCond(backends().isEmpty, 10.seconds, 100.millis)
      }
    }
  }

  test("a finished testing that is the submit's current one delivers exactly one FinishedTesting") {
    withListener { rig =>
      val submit = fx.insertSubmit()
      val testing = fx.insertTesting(submit)
      exec(sqlu"update submits set testing_id = $testing, tested = true, compiled = true, passed = 3, taken = 3, points = 10 where id = $submit")
      exec(sqlu"update testings set finished_at = now() where id = $testing")
      val got = rig.status.expectMsgType[FinishedTesting](2.seconds)
      assert(got == FinishedTesting(SubmitObject(submit.toInt, fx.kid, fx.round, "Z"), testing.toInt, compiled = true, passed = 3, taken = 3))
      rig.status.expectNoMessage(500.millis)
    }
  }

  test("a stale testing (the submit already points at a newer one) delivers nothing") {
    withListener { rig =>
      val submit = fx.insertSubmit()
      val old = fx.insertTesting(submit)
      val newer = fx.insertTesting(submit)
      exec(sqlu"update submits set testing_id = $newer where id = $submit")
      exec(sqlu"update testings set finished_at = now() where id = $old")
      rig.status.expectNoMessage(1.second)
    }
  }

  test("a finished custom test delivers CustomTestResult") {
    withListener { rig =>
      val id = fx.insertCustomTest()
      exec(sqlu"update custom_tests set tested = true, finished_at = now() where id = $id")
      assert(rig.status.expectMsgType[CustomTestResult](2.seconds) == CustomTestResult(id.toInt, fx.round, fx.kid))
      rig.status.expectNoMessage(300.millis)
    }
  }

  test("round_changed delivers ContestChanged for a current round and nothing for a round with null times") {
    withListener { rig =>
      val r = fx.round
      exec(sqlu"update contest_rounds set ends_at = timestamptz '2030-01-01 15:00:00+00' where id = $r")
      val got = rig.status.expectMsgType[StatusActor.ContestChanged](2.seconds)
      assert(got.c.id == r)
      assert(got.c.endTime.getMillis == new org.joda.time.DateTime(2030, 1, 1, 15, 0, org.joda.time.DateTimeZone.UTC).getMillis)
      awaitCond(rig.contests.size == 1, 2.seconds)
      assert(rig.contests.peek().id == r)
      val n = fx.nullRound
      exec(sqlu"update contest_rounds set name = name where id = $n")
      rig.status.expectNoMessage(1.second)
      assert(rig.contests.size == 1)
    }
  }

  test("credential_changed and participant_changed deliver TeamsChanged for the round") {
    withListener { rig =>
      val (r, k) = (fx.round, fx.kid)
      exec(sqlu"update round_credentials set unrated = 'not_rated' where round_id = $r and participant_id = $k")
      rig.teams.expectMsg(2.seconds, StatusActor.TeamsChanged(r))
      rig.teams.expectNoMessage(300.millis)
      exec(sqlu"update participants set grade = 10 where id = $k")
      rig.teams.expectMsg(2.seconds, StatusActor.TeamsChanged(r))
      rig.teams.expectNoMessage(300.millis)
      rig.status.expectNoMessage(100.millis)
    }
  }

  test("after its backend is terminated the listener reconnects and later notifications still arrive") {
    withListener { rig =>
      val pids = backends()
      assert(pids.size == 1)
      val pid = pids.head
      assert(one(sql"select pg_terminate_backend($pid)".as[Boolean]))
      rig.listening.expectMsgType[NotifyListener.Listening](10.seconds)
      assert(backends().size == 1 && backends().head != pid)
      val id = fx.insertCustomTest()
      exec(sqlu"update custom_tests set finished_at = now() where id = $id")
      assert(rig.status.expectMsgType[CustomTestResult](2.seconds).id == id.toLong)
    }
  }

  test("malformed or unknown payloads are skipped and the listener keeps going") {
    withListener { rig =>
      for ((ch, p) <- Seq("testing_finished" -> "abc", "testing_finished" -> "999999999", "eval_finished" -> "",
        "round_changed" -> "x", "round_changed" -> "999999999", "credential_changed" -> "1", "credential_changed" -> "a:b",
        "participant_changed" -> "1.5"))
        one(sql"select pg_notify($ch, $p)::text".as[String])
      rig.status.expectNoMessage(500.millis)
      rig.teams.expectNoMessage(100.millis)
      val id = fx.insertCustomTest()
      exec(sqlu"update custom_tests set finished_at = now() where id = $id")
      assert(rig.status.expectMsgType[CustomTestResult](2.seconds).id == id.toLong)
    }
  }

  test("TeamStateActor reloads one round on TeamsChanged") {
    assume(fx.url.isDefined)
    fx.withFixture {
      val (r, k) = (fx.round, fx.kid)
      val teams = system.actorOf(TeamStateActor.props(fx.db))
      try {
        val probe = TestProbe()
        def kids: Map[Int, Team] = {
          probe.send(teams, TeamStateActor.GetTeams(r))
          probe.expectMsgType[Map[Int, Team]](5.seconds)
        }
        awaitCond(kids.contains(k), 10.seconds)
        assert(kids(k).unrated.isEmpty)
        exec(sqlu"update round_credentials set unrated = 'not_rated' where round_id = $r and participant_id = $k")
        teams ! StatusActor.TeamsChanged(r)
        awaitCond(kids.get(k).exists(_.unrated.isDefined), 5.seconds, 50.millis)
        exec(sqlu"update round_credentials set is_active = false where round_id = $r and participant_id = $k")
        teams ! StatusActor.TeamsChanged(r)
        awaitCond(!kids.contains(k), 5.seconds, 50.millis)
      } finally system.stop(teams)
    }
  }
}
