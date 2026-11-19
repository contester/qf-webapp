package models

import actors.StatusActor
import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.testkit.{TestKit, TestProbe}
import models.ContesterResults.{CustomTestResult, FinishedTesting, SubmitObject}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike
import org.stingray.contester.dbmodel.{Contest, Team}
import org.stingray.qf.actors.{ProblemStateActor, TeamStateActor}

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
                 listening: TestProbe, removed: ConcurrentLinkedQueue[Int],
                 teamsHook: ConcurrentLinkedQueue[Int] = new ConcurrentLinkedQueue[Int]())

  /** Starts a listener with probes as targets, waits until it listens, runs the body, then stops it. */
  private def withListener(body: Rig => Unit): Unit = withListenerCatchingUp(None)(body)

  private def withListenerCatchingUp(catchUp: Option[NotifyListener.CatchUp])(body: Rig => Unit): Unit = {
    assume(fx.url.isDefined)
    fx.withFixture {
      val status = TestProbe()
      val teams = TestProbe()
      val listening = TestProbe()
      system.eventStream.subscribe(listening.ref, classOf[NotifyListener.Listening])
      val contests = new ConcurrentLinkedQueue[Contest]()
      val removed = new ConcurrentLinkedQueue[Int]()
      val teamsHook = new ConcurrentLinkedQueue[Int]()
      val settings = NotifyListener.ConnectionSettings(fx.url.get, fx.user, fx.password, appName)
      val listener = system.actorOf(NotifyListener.props(settings, fx.db,
        NotifyListener.Targets(status.ref, teams.ref, c => { contests.add(c); () }, id => { removed.add(id); () },
          id => { teamsHook.add(id); () }, catchUp)))
      try {
        listening.expectMsgType[NotifyListener.Listening](10.seconds)
        body(Rig(listener, status, teams, contests, listening, removed, teamsHook))
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
      assert(got == FinishedTesting(SubmitObject(submit.toInt, fx.kid, fx.round, "Z"), testing.toInt, compiled = true, passed = 3, taken = 3, points = Some(10.0)))
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

  test("round_changed delivers ContestChanged for a current round and ContestRemoved for a round with null times") {
    withListener { rig =>
      val r = fx.round
      exec(sqlu"update contest_rounds set ends_at = timestamptz '2030-01-01 15:00:00+00' where id = $r")
      val got = rig.status.expectMsgType[StatusActor.ContestChanged](2.seconds)
      assert(got.c.id == r)
      assert(got.c.endTime.getMillis == new org.joda.time.DateTime(2030, 1, 1, 15, 0, org.joda.time.DateTimeZone.UTC).getMillis)
      rig.teams.expectMsg(2.seconds, StatusActor.TeamsChanged(r))
      awaitCond(rig.contests.size == 1, 2.seconds)
      assert(rig.contests.peek().id == r)
      val n = fx.nullRound
      exec(sqlu"update contest_rounds set name = name where id = $n")
      rig.status.expectMsg(2.seconds, StatusActor.ContestRemoved(n))
      rig.teams.expectMsg(2.seconds, StatusActor.TeamsChanged(n))
      awaitCond(rig.removed.contains(n), 2.seconds)
      rig.status.expectNoMessage(300.millis)
      assert(rig.contests.size == 1)
    }
  }

  test("a round that becomes invalid delivers ContestRemoved, once per notification") {
    withListener { rig =>
      val r = fx.round
      exec(sqlu"update contest_rounds set ends_at = null where id = $r")
      rig.status.expectMsg(2.seconds, StatusActor.ContestRemoved(r))
      rig.teams.expectMsg(2.seconds, StatusActor.TeamsChanged(r))
      awaitCond(rig.removed.size == 1 && rig.removed.peek() == r, 2.seconds)
      rig.status.expectNoMessage(300.millis)
      assert(rig.contests.isEmpty)
    }
  }

  test("a problem row change (teacher-web 00009 trigger) delivers ContestChanged for its round") {
    withListener { rig =>
      val (r, s) = (fx.round, fx.season)
      for (stmt <- Seq(
        sqlu"insert into problems (season_id, round_id, problem_id, name, tests, max_points) values ($s, $r, 'Y', 'Why', 1, 5)",
        sqlu"update problems set max_points = 6 where round_id = $r and problem_id = 'Y'",
        sqlu"delete from problems where round_id = $r and problem_id = 'Y'")) {
        exec(stmt)
        assert(rig.status.expectMsgType[StatusActor.ContestChanged](2.seconds).c.id == r)
        rig.teams.expectMsg(2.seconds, StatusActor.TeamsChanged(r))
        rig.status.expectNoMessage(300.millis)
      }
      assert(rig.contests.size == 3)
    }
  }

  test("deleting a round (teacher-web 00009 trigger) delivers ContestRemoved") {
    withListener { rig =>
      val r = fx.round
      exec(sqlu"delete from problems where round_id = $r") // still a valid round: one ContestChanged
      assert(rig.status.expectMsgType[StatusActor.ContestChanged](2.seconds).c.id == r)
      exec(sqlu"delete from round_credentials where round_id = $r")
      exec(sqlu"delete from contest_rounds where id = $r")
      rig.status.expectMsg(2.seconds, StatusActor.ContestRemoved(r))
      awaitCond(rig.removed.contains(r), 2.seconds)
      rig.status.expectNoMessage(300.millis)
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
      // the same TeamsChanged reaches the hook (the admin live monitor and the public board cache)
      assert(rig.teamsHook.toArray.toSeq == Seq(r, r))
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
        "round_changed" -> "x", "credential_changed" -> "1", "credential_changed" -> "a:b",
        "participant_changed" -> "1.5"))
        one(sql"select pg_notify($ch, $p)::text".as[String])
      rig.status.expectNoMessage(500.millis)
      rig.teams.expectNoMessage(100.millis)
      val id = fx.insertCustomTest()
      exec(sqlu"update custom_tests set finished_at = now() where id = $id")
      assert(rig.status.expectMsgType[CustomTestResult](2.seconds).id == id.toLong)
    }
  }

  test("the listener's connect properties work against the server: keep-alive options applied, factory loaded") {
    assume(fx.url.isDefined)
    val settings = NotifyListener.ConnectionSettings(fx.url.get, fx.user, fx.password, appName, NotifyListener.KeepAlive(45, 7, 4))
    val (u, p) = NotifyListener.connectProperties(settings)
    val c = java.sql.DriverManager.getConnection(u, p)
    try {
      def show(k: String) = { val rs = c.createStatement().executeQuery(s"show $k"); rs.next(); rs.getString(1) }
      assert((show("tcp_keepalives_idle"), show("tcp_keepalives_interval"), show("tcp_keepalives_count")) == (("45", "7", "4")))
    } finally c.close()
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

  test("no periodic refresh: TeamStateActor drops a removed round, ProblemStateActor re-reads problems on ContestChanged") {
    assume(fx.url.isDefined)
    fx.withFixture {
      val r = fx.round
      val teams = system.actorOf(TeamStateActor.props(fx.db))
      val problems = system.actorOf(ProblemStateActor.props(fx.db))
      try {
        val probe = TestProbe()
        def kids: Map[Int, Team] = {
          probe.send(teams, TeamStateActor.GetTeams(r))
          probe.expectMsgType[Map[Int, Team]](5.seconds)
        }
        def problemIds: Seq[String] = {
          probe.send(problems, ProblemStateActor.GetProblems(r))
          probe.expectMsgType[Seq[org.stingray.contester.dbmodel.Problem]](5.seconds).map(_.id)
        }
        awaitCond(kids.nonEmpty, 10.seconds)
        teams ! StatusActor.ContestRemoved(r)
        assert(kids.isEmpty)
        awaitCond(problemIds == Seq("Z"), 10.seconds)
        val s = fx.season
        exec(sqlu"insert into problems (season_id, round_id, problem_id, name, tests, max_points) values ($s, $r, 'Y', 'Why', 1, 5)")
        assert(problemIds == Seq("Z"), "problems are not polled")
        problems ! StatusActor.ContestChanged(Contest(r, "x", org.joda.time.DateTime.now, org.joda.time.DateTime.now, None, s))
        awaitCond(problemIds == Seq("Y", "Z"), 5.seconds, 50.millis)
        problems ! StatusActor.ContestRemoved(r)
        assert(problemIds.isEmpty)
      } finally {
        system.stop(teams)
        system.stop(problems)
      }
    }
  }

  test("after every (re)connect the listener catches up: board cache dropped, live monitor reloaded, rounds re-sent") {
    val reconnects = new java.util.concurrent.atomic.AtomicInteger
    val cache = new PublicBoardCache
    val live = TestProbe()
    val ghost = 2000000000 // a round the status actor still holds that is gone from the database
    val catchUp = NotifyListener.CatchUp(() => scala.concurrent.Future.successful(Seq(fx.round, ghost)), () => {
      reconnects.incrementAndGet()
      cache.invalidateAll()
      live.ref ! actors.LiveMonitorActor.ReloadAll
    })
    withListenerCatchingUp(Some(catchUp)) { rig =>
      val r = fx.round
      // The first connect catches up too.
      live.expectMsg(5.seconds, actors.LiveMonitorActor.ReloadAll)
      rig.status.fishForMessage(5.seconds) { case StatusActor.ContestRemoved(`ghost`) => true; case _ => false }
      rig.teams.fishForMessage(5.seconds) { case StatusActor.TeamsChanged(`ghost`) => true; case _ => false }
      awaitCond(rig.removed.contains(ghost), 2.seconds)

      fx.await(cache.getOrRender(r, "ru")(scala.concurrent.Future.successful("page")))
      assert(cache.cachedRounds == Set(r))
      val submit = fx.insertSubmit()
      val testing = fx.insertTesting(submit)
      exec(sqlu"update submits set testing_id = $testing where id = $submit")

      val pid = backends().head
      assert(one(sql"select pg_terminate_backend($pid)".as[Boolean]))
      awaitCond(!backends().contains(pid), 5.seconds, 10.millis)
      // Down (the reconnect waits 1 s): these notifications are lost.
      val newEnd = new org.joda.time.DateTime(2031, 2, 3, 4, 5, org.joda.time.DateTimeZone.UTC)
      exec(sqlu"update contest_rounds set ends_at = timestamptz '2031-02-03 04:05:00+00' where id = $r")
      exec(sqlu"update submits set tested = true, compiled = true, points = 3 where id = $submit")
      exec(sqlu"update testings set finished_at = now() where id = $testing")
      assert(backends().isEmpty, "the changes must happen while the listener is disconnected")

      rig.listening.expectMsgType[NotifyListener.Listening](10.seconds)
      live.expectMsg(5.seconds, actors.LiveMonitorActor.ReloadAll)
      rig.status.fishForMessage(5.seconds) {
        case StatusActor.ContestChanged(c) => c.id == r && c.endTime.getMillis == newEnd.getMillis
        case _ => false
      }
      awaitCond(rig.contests.toArray.exists { case c: Contest => c.id == r && c.endTime.getMillis == newEnd.getMillis }, 2.seconds)
      assert(cache.cachedRounds.isEmpty, "the public board cache is dropped")
      assert(reconnects.get == 2)
    }
  }
}
