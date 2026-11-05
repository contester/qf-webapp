# Live Monitor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** An interactive scoreboard for qf-webapp: pending submits shown as `?`, rows updated and moved in the browser over SSE, public and own-row-under-freeze variants, static files and palevo fed from it, all behind `monitor.disable_new_monitor`.

**Architecture:** A pure core (`app/live/`: cells, ranking, per-contest state applying events) with no Akka or DB, fully unit-tested; one `LiveMonitorActor` that owns the state, loads from the database, receives hook messages, broadcasts per-board SSE events, writes throttled static files, runs the cross-check and feeds palevo. Controllers pick a stream per audience (public / team / admin). The old `MonitorActor` runs only when the new monitor is disabled.

**Tech Stack:** Scala 2.12, Play 2.8 (Twirl, `play.api.libs.EventSource`), Akka actors/streams, Slick, scalatest `AnyFunSuite`, plain JS in `app/assets/js/main.js`. Tests: `sbt "testOnly live.<Spec>"` from `/home/stingray/m/qf-webapp`; no test needs a database.

**Spec:** `docs/superpowers/specs/2026-11-05-live-monitor-design.md`

## Global Constraints

- Config keys: `monitor.disable_new_monitor` (default `false`, so the live monitor is ON unless `true`); `monitor.static_interval` (default `30s`); `monitor.crosscheck_interval` (default `60s`, `0` disables); `monitor.public_contests` (default 1–10); `monitor.public_max_connections` (default 1000); `monitor.static_location` keeps its meaning.
- With `disable_new_monitor = true`: no live class is constructed, hooks are no-ops, the old `MonitorActor` feeds board, static files and palevo exactly as today.
- Cell texts: `?`/`?X` pending, `-X` failed, `+X` solved (X per existing `ACMCell.toString`). Scoring reuses `ACMScorer`; ranking reuses the existing tie rules (`notRated` last with rank `*`/0, ties share rank, next rank skips).
- Freeze: a submit is hidden on the freezing board iff `freezeTime < endTime && arrivedAt > freezeTime && now < exposeTime` (strict `>`, same as `afterFreeze`).
- Own-row overlay: only the team's own **cells** come from the real board; position, rank, solved/penalty and header counts stay freezing-board values.
- Public routes: `GET /public/monitor/:contestId` and `GET /public/monitor/:contestId/events`; unlisted id → 404; over the cap → 503; stream headers `Cache-Control: no-cache`, `X-Accel-Buffering: no`.
- Event types: `board`, `row`, `cells`, `ping`. Every connection starts with `board`; no replay.
- Live code never throws into submit handling, the RabbitMQ ack, or rejudge actions; hooks are fire-and-forget (`!`) wrapped in `Try`.
- Commit messages end with `Co-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>`.

## Review Focus

- A finished/rejudge event for a submit id the monitor never saw (race with the insert hook): loads that one row, does not crash (Task 4, Task 8).
- Two submits with the same arrival instant: order by `(arrivedAt, id)`, deterministic (Task 2).
- Admin edits `freeze_time`/`expose_time` mid-contest: both boards rebuild from memory with no stale flag (Task 4).
- An event for a team that is not in the contest's team list (not yet loaded / removed): ignored and logged, cross-check heals (Task 4).
- Rejudge requested while the submit is already pending, and requested twice: stays pending, finishes once (Task 4).

## File Structure

- `app/live/LiveModel.scala` — `SubmitRec`, `SubmitState`, `BoardKind`, `FreezeRules`, `LiveCell`, `LiveCells.cellOf`.
- `app/live/LiveBoard.scala` — `LiveRow`, `LiveBoard`, `RowChange`, ranking.
- `app/live/LiveContest.scala` — per-contest state and event application (`Delta`).
- `app/live/LiveJson.scala` — JSON encoding of events and the own-row overlay.
- `app/live/LiveMonitorConfig.scala`, `app/live/StaticThrottle.scala`, `app/live/LiveHooks.scala`.
- `app/actors/LiveMonitorActor.scala` — actor, broadcasts, timers, cross-check, joins.
- `app/views/livemonitor.scala.html`, `app/views/livetable.scala.html` — page shells and table.
- Modified: `app/models/Monitor.scala`, `app/palevo/StandingsTracker.scala`, `app/palevo/StandingsAnnouncer.scala`, `app/actors/StatusActor.scala`, `app/controllers/Application.scala`, `app/controllers/AdminApplication.scala`, `app/MyApplicationLoader.scala`, `app/assets/js/main.js`, `conf/routes`.
- Tests mirror under `test/live/`.

---

### Task 1: Configuration

**Files:** Create `app/live/LiveMonitorConfig.scala`, `test/live/LiveMonitorConfigSpec.scala`.

**Interfaces:**
- Produces: `case class LiveMonitorConfig(enabled: Boolean, staticInterval: FiniteDuration, crosscheckInterval: FiniteDuration, publicContests: Set[Int], publicMaxConnections: Int)` and `object LiveMonitorConfig { def apply(c: play.api.Configuration): LiveMonitorConfig }`. `crosscheckInterval == Duration.Zero` means disabled.

- [ ] **Step 1: Write the failing test** `LiveMonitorConfigSpec`: empty config gives `enabled = true`, `staticInterval = 30.seconds`, `crosscheckInterval = 60.seconds`, `publicContests == (1 to 10).toSet`, `publicMaxConnections == 1000`; `monitor.disable_new_monitor = true` gives `enabled = false`; `monitor.public_contests = [3]` gives `Set(3)`; `monitor.crosscheck_interval = 0s` gives `Duration.Zero`.
- [ ] **Step 2:** Run `sbt "testOnly live.LiveMonitorConfigSpec"`. Expected: FAIL (not compiled).
- [ ] **Step 3:** Implement with `configuration.getOptional[Boolean]`/`getOptional[FiniteDuration]`/`getOptional[Seq[Int]]` under `monitor.`.
- [ ] **Step 4:** Run the spec; expected PASS.
- [ ] **Step 5:** Commit `Live monitor: configuration`.

### Task 2: Cells and freeze rules

**Files:** Create `app/live/LiveModel.scala`, `test/live/LiveCellsSpec.scala`.

**Interfaces:**
- Produces:
  - `sealed trait SubmitState; case object Pending extends SubmitState; case class Done(compiled: Boolean, passed: Int, taken: Int) extends SubmitState`
  - `case class SubmitRec(id: Int, team: Int, problem: String, arrivedAt: DateTime, arrivedSeconds: Int, state: SubmitState)`; `object SubmitRec { def fromSubmit(s: models.Submit): SubmitRec }` (state `Done` iff `s.finished`)
  - `sealed trait BoardKind; case object Freezing extends BoardKind; case object Real extends BoardKind`
  - `case class FreezeRules(freezeTime: DateTime, endTime: DateTime, exposeTime: DateTime) { def hides(arrivedAt: DateTime, now: DateTime): Boolean }`; `object FreezeRules { def of(c: Contest): FreezeRules }`
  - `sealed trait LiveCell { def solved: Boolean; def text: String }` with `Empty`, `Failed(attempts: Int)`, `Solved(attempts: Int, seconds: Int)`, `Pending(failures: Int)`
  - `object LiveCells { def cellOf(recs: Seq[SubmitRec], kind: BoardKind, rules: FreezeRules, now: DateTime): LiveCell }` (records are the cell's own, unsorted; the function sorts by `(arrivedAt, id)`)

- [ ] **Step 1: Write the failing tests** in `LiveCellsSpec` (use a 10:00 start, 14:00 freeze, 15:00 end, 16:00 expose contest, `now` varied):
  - no records → `Empty`; one pending → `Pending(0)` text `"?"`; failed then pending → `Pending(1)` text `"?1"`.
  - compiled failure after 2 tests (`Done(true,1,2)`) → `Failed(1)` text `"-1"`; `Done(true,1,1)` (failed on first test) is ignored (matches `ACMScorer`), as is `Done(false,0,0)`.
  - success `Done(true,3,3)` at 600 s after one failure → `Solved(2, 600)` text `"+1"`; a pending record after the solve is ignored.
  - out of order: record 1 pending, record 2 success → `Pending(0)` (spec: first pending decides).
  - same `arrivedAt`, ids 5 and 6: order by id.
  - freezing board, record arrived 14:30 and judged success, `now` 15:30 → `Pending`; same record on `Real` → `Solved`; `now` 16:00 (exposed) → `Solved` on both; freeze before-arrival record judged late → real result on `Freezing`.
  - `FreezeRules(freeze = end)`: `hides` is always false.
- [ ] **Step 2:** Run `sbt "testOnly live.LiveCellsSpec"`; expected FAIL.
- [ ] **Step 3:** Implement. Fold records in sorted order with `ACMScorer` (convert `Done` via a `models.Submit` built by `SubmitRec`'s private helper); stop at the first record that is `Pending` or `hides(...)` on `Freezing` (a hidden record counts as pending). `Pending(failures)` carries the counted failures so far.
- [ ] **Step 4:** Run the spec; expected PASS.
- [ ] **Step 5:** Commit `Live monitor: cells and freeze rules`.

### Task 3: Ranking and the board

**Files:** Modify `app/models/Monitor.scala` (extract shared ranking); create `app/live/LiveBoard.scala`, `test/live/LiveBoardSpec.scala`.

**Interfaces:**
- Consumes: `LiveCell` (Task 2), `ACM.Score`.
- Produces:
  - In `Foo` (Monitor.scala), public: `def rowLess[S](a: (Team, S), b: (Team, S))(implicit ord: Ordering[S]): Boolean` (the existing comparator body) and `def ranks[S](sorted: Seq[(Team, S)]): Seq[Int]` (the existing `pullRank` rule); `groupAndRank` is rewritten to call both. Behaviour unchanged.
  - `case class LiveRow(team: Team, score: ACM.Score, cells: Map[String, LiveCell], rank: Int)`
  - `case class RowChange(team: Int, position: Int, row: LiveRow, rankFrom: Int, ranks: Seq[Int], solved: Map[String, Int])` (`ranks` covers positions `rankFrom` until `rankFrom + ranks.size`; `solved` holds only changed header counts)
  - `case class LiveBoard(problems: Seq[String], rows: Vector[LiveRow], solvedCounts: Map[String, Int])` with `def update(team: Team, cells: Map[String, LiveCell]): (LiveBoard, Option[RowChange])` and `def rowOf(teamId: Int): Option[LiveRow]`
  - `object LiveBoard { def build(problems: Seq[String], teams: Seq[Team], cells: Map[Int, Map[String, LiveCell]]): LiveBoard; def scoreOf(cells: Iterable[LiveCell]): ACM.Score }`

- [ ] **Step 1: Write the failing tests** in `LiveBoardSpec`:
  - equivalence: for several hand-built submit sets with no pending cells, `LiveBoard.build(...)` rows (team ids, ranks, scores) equal `ACM.calculateStatus(...)`'s rows (existing `Foo.groupAndRank` path).
  - ties share rank and the next rank skips (1,1,3); not-rated team shows rank 0 and sorts after rated teams with equal score.
  - `Pending` cells add nothing to the score.
  - `update` moving team from position 4 to 1: `RowChange.position == 0`, `rankFrom == 0`, `ranks` covers positions 0..3 and equals the new ranks of those rows; rows after position 3 unchanged.
  - `update` changing a cell without changing the score or order: `Some(change)` with a one-element range; unchanged cells and score → `None`.
  - `solved` in the change contains only problems whose count moved.
- [ ] **Step 2:** Run `sbt "testOnly live.LiveBoardSpec"`; expected FAIL.
- [ ] **Step 3:** Extract `Foo.rowLess`/`Foo.ranks` first and run the existing tests to confirm no regression; then implement `LiveBoard` (immutable `Vector`; update removes the row, inserts by binary/linear search with `rowLess`, recomputes ranks via `Foo.ranks`, derives the range as `min`/`max` of old and new position, compares ranks to the previous ones).
- [ ] **Step 4:** Run the spec and `sbt test`; expected PASS (DB-dependent specs stay canceled).
- [ ] **Step 5:** Commit `Live monitor: ranking and board`.

### Task 4: Per-contest state and events

**Files:** Create `app/live/LiveContest.scala`, `test/live/LiveContestSpec.scala`.

**Interfaces:**
- Consumes: Tasks 2–3.
- Produces:
  - `sealed trait BoardOut; case class RowOut(change: RowChange) extends BoardOut; case class WholeOut(board: LiveBoard) extends BoardOut`
  - `case class Delta(freezing: Seq[BoardOut], real: Seq[BoardOut], ownCells: Seq[(Int, Map[String, LiveCell])])`
  - `class LiveContest(initial: Contest, problems: Seq[String], teams: Seq[Team], subs: Seq[SubmitRec], now: DateTime)` (mutable, single-threaded) with:
    - `def board(kind: BoardKind): LiveBoard`; `def contest: Contest`; `def rules: FreezeRules`; `def records: Seq[SubmitRec]`
    - `def onArrived(rec: SubmitRec, now: DateTime): Delta`
    - `def onFinished(id: Int, compiled: Boolean, passed: Int, taken: Int, now: DateTime): Option[Delta]` (`None` = unknown id; caller loads the row and calls `onArrived` then `onFinished`)
    - `def onRejudge(id: Int, now: DateTime): Option[Delta]`
    - `def onTick(now: DateTime): Option[Delta]` (emits `WholeOut` on both boards when `now` crosses `startTime`, `freezeTime` or `exposeTime` since the last tick)
    - `def onContest(c: Contest, now: DateTime): Delta` (times changed → rebuild both boards from memory, `WholeOut` on both)
    - `def rebuilt(problems: Seq[String], teams: Seq[Team], subs: Seq[SubmitRec], now: DateTime): LiveContest`
    - `def differences(other: LiveContest): Seq[String]` (empty = equal; messages name board, team)

- [ ] **Step 1: Write the failing tests** in `LiveContestSpec`:
  - arrive → pending on both boards; finish success → solved; post-freeze arrival+finish changes the real board only (`Delta.freezing` empty) and yields an `ownCells` entry; at the expose tick both boards become equal and the tick returns `WholeOut`.
  - rejudge of a finished submit sets it pending, finish sets it back; rejudge of an already pending submit and a second rejudge are no-ops (still one pending, one finish ends it).
  - unknown id in `onFinished`/`onRejudge` → `None`; an event for a team not in `teams` returns an empty `Delta` and does not throw.
  - `onContest` with a later `freezeTime` makes a previously hidden submit visible on the freezing board without any database access.
  - **property test:** a seeded random sequence (≥200 events of arrive / finish / rejudge across 5 teams, 4 problems, with random arrival instants straddling the freeze) applied incrementally equals `rebuilt(...)` (via `differences`) after every step.
  - applying each emitted `RowOut` to the previous board snapshot reproduces the new board exactly (rank-range correctness).
- [ ] **Step 2:** Run `sbt "testOnly live.LiveContestSpec"`; expected FAIL.
- [ ] **Step 3:** Implement. State: `Map[Int, SubmitRec]` by id and `Map[(Int, String), Vector[SubmitRec]]` per cell; for each event recompute the affected cell for both kinds with `LiveCells.cellOf`, call `LiveBoard.update`, build `Delta`; `ownCells` entry when `Real` cells differ from `Freezing` cells and `rules` freeze is active.
- [ ] **Step 4:** Run the spec; expected PASS.
- [ ] **Step 5:** Commit `Live monitor: contest state and events`.

### Task 5: JSON protocol and own-row overlay

**Files:** Create `app/live/LiveJson.scala`, `test/live/LiveJsonSpec.scala`.

**Interfaces:**
- Consumes: Tasks 3–4.
- Produces `object LiveJson`:
  - `def board(b: LiveBoard, own: Option[(Int, Map[String, LiveCell])] = None): JsValue` — `{"problems":[..],"solved":{p:n},"rows":[{"team":id,"school":..,"name":..,"rank":"3","solved":n,"penalty":n,"cells":{p:{"t":"ok|fail|pend|empty","s":"+1","at":"00:10"}}}]}`; with `own`, that row's `cells` come from `own` while everything else stays.
  - `def row(c: RowChange, own: Option[Map[String, LiveCell]] = None): JsValue` — `{"team","pos","row":{..},"rankFrom","ranks":[..],"solved":{..}}`.
  - `def cells(team: Int, cells: Map[String, LiveCell]): JsValue`
  - `def event(name: String, js: JsValue): play.api.libs.EventSource.Event`

- [ ] **Step 1: Write the failing tests:** exact JSON for a small board (cell kinds `ok`/`fail`/`pend`/`empty`, `at` formatted with `SecondsToTimeStr`); `board(b, Some((7, realCells)))` differs from `board(b)` only in team 7's `cells`; `row(change, Some(cells))` keeps `pos`, `rank`, `solved`, `penalty` and replaces only `cells`; rank of a not-rated team is `"*"`.
- [ ] **Step 2:** Run `sbt "testOnly live.LiveJsonSpec"`; expected FAIL.
- [ ] **Step 3:** Implement with `play.api.libs.json.Json.obj`.
- [ ] **Step 4:** Run the spec; expected PASS.
- [ ] **Step 5:** Commit `Live monitor: JSON protocol`.

### Task 6: Palevo input type

**Files:** Modify `app/palevo/StandingsTracker.scala`, `app/palevo/StandingsAnnouncer.scala`; modify `test/palevo/StandingsTrackerSpec.scala`, `test/palevo/StandingsAnnouncerSpec.scala` (only to adapt construction); create `app/live/PalevoView.scala`, `test/live/PalevoViewSpec.scala`.

**Interfaces:**
- Produces: `trait StandingsView { def rows: Seq[StandingsRow] }`, `case class StandingsRow(team: Team, rank: Int, solved: Int, solves: Map[String, Int])` (`solves`: problem → `arrivedSeconds` of solved cells); `StandingsTracker.observe(contest, view: StandingsView, now)` and `StandingsAnnouncer.observe(contest, freezing: StandingsView, real: StandingsView, now)`; `implicit`/explicit adapters `StandingsView.of(status: AnyStatus)` (from `ACM.Status`, other statuses → empty view) and `PalevoView.of(board: LiveBoard)`.

- [ ] **Step 1: Write the failing test** `PalevoViewSpec`: feeding a `StandingsTracker` with `PalevoView.of(liveBoard)` and with `StandingsView.of(equivalentAcmStatus)` for the same submits yields identical events across a sequence of three observations (first solve, new leader), for both the public tracker and the jury tracker split through `StandingsAnnouncer`; `Pending` cells produce no events.
- [ ] **Step 2:** Run it; expected FAIL.
- [ ] **Step 3:** Implement the view type, change the tracker to use it, and update existing specs by wrapping their statuses with `StandingsView.of(...)`; their assertions must not change.
- [ ] **Step 4:** Run `sbt "testOnly palevo.* live.PalevoViewSpec"`; expected PASS.
- [ ] **Step 5:** Commit `Palevo: tracker takes a view of the standings`.

### Task 7: Static file throttle and writer

**Files:** Create `app/live/StaticThrottle.scala`, `test/live/StaticThrottleSpec.scala`.

**Interfaces:**
- Produces: `class StaticThrottle(interval: FiniteDuration)` (single-threaded) with `def markDirty(now: Long): Unit`, `def due(now: Long): Boolean`, `def written(now: Long): Unit` (clock in millis); and `object StaticFiles { def write(dir: File, contestId: Int, html: String): Unit }` (temp file + `ATOMIC_MOVE`, as in the old `MonitorActor`).

- [ ] **Step 1: Write the failing tests:** not due when clean; due immediately after the first `markDirty` when never written; after `written(t)`, a `markDirty` within the interval is not due until `t + interval` (trailing write), then due once; unchanged board marks nothing; `StaticFiles.write` leaves no `.new` file and the final file has the content (use a temp dir).
- [ ] **Step 2:** Run `sbt "testOnly live.StaticThrottleSpec"`; expected FAIL.
- [ ] **Step 3:** Implement.
- [ ] **Step 4:** Run the spec; expected PASS.
- [ ] **Step 5:** Commit `Live monitor: static file throttle`.

### Task 8: The actor

**Files:** Create `app/actors/LiveMonitorActor.scala`, `app/live/LiveHooks.scala`, `test/live/LiveMonitorActorSpec.scala` (Akka TestKit; check `build.sbt` for `akka-testkit`, add it in `Test` scope if absent).

**Interfaces:**
- Consumes: Tasks 1–7; `ProblemClient.getProblems(contest)(implicit Timeout)`, `TeamClient.getTeams(contest)`, `Submits.getContestSubmits(contest)` + `Submits.upliftSub`, `StandingsAnnouncer`, `PalevoSink`.
- Produces:
  - `object LiveMonitorActor { def props(db, cfg: LiveMonitorConfig, staticLocation: Option[File], teamClient: TeamClient, problemClient: ProblemClient, contests: () => Future[Seq[Contest]], renderStatic: (Contest, LiveBoard) => String, palevo: PalevoSink): Props }`
  - Messages: `Arrived(rec: SubmitRec)`, `Finished(contest: Int, id: Int, compiled: Boolean, passed: Int, taken: Int)`, `RejudgeRequested(contest: Int, id: Int)`, `JoinPublic(contest: Int)`, `JoinTeam(contest: Int, team: Int)`, `JoinAdmin(contest: Int, real: Boolean)`; each `Join*` is answered with `Source[EventSource.Event, NotUsed]` (first element is the `board` event).
  - `trait LiveHooks { def submitArrived(contest: Int, rec: SubmitRec): Unit; def finished(contest: Int, id: Int, compiled: Boolean, passed: Int, taken: Int): Unit; def rejudged(contest: Int, id: Int): Unit }`; `object NoopLiveHooks`; `class ActorLiveHooks(ref: ActorRef) extends LiveHooks` (each method is `Try(ref ! ...)`).

- [ ] **Step 1: Write the failing tests** (TestKit, fake `contests`/clients through small stubs): startup computes both boards before handling a message sent during the load (stash); `JoinPublic` first element is a `board` event whose JSON equals `LiveJson.board(freezing)`; `Arrived` then `Finished` produces `row` events on a subscribed stream; `JoinTeam` receives `cells` events for its own team and none for another team; `JoinAdmin(real = false)` never receives a value from the real board; an exception injected into one event (stub that throws) does not kill the actor and marks the contest for rebuild; cross-check with a deliberately corrupted live state logs a difference, replaces the state and emits `board`; with `crosscheck_interval = 0` no cross-check runs; static file is written once per interval when dirty and not when clean (inject a fake clock); palevo sink receives the announcements from a first solve; a submit with a rejudge in flight is excluded from the cross-check comparison (no false mismatch); a changed contest row from `contests` (edited `freeze_time`) reaches `onContest` without a database read of submits.
- [ ] **Step 2:** Run `sbt "testOnly live.LiveMonitorActorSpec"`; expected FAIL.
- [ ] **Step 3:** Implement following the `MonitorActor` pattern (`Stash` until loaded, `Concur.broadcast` for the freezing board, the real board and a `(contest, team)`-tagged overlay channel, `scheduler.scheduleWithFixedDelay` for ping/tick/static/cross-check, `Try` around each event, unknown-id events load one row with `Submits.getContestSubmits`-style query by id then apply). Only contests passed the `public_contests` check are joinable by `JoinPublic` (the controller also checks). Static files are written for contests with `id < 11` only when `staticLocation` is set, from the freezing board.
- [ ] **Step 4:** Run the spec; expected PASS.
- [ ] **Step 5:** Commit `Live monitor: actor`.

### Task 9: Hooks, wiring, old monitor off

**Files:** Modify `app/MyApplicationLoader.scala`, `app/models/Monitor.scala`, `app/actors/StatusActor.scala`, `app/models/StatusActorModel.scala`, `app/controllers/Application.scala`, `app/controllers/AdminApplication.scala`; create `test/live/LiveWiringSpec.scala`.

**Interfaces:**
- Consumes: Task 8.
- Produces: `MyComponents.liveConfig: LiveMonitorConfig`, `MyComponents.liveHooks: LiveHooks` (`NoopLiveHooks` when disabled), `MyComponents.liveMonitor: Option[ActorRef]`; `Monitor` takes `LiveMonitorConfig` and does **not** create `MonitorActor` when `enabled` (its `getMonitor` returns `Future.successful(None)` then); `StatusActor.props` gains a `LiveHooks` parameter and calls `liveHooks.finished(...)` on `FinishedTesting` before `sender() ! {}` (inside `Try`); `Application.submitPost` calls `liveHooks.submitArrived` after the insert (insert query changed to return `(id, submit_time_absolute)`; the arrival seconds come from `contest.startTime`); `AdminApplication` calls `liveHooks.rejudged` at the two `contester.submitrequests` sends (~l.182, ~l.193).

- [ ] **Step 1: Write the failing test** `LiveWiringSpec`: a pure test on a small factory `LiveWiring.build(cfg, ...)`: with `enabled = false` it returns `NoopLiveHooks` and no actor props; with `enabled = true` it returns `ActorLiveHooks`; `ActorLiveHooks` swallows a thrown exception from `ref ! ...` (stub `ActorRef`); old-monitor flag: `LiveWiring.startOldMonitor(cfg) == !cfg.enabled`.
- [ ] **Step 2:** Run it; expected FAIL.
- [ ] **Step 3:** Implement the factory and wire it in `MyComponents` (`lazy val`s so a disabled monitor constructs nothing). Make the insert return the DB timestamp (`returning` both columns). Pass hooks to `StatusActor`, `Application`, `AdminApplication`.
- [ ] **Step 4:** Run `sbt test`; expected PASS (DB specs canceled). Start the app locally if possible (`sbt run`) once with `monitor.disable_new_monitor = true` and once without; expected: both boot, old pages load in the first mode.
- [ ] **Step 5:** Commit `Live monitor: hooks and wiring`.

### Task 10: Controllers, routes, views, browser script

**Files:** Modify `conf/routes`, `app/controllers/Application.scala`, `app/controllers/AdminApplication.scala`, `app/assets/js/main.js`; create `app/controllers/PublicMonitor.scala`, `app/views/livemonitor.scala.html`, `app/views/livetable.scala.html`; create `test/controllers/LiveMonitorControllerSpec.scala`.

**Interfaces:**
- Consumes: Task 8 `Join*` messages, Task 5 JSON, Task 1 config.
- Produces routes: `GET /monitor/events` (`Application.monitorEvents`), `GET /admin/monitor/:contestId/events` (`AdminApplication.monitorEvents(contestId)`), `GET /public/monitor/:contestId` (`PublicMonitor.page`), `GET /public/monitor/:contestId/events` (`PublicMonitor.events`). `Application.monitorDefault` and `AdminApplication.monitor` render the live page when enabled and the old page otherwise. `staticlive.scala.html` (standalone page like `staticmonitor`, with the 60 s reload, for the `renderStatic` function passed to the actor); `livetable(contest, board: LiveBoard, currentTeam: Int, own: Option[(Int, Map[String, LiveCell])])` renders the same markup as `monitortable` for ACM. JS: `liveMonitor(eventsPath, tableId, currentTeamId)` in `main.js` handling `board`, `row`, `cells`, built on `listenV2`.

- [ ] **Step 1: Write the failing tests** (Play `ScalaTest`-less style via direct controller construction or `FakeRequest`; stub the actor with a TestProbe): `/public/monitor/11` → 404 and `/public/monitor/1` → 200 with `Content-Type: text/event-stream` on `/events`; stream headers `Cache-Control: no-cache` and `X-Accel-Buffering: no`; the `(max_connections + 1)`-th concurrent public stream → 503; the public routes need no session cookie (no `SecuredAction`); `livetable` of a board renders `?` for a pending cell, the highlight class for `currentTeam`, and the same cell markup as `monitortable` for solved/failed cells (shared fixture string); before `start_time` the public page shows no table body.
- [ ] **Step 2:** Run `sbt "testOnly controllers.LiveMonitorControllerSpec"`; expected FAIL.
- [ ] **Step 3:** Implement. The team route chooses `JoinTeam`; the admin route chooses `JoinAdmin(contest, real = account.canSeeAll(contest))` and keeps `AdminPermissions.withSpectate(contestId)`; the public controller enforces `publicContests` and a shared `AtomicInteger` connection count decremented on stream termination. The page renders the initial table server-side from the current snapshot and the script replaces the table body on the first `board` event. JS `row` handling: remove the row, insert at `pos`, replace cells, rewrite rank cells for `rankFrom..`, update header counts, keep the `info` class on the current team.
- [ ] **Step 4:** Run the spec; expected PASS. Manually: load the three pages against a local contest and watch a submit flow through (`?` then result).
- [ ] **Step 5:** Commit `Live monitor: routes, views and browser script`.

### Task 11: Leak and kill-switch verification

**Files:** Create `test/live/LiveLeakSpec.scala`; modify `README.md` (short section: config keys, how to disable).

- [ ] **Step 1: Write the failing tests:** for a contest mid-freeze with random events, every JSON event on the public stream equals the corresponding freezing-board event, byte for byte; a team stream differs from the public stream only in that team's `cells` and in `cells` events; a stream for `JoinAdmin(real = false)` equals the public stream; at the expose tick the team stream equals the real board.
- [ ] **Step 2:** Run `sbt "testOnly live.LiveLeakSpec"`; expected FAIL until the overlay helpers are exposed for the test.
- [ ] **Step 3:** Make the minimal changes needed (visibility only); document the switch in the README.
- [ ] **Step 4:** Run full `sbt test`; expected: all new specs pass, the 11 DB-dependent specs stay canceled, nothing else regresses.
- [ ] **Step 5:** Commit `Live monitor: leak tests and docs`.
