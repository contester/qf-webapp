# palevo query API (qf-webapp) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a token-authenticated, read-only JSON query API to qf-webapp (`/api/palevo/v1/*`) that lets palevo's chat bot answer contest, standings, team, problem, computer and printing questions, with the freeze applied per audience.

**Architecture:** A new package `palevo.query` holds pure pieces (scope parsing, contest selection, freeze `view`, name matching), a `QuerySource` trait (all data access) with a real implementation (live-monitor boards via the existing actor, plain SQL for activity/locations/print jobs, `PrinterAlertState` for printer problems), JSON writers, and one controller `PalevoQuery` that composes them. The controller is tested against a fake `QuerySource`; the real source is tested like the existing `*DbSpec` tests.

**Tech Stack:** Scala 2.12.20, Play (macwire wiring in `MyApplicationLoader`), Slick + `org.stingray.contester.dbmodel`, Akka actors, play-json, scalatest 3.2 (`AnyFunSuite`/`AnyFunSuiteLike`), sbt 1.10.

**Spec:** `/home/stingray/m/palevo/docs/superpowers/specs/2026-11-06-palevo-tools-design.md` (binding; read the "qf-webapp query API" and "Matching rules" sections first). This plan implements the qf-webapp half; palevo's half is `/home/stingray/m/palevo/docs/superpowers/plans/2026-11-06-tools-palevo.md`.

## Global Constraints

- Base path `/api/palevo/v1`, GET only, JSON only. Auth: `Authorization: Bearer <token>`, token from config `palevo.query.token`; **no token configured or blank → every endpoint answers 404** (disabled); wrong/missing token → 401; token compared in constant time (`java.security.MessageDigest.isEqual`) and never logged.
- Required query parameters on every contest/jury endpoint: `audience` = `public` | `all` (anything else → 400), `contests` = `*` or comma-separated ints (empty or non-numeric → 400). Optional `contest=<id>`.
- Allowed contest ids: config `palevo.query.contests` (list of ints), default `1 to 10`. Ids outside it are never returned or selectable, even for `contests=*` or `contest=11` (→ 400).
- Contest selection (spec): candidates = scope ∩ allowed; current = running (`start ≤ now < end`); if none running → the finished candidate with the latest end; else the upcoming candidate with the soonest start; else none. Endpoints return **all** current contests. `status`: `before` | `running` | `over`.
- Freeze in effect (per contest) ⇔ `freezeTime < endTime && now ≥ freezeTime && now < exposeTime`. `view` per contest: audience `public` + in effect → `"frozen"`; audience `all` + in effect → `"live-jury"`; otherwise `"no-freeze"`. Public uses the **Freezing** board (`JoinPublic`), `all` the **Real** board (`JoinAdmin(c, real = true)`).
- Envelope: `{"as_of": <ISO-8601 UTC>, "contests": [{"id","name","status","view", …}]}`; none selectable → `{"as_of":…,"contests":[],"note":"no contest"}`. Errors: `{"error":"<short English text>"}` with 400/401/403/404/5xx. Jury-only endpoints (`/computers`, `/printing`, and the `room` field of `/team`) answer 403 / omit the field for `audience=public`.
- Before a contest starts (`status = before`): no problem list, no standings rows, no problem stats (don't leak problems, as the public monitor does).
- Limits: standings `top` default 10, max 30; team candidates ≤ 5; computers ≤ 10; print jobs ≤ 10; `/problems` `window` default 15 min, max 2 h (`window=<n>m`).
- Public numbers during a freeze: `/problems` `solved`/`teams_attempted`/`attempts` come from the **frozen board's visible cells only**; `recent_submits` and `pending` count arrivals regardless of outcome. Under `all` all numbers are real.
- Read-only: no writes anywhere. Never touch or commit the pre-existing uncommitted change to `app/views/admin/printjobs.scala.html` (it belongs to the operator); stage files explicitly, never `git add -A`.
- Commit messages end with the line `Co-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>`.

## Review Focus

1. `contest=11`, `contests=11`, `contests=*` with a debug contest (id > 10) running: no data for ids outside the allowed set; explicit disallowed `contest` → 400. (Task 2, Task 8)
2. `audience=public` with a freeze in effect never carries real results or jury-only data: `/problems` counts, `/team` cells, `room`, `/computers`, `/printing`. (Tasks 5–8)
3. Team/computer query text that is empty, only punctuation, very long, contains regex metacharacters, mixes Latin/Cyrillic look-alikes, or is a bare number: no exception, no match-everything, ≤ 5 candidates. (Task 3)
4. No selectable contest / contest not started / contest over but not exposed: empty arrays or the correct `view`, no problem list before start. (Tasks 2, 5)
5. Missing, blank, wrong-length, or wrong bearer token; token unset → 404 with no hint of the feature. (Task 1)

---

### Task 1: Config, scope parsing, bearer auth, routes, wiring

**Files:**
- Create: `app/palevo/query/QueryConfig.scala`, `app/palevo/query/Scope.scala`, `app/controllers/PalevoQuery.scala` (skeleton with an `authed` action wrapper and a `GET /api/palevo/v1/ping` that returns `{"ok":true}` — removed again in Task 9), `test/palevo/query/ScopeSpec.scala`, `test/controllers/PalevoQueryAuthSpec.scala`
- Modify: `conf/routes`, `app/MyApplicationLoader.scala`

**Interfaces:**
- Produces:
  - `case class QueryConfig(token: Option[String], allowedContests: Set[Int])`; `object QueryConfig { def apply(c: play.api.Configuration): QueryConfig }` (keys `palevo.query.token`, blank → None; `palevo.query.contests` default `(1 to 10).toSet`).
  - `sealed trait Audience; case object Public extends Audience; case object Jury extends Audience`.
  - `case class Scope(audience: Audience, contests: Option[Set[Int]])` (`None` = `*`); `object Scope { def parse(audience: Option[String], contests: Option[String]): Either[String, Scope] }` (Left = short error text for a 400).
  - `class PalevoQuery(cc: ControllerComponents, cfg: QueryConfig, source: QuerySource)` — in this task `source` may be a placeholder trait declared in Task 4's file; declare the trait file empty-shaped now (`trait QuerySource`) and extend it in Task 4.

- [ ] **Step 1: Write failing tests.** `ScopeSpec`: `parse(Some("public"), Some("*"))` = `Right(Scope(Public, None))`; `Some("all")` + `Some("3,5")` = `Right(Scope(Jury, Some(Set(3,5))))`; missing/unknown audience, missing/empty/`"a,b"`/`"1,,2"` contests → `Left`. `PalevoQueryAuthSpec` (pattern of `LiveMonitorControllerSpec`: build `PalevoQuery` directly with `DefaultControllerComponents`, call the action with a `FakeRequest`-style request): token unset → 404 with empty body; token set + no header → 401; `Bearer wrong` → 401; `Bearer <token>` → 200 `{"ok":true}`; header with different length → 401 (no exception).
- [ ] **Step 2: Run** `sbt "testOnly palevo.query.ScopeSpec controllers.PalevoQueryAuthSpec"`; expected FAIL (types undefined).
- [ ] **Step 3: Implement** the types above. `authed` is a `Request`-level check returning `Result` (404/401) before delegating; use `MessageDigest.isEqual` on UTF-8 bytes. Add routes `GET /api/palevo/v1/ping controllers.PalevoQuery.ping` (under the existing routes style; no CSRF/session). Wire in `MyComponents`: `lazy val queryConfig = QueryConfig(configuration)`; `lazy val palevoQueryController = wire[_root_.controllers.PalevoQuery]` (the placeholder `QuerySource` gets a `NoSource` instance until Task 4). No other component changes.
- [ ] **Step 4: Run** the same command; expected PASS. Then `sbt compile` to confirm macwire wiring.
- [ ] **Step 5: Commit** `palevo query API: config, scope parsing and bearer auth`.

### Task 2: Contest selection and freeze `view` (pure)

**Files:**
- Create: `app/palevo/query/ContestSelection.scala`, `test/palevo/query/ContestSelectionSpec.scala`

**Interfaces:**
- Consumes: `Scope`, `Audience` (Task 1); `org.stingray.contester.dbmodel.Contest` (fields: id, name, start, freeze, end, expose times — see its use in `LiveMonitorControllerSpec`: `Contest(id, name, startTime, freezeTime, endTime, exposeTime, None, "en", schoolMode = false)`).
- Produces:
  - `sealed trait View { def json: String }` with `Frozen` ("frozen"), `LiveJury` ("live-jury"), `NoFreeze` ("no-freeze").
  - `object ContestSelection { def freezeInEffect(c: Contest, now: DateTime): Boolean; def viewOf(c: Contest, a: Audience, now: DateTime): View; def status(c: Contest, now: DateTime): String; def select(all: Seq[Contest], scope: Scope, allowed: Set[Int], now: DateTime, explicit: Option[Int]): Either[String, Seq[Contest]] }` (Left only for a disallowed/not-candidate `explicit`).

- [ ] **Step 1: Write failing tests** with fixed `now`: freeze rules table (no freeze when freezeTime ≥ endTime; before freeze; in effect; after expose); `viewOf` for both audiences in each state; `status`; `select`: scope `*` ∩ allowed excludes id 11; running contests (two overlapping) both returned in id order; none running → latest-ended finished (not the earliest); none finished → soonest upcoming; none at all → empty `Right(Nil)`; `explicit` = candidate narrows to it, `explicit = Some(11)` → `Left`; `explicit` not in scope → `Left`.
- [ ] **Step 2: Run** `sbt "testOnly palevo.query.ContestSelectionSpec"`; expected FAIL.
- [ ] **Step 3: Implement** as pure functions; freeze rule exactly as in Global Constraints.
- [ ] **Step 4: Run**; expected PASS.
- [ ] **Step 5: Commit** `palevo query API: contest selection and freeze view`.

### Task 3: Name matching (pure)

**Files:**
- Create: `app/palevo/query/Matching.scala`, `test/palevo/query/MatchingSpec.scala`

**Interfaces:**
- Produces:
  - `object Matching { def normalize(s: String): String; def computerMatches(query: String, computerName: String): Boolean; def teamScore(query: String, t: TeamNames): Option[Int] }` and `case class TeamNames(schoolShort: String, schoolFull: String, num: Int, teamName: String)` where `teamName` is the raw team name (`"Парафраз: Черабаев, Лёвина, Юнусов"`); `def rankTeams[A](query: String, items: Seq[A])(names: A => TeamNames): Seq[A]` (best first, at most 5).
- Rules (spec "Matching rules"): `normalize` = lowercase, drop everything but letters/digits, fold Latin look-alikes to Cyrillic (`a c e o p x y k m h t b` ↔ `а с е о р х у к м н т в`), `ё`→`е`. Computers: number in query (leading zeros ignored) must equal the trailing number of the name; if the query also has letters, the normalized letter prefix must prefix the normalized name; no number in the query → false. Teams: fields = `schoolShort+num` (`СГУ1`), `schoolShort`, self name (text before the first `:`), each surname (text after the first `:` split on `,`); score exact (3) > prefix (2) > substring (1) on the best field; empty/punctuation-only normalized query → no match; query longer than 100 chars is cut.

- [ ] **Step 1: Write failing tests:** computers `46`/`046`/`wi46`/`wi 46`/`WI46` match `wi46`, not `wi146` or `lin46` when query is `wi46`; `46` matches both `wi46` and `lin46`; `""`, `"!!"`, `"abc"` (no number) → false. Teams: `СГУ1`, `сгу 1`, Latin `CГУ1` all match school "СГУ" num 1 but not num 2; `парафраз` prefix, `Черабаев`, `лёвина` (ё) match team "Парафраз: Черабаев, Лёвина, Юнусов"; Latin-named `cheesedom` and `Sibiryakov` match "cheesedom: Lepa, Sibiryakov, Garaev"; ranking puts exact before substring; `""`, `".*"`, `"(("`, a 5000-char string, and `"%"` do not throw and do not match everything; results capped at 5.
- [ ] **Step 2: Run** `sbt "testOnly palevo.query.MatchingSpec"`; expected FAIL.
- [ ] **Step 3: Implement** with plain string operations (no regex built from user input).
- [ ] **Step 4: Run**; expected PASS.
- [ ] **Step 5: Commit** `palevo query API: team and computer name matching`.

### Task 4: `QuerySource` and its database/actor implementation

**Files:**
- Modify: `app/palevo/query/QuerySource.scala` (created as an empty trait in Task 1), `app/models/PrinterAlertState.scala` (add read accessor), `app/MyApplicationLoader.scala`
- Create: `app/palevo/query/DbQuerySource.scala`, `test/palevo/query/DbQuerySourceSpec.scala` (skipped unless `ADMINS_TEST_URL` is set, same convention as `test/models/TeamProblemSubmitsDbSpec.scala`), `test/palevo/query/FakeSource.scala` (in-memory fake used by the controller specs)

**Interfaces:**
- Produces (exact):
  ```scala
  case class ComputerRow(name: String, area: String, printer: String)
  case class PrintJobRow(id: Int, contest: Int, team: Int, computer: String, area: String, printed: Boolean, error: String, pages: Int, arrived: DateTime)
  case class PrinterProblem(agent: String, printer: String, condition: String)
  trait QuerySource {
    def contests(): Future[Seq[Contest]]
    def board(contestId: Int, real: Boolean): Future[Option[live.LiveBoard]]
    def problemNames(contestId: Int): Future[Map[String, String]]
    def lastSubmits(contestId: Int, teams: Seq[Int]): Future[Map[Int, DateTime]]
    def recentArrivals(contestId: Int, since: DateTime): Future[Map[String, Int]]
    def pendingCounts(contestId: Int): Future[Map[String, Int]]
    def teamRooms(contestId: Int, teams: Seq[Int]): Future[Map[Int, String]]
    def computers(): Future[Seq[ComputerRow]]
    def printJobs(contests: Seq[Int], team: Option[Int], area: Option[String], limit: Int): Future[Seq[PrintJobRow]]
    def printerProblems(): Seq[PrinterProblem]
  }
  ```
  and `PrinterAlertState.currentProblems: Seq[(String, String, String)]` (a synchronized snapshot of the `open` map).

- [ ] **Step 1: Write failing tests.** `PrinterAlertStateSpec` addition: after `isNews` of an open alert, `currentProblems` contains `(agent, printer, condition)`; after the matching cleared alert it is empty. `DbQuerySourceSpec` (DB-gated): with a contest database copy, `teamRooms` returns the area name of the computer of the team's latest submit and falls back to the latest print job; `lastSubmits` returns `submit_time_absolute` maxima; `recentArrivals` counts submits per problem since `since`; `pendingCounts` counts untested submits (`tested = false`); `computers` joins `computer_locations`↔`areas`; `printJobs` honours contests/team/area/limit and never returns data bytes. Without the env var these tests are skipped, so also add `FakeSource` (immutable maps in constructor) for the controller tests.
- [ ] **Step 2: Run** `sbt "testOnly models.PrinterAlertStateSpec palevo.query.DbQuerySourceSpec"`; expected FAIL (accessor missing; DB tests skipped).
- [ ] **Step 3: Implement.** `board`: `liveMonitor ? Snapshot(JoinPublic(id))` for `real = false`, `Snapshot(JoinAdmin(id, real = true))` for `true` (see `LiveStreams.snapshot`; ask timeout 1 s; `None` on failure or when the live monitor is disabled). `contests`: the same function the actor uses (`SlickModel.allContests.result`). `problemNames`: from `statusActorModel.problemClient` (or the problems table). Use plain SQL (`sql"…"` as in `InitialImports.getNetmapComputers`) for the submit/print/location queries against `submits (contest, team_id, problem, submit_time_absolute, tested, computer_id)`, `print_jobs (contest, team, computer_id, arrived, printed, error, pages)`, `computer_locations (location int → areas.id, name text, id inet)` and `areas (id, name, printer)` (confirmed in `/home/stingray/m/director/doc/contest2026_10_schema.sql`); every query is bounded by contest and a LIMIT. Wire `DbQuerySource` into `PalevoQuery` in `MyComponents`.
- [ ] **Step 4: Run** the same command; expected PASS (DB spec passes when the env var points at a contest DB copy, otherwise reports skipped). `sbt compile`.
- [ ] **Step 5: Commit** `palevo query API: data source over live boards, submits, locations and print jobs`.

### Task 5: `/contest` and `/standings`

**Files:**
- Create: `app/palevo/query/QueryJson.scala` (envelope + writers), `test/controllers/PalevoQueryContestSpec.scala`
- Modify: `app/controllers/PalevoQuery.scala`, `conf/routes`

**Interfaces:**
- Consumes: Tasks 1–4. Produces routes `GET /api/palevo/v1/contest` and `GET /api/palevo/v1/standings`; `QueryJson.envelope(now: DateTime, contests: Seq[JsObject]): JsObject` and the shared helper `PalevoQuery.withContests(request)(f)` that parses scope, selects contests (Task 2), builds the per-contest base object (`id`, `name`, `status`, `view`) and returns the `no contest` envelope when empty.
- Per contest `/contest` adds `start`, `end`, `freeze` (ISO), `elapsed_seconds`, `remaining_seconds` (≥ 0), `problems: [{id,name}]` (only when `status != before`). `/standings` adds `teams` (total count) and `rows: [{rank, team (display name), solved, penalty}]` limited by `top`; none when `before`.

- [ ] **Step 1: Write failing tests** (controller + `FakeSource`): public and jury requests for a frozen contest return the Freezing/Real board respectively (fake gives two different boards) and `view` `frozen`/`live-jury`; no freeze → `no-freeze`; contest `before` → no `problems`/`rows`; `top=100` → 30 rows, `top=0`/`top=x` → 400; two running contests → two array elements; `contests=11` with only ids > 10 → `contests: []`, `note`; `contest=11` → 400; missing `audience` → 400; every response has `as_of`.
- [ ] **Step 2: Run** `sbt "testOnly controllers.PalevoQueryContestSpec"`; expected FAIL.
- [ ] **Step 3: Implement** both actions with `withContests`; display name = `team.schoolFullNameWithNum` plus `teamName` as the monitor does (reuse the existing helper used in `LiveJson`).
- [ ] **Step 4: Run**; expected PASS.
- [ ] **Step 5: Commit** `palevo query API: /contest and /standings`.

### Task 6: `/team`

**Files:**
- Create: `test/controllers/PalevoQueryTeamSpec.scala`
- Modify: `app/controllers/PalevoQuery.scala`, `conf/routes`

**Interfaces:**
- Consumes: `Matching.rankTeams`, `QuerySource.board/lastSubmits/teamRooms`. Route `GET /api/palevo/v1/team?q=<text>|id=<teamId>`.
- Per contest adds `candidates: [{id, name, school}]` (≤ 5, ranked); when exactly one candidate (or `id` given and found) also `team: {id, name, rank, solved, penalty, problems: {<id>: {state: ok|fail|pending|empty, attempts, minutes?}}, last_submit_at?, seconds_since_last_submit?, room?}` where `room` appears only for audience `all` and only if known. Neither `q` nor `id` → 400. No match → `candidates: []`.

- [ ] **Step 1: Write failing tests:** `q=СГУ1` resolves one team and returns its row with cells from the board (`Solved`→ok with minutes, `Failed`→fail, `Pending`→pending, `Empty`→empty); two matches → `candidates` of 2 and no `team`; `id=<n>` works; a team in a different selected contest is found there and the result names its contest; `room` present for `all` + known room, absent for `public` even when known; unknown room → absent; query `""`, `".*"`, 5000 chars → no exception and no wide match (Review Focus 3); public + frozen: cell states come from the Freezing board (pending, not ok).
- [ ] **Step 2: Run** `sbt "testOnly controllers.PalevoQueryTeamSpec"`; expected FAIL.
- [ ] **Step 3: Implement** using board rows as the team universe (only teams in the selected contests).
- [ ] **Step 4: Run**; expected PASS.
- [ ] **Step 5: Commit** `palevo query API: /team`.

### Task 7: `/problems`

**Files:**
- Create: `test/controllers/PalevoQueryProblemsSpec.scala`
- Modify: `app/controllers/PalevoQuery.scala`, `conf/routes`

**Interfaces:**
- Route `GET /api/palevo/v1/problems?window=15m`. Per contest adds `problems: [{id, name, solved, teams_attempted, attempts, recent_submits, pending}]` computed per Global Constraints (board cells for `solved`/`teams_attempted`/`attempts`; `recentArrivals(contest, now - window)` and `pendingCounts` for the rest). `attempts` per team cell = `Solved(a,_)` → a, `Failed(a)` → a, `Pending(f)` → f (visible failures only), `Empty` → 0; `teams_attempted` counts non-`Empty` cells. `window` accepts `<n>m` (1–120); anything else → 400. None when `before`.

- [ ] **Step 1: Write failing tests:** counts from a fixed board with solved, failed, pending and empty cells; `window` default 15 and `window=300m` → 400; public + frozen: a post-freeze solve (hidden, shown pending in the Freezing board) is **not** counted as solved or attempts, while `recent_submits`/`pending` still count its arrival; jury + frozen uses the Real board and counts it (Review Focus 2).
- [ ] **Step 2: Run** `sbt "testOnly controllers.PalevoQueryProblemsSpec"`; expected FAIL.
- [ ] **Step 3: Implement.**
- [ ] **Step 4: Run**; expected PASS.
- [ ] **Step 5: Commit** `palevo query API: /problems`.

### Task 8: `/computers` and `/printing` (jury only)

**Files:**
- Create: `test/controllers/PalevoQueryJuryEndpointsSpec.scala`
- Modify: `app/controllers/PalevoQuery.scala`, `conf/routes`

**Interfaces:**
- `GET /api/palevo/v1/computers?q=<text>|area=<name>` → `{"as_of", "computers":[{name, area, printer}], "ambiguous": <bool>}` (≤ 10; `ambiguous` true when more than one distinct prefix/area matched; no `contests` key). `GET /api/palevo/v1/printing?team=<id>&area=<name>` → `{"as_of","contests":[{"id","name","status","view","print_jobs":[…≤10…]}],"printers":[{agent,printer,condition}]}` (printers global; jobs limited to the selected contests, so never ids outside the allowed set). `audience=public` → 403 `{"error":"jury only"}` for both, checked before any data access. Neither `q` nor `area` for `/computers` → 400.

- [ ] **Step 1: Write failing tests:** public → 403 on both (and `FakeSource` records that no data method was called); jury `q=46` returns `wi46`, with `ambiguous` when `wi46` and `lin46` exist; `area=` lists that room's computers; `q` junk (Review Focus 3) → empty list; `/printing` returns open printer problems and only jobs of the selected contests (a job in contest 11 is never returned); limit 10.
- [ ] **Step 2: Run** `sbt "testOnly controllers.PalevoQueryJuryEndpointsSpec"`; expected FAIL.
- [ ] **Step 3: Implement.**
- [ ] **Step 4: Run**; expected PASS.
- [ ] **Step 5: Commit** `palevo query API: /computers and /printing`.

### Task 9: Contract document, config docs, end-to-end leak matrix

**Files:**
- Create: `doc/palevo-query-api.md`, `test/controllers/PalevoQueryLeakSpec.scala`
- Modify: `app/controllers/PalevoQuery.scala` (remove `ping`), `conf/routes` (remove `ping`), README or the config sample where `palevo.url`/`palevo.token` are documented (find with `grep -rn "palevo.token" .`) to add `palevo.query.token` and `palevo.query.contests`

**Interfaces:** none new.

- [ ] **Step 1: Write failing test** `PalevoQueryLeakSpec`: for every endpoint × audience (`public`, `all`) × freeze state (none, in effect, exposed), assert: `public` responses contain none of the jury-only keys (`room`, `computers`, `printers`, `print_jobs`) and the endpoints that are jury-only answer 403; `view` equals the rule table; with `FakeSource` holding contest 11 running, no response mentions id 11; the token-unset case is 404 for every route.
- [ ] **Step 2: Run** `sbt "testOnly controllers.PalevoQueryLeakSpec"`; expected FAIL until the matrix holds (fix any gap in the controller, not in the test).
- [ ] **Step 3:** Write `doc/palevo-query-api.md`: auth, parameters, selection and `view` rules, each endpoint with a real example response, limits and error shapes (palevo's tool layer passes these bodies to the model verbatim, so keep field names stable and descriptive). Add the config keys to the docs. Remove `ping`.
- [ ] **Step 4: Run** `sbt test` (DB specs skipped without the env var) and `sbt compile`; expected all PASS. Manually run the app against a dev database if one is available and `curl` each endpoint with and without the token; report what was and was not exercised.
- [ ] **Step 5: Commit** `palevo query API: contract document, configuration and leak tests`.

---

## Self-review notes

- Spec coverage: auth/disabled (T1), contest selection + `view` (T2), matching (T3), data access incl. printer problems and room derivation (T4), `/contest` `/standings` (T5), `/team` incl. room and last submit (T6), `/problems` incl. frozen split (T7), jury endpoints (T8), contract doc and leak matrix (T9). Review Focus items map to T1–T8 tests.
- The `QuerySource` method names and the `View`/`Scope`/`Audience` types are used identically in T1–T9.
