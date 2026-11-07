# Contest Time Events Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** palevo announces contest countdowns (before start and before end) and contest reschedules, and every contest-time event comes from one tracker with exactly one host.

**Architecture:** `ContestPhaseTracker` becomes `ContestTimeTracker`, a single-threaded class `observe(contest, now): Seq[PalevoEvent]` producing phase, countdown and reschedule events. `LiveMonitorActor` hosts it when the live monitor is enabled; `StatusActor` hosts it otherwise (one switch in `LiveWiring`). The three phase events are renamed (`contest_started` -> `contest_starting`, `contest_frozen` -> `contest_freezing`, `contest_finished` -> `contest_ending`) and gain an optional `in_minutes`; palevo gains `contest_rescheduled`, templates and a render timezone. Part C replaces the monitor's 1 s tick with deadline timers.

**Tech Stack:** qf-webapp: Scala, Play, Akka, ScalaTest (`sbt "testOnly <spec>"`). palevo (`/home/stingray/m/palevo`): Go (`go test ./...`).

**Spec:** `docs/superpowers/specs/2026-11-07-contest-time-events-design.md` (this repo). Read it first.

**Deploy order:** palevo (Part A) before qf-webapp (Parts B, C): palevo answers 422 for an unknown event type and qf-webapp drops it.

## Global Constraints

- Thresholds (minutes): start `60,30,20,10,5,2,1`; end `20,10,5,1`. Config keys `palevo.countdown.start` / `palevo.countdown.end`; empty list disables; an invalid entry (<= 0) falls back to the default for that key with a logged warning.
- New events are produced only for contests with id <= 10 (`ContestTimeTracker.MaxAnnouncedContest = 10`); phase events stay for every contest.
- Event types: `contest_starting`, `contest_freezing`, `contest_ending` (renamed from `contest_started`, `contest_frozen`, `contest_finished`) and the new `contest_rescheduled`. Phase data: `{}` for "now" (id `qf-<type>-<cid>`) or `{in_minutes: N > 0, at: <ISO>}` for ahead (id `qf-<type>-<cid>-in<N>-<atMillis>`). Reschedule data `{changes:[{what:"start"|"freeze"|"end", old, new}]}`, id `qf-contest_rescheduled-<cid>-<nowMillis>`. Times ISO-8601 UTC without millis. Audience public.
- palevo accepts the old type names for one release (mapped in `event.Parse`; nothing after `Parse` sees them) and still honours a `templates_dir` override named after an old type when no file with the new name exists. The tracker emits only `contest_starting` and `contest_ending` countdowns; `contest_freezing` carries `in_minutes` in the format but no freeze countdown is produced.
- Order inside one `observe`: reschedule, then phase events, then countdown.
- First observation of a contest is silent; thresholds already reached are marked fired; phase never moves backwards.
- Expose-only changes are silent; freeze is listed only when freeze < end before or after the change.
- palevo has no route filter by type; do not add one.
- Commit messages end with the session's usual `Co-Authored-By` / `Claude-Session` trailers.

## Review Focus

1. Restart or live-monitor flag flip in the middle of a countdown: nothing is replayed (Task 5, Task 8).
2. Degenerate rows (end <= start, freeze >= end, zero-length contest): no exception, no countdown for an impossible window (Task 5).
3. A row edit that changes only the name (or only expose) must not produce `contest_rescheduled` (Task 6).
4. A late tick or restart gap crossing several thresholds fires only the smallest (Task 5).
5. Times near midnight or in another zone: palevo shows the date when it differs from the event's date, and converts to its `timezone` (Task 2).

---

# Part A: palevo (`/home/stingray/m/palevo`)

### Task 1: Rename the phase events, add `in_minutes`, register `contest_rescheduled`

**Files:**
- Modify: `internal/event/event.go` (`Types`), `internal/event/event_test.go` (`TestTypesComplete`: 12 -> 13)
- Modify: `cmd/palevo-send/main.go` (`samples`), `internal/render/render_test.go` (`fixtures`)

**Interfaces:**
- Produces: `event.Types` contains `contest_starting`, `contest_freezing`, `contest_ending`, `contest_rescheduled` and not the three old names; `event.legacyTypes map[string]string` (old -> new) applied inside `Parse`, so `Event.Type` is always a current name.

- [ ] **Step 1:** Tests: `TestTypesComplete` expects 13 types (three renamed, one added); `TestLegacyTypes`: `Parse` of each old name returns the new `Event.Type`, and the old names are not in `Types`; rename every `contest_started` in existing tests (`internal/api/api_test.go`, `internal/route/route_test.go`, `internal/event/event_test.go`, `internal/render/*_test.go`) to `contest_starting`, `contest_frozen` -> `contest_freezing`, `contest_finished` -> `contest_ending`. Fixtures/samples (`render_test.go` `fixtures`, `palevo-send` `samples`): `contest_starting` `{}`, `contest_freezing` `{}`, `contest_ending` `{}`, plus `contest_rescheduled` `{"changes":[{"what":"start","old":"2026-11-07T14:00:00Z","new":"2026-11-07T15:30:00Z"}]}`; a second fixture for the ahead form: `{"in_minutes":10,"at":"2026-11-07T15:30:00Z"}`.
- [ ] **Step 2:** `go test ./internal/event ./internal/render ./cmd/palevo-send` — expect FAIL (types missing / no template).
- [ ] **Step 3:** Rename the keys in `Types`, add `contest_rescheduled`, add `legacyTypes` and apply it in `Parse` before the `Types` check. Rename the six embedded templates (`git mv`). (Templates arrive in Task 2; `render.New` fails until then, so do Tasks 1 and 2 before running the render tests.)
- [ ] **Step 4:** Commit with Task 2.

### Task 2: Render the new events, in a configurable timezone

**Files:**
- Modify: `internal/render/render.go`
- Rename: `internal/render/templates/contest_{started,frozen,finished}.{tmpl,html.tmpl}` -> `contest_{starting,freezing,ending}.*` and edit them; Create: `contest_rescheduled.tmpl` and `.html.tmpl`
- Test: `internal/render/render_test.go`

**Interfaces:**
- Produces: `type Option func(*Renderer)`; `func WithLocation(*time.Location) Option`; `func New(overrideDir string, opts ...Option) (*Renderer, error)` (existing callers unchanged; default location `time.Local`).
- Template funcs (per renderer, so they know the location): `clock(t time.Time) string` -> `"15:04"`; `when(t, ref time.Time) string` -> `"15:04"` when `t` and `ref` fall on the same date in the location, else `"2 Jan 15:04"`. Global funcs: `minutes(n int) string` -> `"1 minute"` / `"5 minutes"`; `label(what string) string` -> `Start` / `Freeze` / `End`.
- Data struct in `decode` shared by the three phase types: `phaseData{InMinutes int \`json:"in_minutes"\`; At time.Time}` (valid: InMinutes >= 0, and At non-zero when InMinutes > 0; `{}` stays valid), and `contestRescheduled{Changes []struct{What string; Old, New time.Time}}` (valid: at least one change, every `What` in start/freeze/end, times non-zero).

Wording (plain), now / ahead: `Contest started` / `[Contest 12] Contest starts in 10 minutes (19:30)`; `Scoreboard frozen` / `Scoreboard freezes in 10 minutes (18:00)`; `Contest finished` / `Contest ends in 5 minutes (00:30)` (the "now" lines keep their current text and prefix); `[Contest 12] Start time changed to 19:30 (was 18:00)`; several changes joined with `; `. HTML: the existing templates' lead phrase for "now", a bold ahead phrase plus the shared `contest` line otherwise.
Override fallback: when loading `<type>.tmpl` / `<type>.html.tmpl` from `overrideDir`, if the file for a renamed type is absent, try the old name (`contest_started`, `contest_frozen`, `contest_finished`).

- [ ] **Step 1: Tests** in `render_test.go` (all failing first):
  - `TestContestStartingZone`: location `Europe/Saratov`, `contest_starting` with `{"in_minutes":10,"at":"2026-11-07T15:30:00Z"}` -> first line equals `[Contest 12] Contest starts in 10 minutes (19:30)`.
  - `TestMinutesPlural`: `in_minutes: 1` -> contains `in 1 minute (`; no trailing `s`.
  - `TestRescheduledDate`: Saratov (UTC+4), event ts `2026-11-07T10:00:00Z`, one start change old `2026-11-07T14:00:00Z` (18:00 on the 7th) -> new `2026-11-08T21:30:00Z` (01:30 on the 9th): line equals `[Contest 12] Start time changed to 9 Nov 01:30 (was 18:00)`; the old time is on the event's date so it has no date.
  - `TestRescheduledMultiple`: start and end changes -> one line, two clauses joined by `; `, `Start` before `End`.
  - `TestPhaseData`: `{}` renders the "now" text; `in_minutes: 10` without `at`, `in_minutes: -1` and a zero `at` return an error wrapping `errRequired`; `TestRescheduledBadData`: empty `changes`, unknown `what`, zero times return `errRequired`; TestBadData style.
  - `TestLegacyOverride`: an override dir holding only `contest_started.tmpl` is used for `contest_starting`; with both files present the new name wins.
  - `TestEveryTypeRenders` keeps passing with the new fixtures (both IRC and HTML).
- [ ] **Step 2:** `go test ./internal/render` — expect FAIL.
- [ ] **Step 3:** Implement options, funcs (build the per-renderer `FuncMap` in `New`), the `decode` cases (phase types share one struct) and the templates.
- [ ] **Step 4:** `go test ./internal/event ./internal/render ./internal/api ./cmd/palevo-send` — expect PASS.
- [ ] **Step 5:** Commit (Tasks 1 and 2): `palevo: rename the phase events, add in_minutes and contest_rescheduled`.

### Task 3: `timezone` setting

**Files:**
- Modify: `cmd/palevo/config.go` (`Config.Timezone string \`toml:"timezone"\``, validation in `loadConfig`, `restart` list in `reload`), `cmd/palevo/main.go:52`
- Modify: `packaging/config/palevo.toml` (commented sample), `docs/superpowers/specs/2026-11-01-palevo-design.md` (event table)
- Test: `cmd/palevo/config_test.go`, `cmd/palevo/reload_test.go`

**Interfaces:**
- Consumes: `render.WithLocation` (Task 2).
- Produces: `func (c Config) Location() *time.Location` (empty -> `time.Local`).

- [ ] **Step 1: Tests:** `TestTimezoneDefault` (empty -> `time.Local`); `TestTimezoneInvalid` (`timezone = "Mars/Base"` fails `loadConfig` with an error naming `timezone`); reload test: changing `timezone` yields the restart-only key `timezone` (same style as the `listen,dedupe_size` test at `reload_test.go:84`).
- [ ] **Step 2:** `go test ./cmd/palevo -run 'Timezone|Reload'` — expect FAIL.
- [ ] **Step 3:** Implement; pass `render.WithLocation(cfg.Location())` in `main.go`; add `timezone` to the restart-only list; sample line in `palevo.toml` (`# timezone = "Europe/Saratov"   # zone for times in announcements; default: the server's`); update the design docs' event tables (`2026-11-01-palevo-design.md:109`, `2026-11-01-palevo-matrix-design.md:114-116`) to the new names and `in_minutes`, and document the legacy names.
- [ ] **Step 4:** `go test ./...` — expect PASS.
- [ ] **Step 5:** Commit: `palevo: timezone setting for announced times`.

---

# Part B: qf-webapp (`/home/stingray/m/qf-webapp`)

### Task 4: Renamed phase events, reschedule event, countdown config

**Files:**
- Modify: `app/palevo/PalevoEvent.scala`; Create: `app/palevo/CountdownConfig.scala`
- Test: `test/palevo/PalevoEventSpec.scala`; Create: `test/palevo/CountdownConfigSpec.scala`

**Interfaces:**
- Produces:
  - Renamed from `ContestStarted` / `ContestFrozen` / `ContestFinished`: `final case class ContestStarting(contest: PContest, ts: DateTime, inMinutes: Int = 0, at: Option[DateTime] = None) extends PalevoEvent`, and the same shape for `ContestFreezing` and `ContestEnding`. `eventType` is the new name; `data` is `{}` when `inMinutes == 0`, else `{in_minutes, at}`; `id` per Global Constraints. Construction with `inMinutes > 0` and no `at` is a programming error (`require`).
  - `final case class TimeChange(what: String, from: DateTime, to: DateTime)` and `final case class ContestRescheduled(contest: PContest, ts: DateTime, changes: Seq[TimeChange]) extends PalevoEvent` (JSON keys are `old` and `new`).
  - `private[palevo] def PalevoEvent.iso(d: DateTime): String` (the existing UTC no-millis format).
  - `final case class CountdownConfig(start: Seq[Int], end: Seq[Int])`; `object CountdownConfig { val Default; def sanitize(xs: Seq[Int], default: Seq[Int]): Seq[Int]; def apply(cfg: play.api.Configuration): CountdownConfig }`. `sanitize` returns `xs` sorted descending and distinct, or `default` if any entry <= 0 (empty stays empty).

- [ ] **Step 1: Tests:** in `PalevoEventSpec`, JSON of each renamed event in the now form (`qf-contest_starting-12` etc., type, `{}`) and the ahead form (`qf-contest_starting-12-in10-<atMillis>`, `{in_minutes, at}`), and of `ContestRescheduled`, equals the shapes and ids from Global Constraints (fixed UTC instant). Update the existing assertions at `PalevoEventSpec.scala:72-74` to the new names; `require` failure for `inMinutes > 0` without `at`. `CountdownConfigSpec`: defaults; `sanitize(Seq(5, 20, 5), d) == Seq(20, 5)`; `sanitize(Seq(10, 0), d) == d`; `sanitize(Nil, d) == Nil`; `apply` reads `palevo.countdown.start`/`end` from a `Configuration.from(Map(...))`, and a missing key gives the default.
- [ ] **Step 2:** `sbt "testOnly palevo.PalevoEventSpec palevo.CountdownConfigSpec"` — expect FAIL.
- [ ] **Step 3:** Implement. A bad entry logs a warning via `play.api.Logger` and uses the default.
- [ ] **Step 4:** Same command — expect PASS.
- [ ] **Step 5:** Commit: `palevo: rename phase events, add in_minutes, reschedule event and countdown config`.

### Task 5: ContestTimeTracker, phases and countdown

**Files:**
- Rename: `app/palevo/ContestPhaseTracker.scala` -> `ContestTimeTracker.scala`; `test/palevo/ContestPhaseTrackerSpec.scala` -> `ContestTimeTrackerSpec.scala`
- Modify: `app/actors/StatusActor.scala` (type name only, for now)

**Interfaces:**
- Consumes: Task 4 classes.
- Produces: `final class ContestTimeTracker(countdown: CountdownConfig = CountdownConfig.Default)` with `def observe(c: Contest, now: DateTime): Seq[PalevoEvent]`; `object ContestTimeTracker { val MaxAnnouncedContest = 10 }`.

Rules (see spec "Tracker behaviour"): per contest keep recorded phase (max ever), last-seen start/freeze/end/expose, and fired sets for start and end countdowns. A threshold T is *reached* when `remaining <= T minutes`. Start countdown runs while the recorded phase is 0 and `now < start`; end countdown while recorded phase is 1 or 2, `now < end`, and `end` is after `start`. Of the reached, unfired thresholds only the smallest fires; all reached are marked.

- [ ] **Step 1:** Rename the class and, in the existing tests, the event types (`contest_started` -> `contest_starting`, `contest_frozen` -> `contest_freezing`, `contest_finished` -> `contest_ending`); the phase tests stay green. The countdown events are the same classes with `inMinutes > 0` and `at` set.
- [ ] **Step 2: Tests** (new, in `ContestTimeTrackerSpec`; contest starts 10:00, freeze 14:00, end 15:00 as in the existing helper):
  - `first observation at 09:53 is silent`; it marks 60, 30, 20, 10 as reached (remaining 7 min), so only 5, 2, 1 fire later.
  - `fires exactly when remaining == 5 minutes`: observe at 09:54:59 (nothing new) then 09:55:00 -> minutes 5.
  - each start threshold fires once as time advances minute by minute from 08:50 to 10:00; produces exactly `60,30,20,10,5,2,1`, then `contest_starting` (now).
  - `late tick`: first observe at 09:00, then 09:56 (remaining 4: reached 10 and 5) -> only minutes 5 fires; then 09:59 (reached 2 and 1) -> only minutes 1 fires.
  - `no start countdown once the contest has started`, `no countdown at or after the moment`: observe at 10:00 exactly -> `contest_starting` (now) only.
  - end countdown only between start and end: from 14:30 to 15:00 yields `20,10,5,1` then `contest_ending` (now); none before start.
  - `restart mid-countdown`: a new tracker first-observing at 09:53 stays silent; 09:55 fires 5, 09:58 fires 2, 09:59 fires 1.
  - degenerate rows: `end == start`, `freeze >= end`: no exception, no end countdown when end is not after start.
  - `contest 11` gets phase events but no countdown; `contest 10` gets both.
  - custom `CountdownConfig(Seq(3), Nil)` and `CountdownConfig(Nil, Nil)` (disabled).
  - ids: assert `qf-contest_starting-1-<startMillis>-10`.
- [ ] **Step 3:** `sbt "testOnly palevo.ContestTimeTrackerSpec"` — expect FAIL on the new tests.
- [ ] **Step 4:** Implement countdown inside `observe` (after phase events).
- [ ] **Step 5:** Same command — expect PASS. Commit: `palevo: ContestTimeTracker with start and end countdowns`.

### Task 6: Reschedule detection

**Files:** `app/palevo/ContestTimeTracker.scala`, `test/palevo/ContestTimeTrackerSpec.scala`

Rules: compare stored start/freeze/end with the new row (all four stored; update after comparing). Emit one `ContestRescheduled` with the changed fields among start, freeze (only if freeze < end before or after), end; nothing when only expose or name changed, or id > 10. After a reschedule, re-mark both fired sets against the new times. Recorded phase is untouched.

- [ ] **Step 1: Tests:**
  - start moved 18:00 -> 19:30 before the contest: one event, `changes == Seq(TimeChange("start", oldStart, newStart))`, emitted before any phase/countdown event of the same observe.
  - shift of all four times by +90 min: ONE event with start, freeze, end changes in that order.
  - end extended while running: one event with only `end`.
  - expose-only change: silent. Name-only change: silent. Freeze change while freeze disabled (freeze >= end both before and after): silent.
  - after a reschedule the countdown re-fires for the new time: start moved from 10:00 to 11:00 while at 09:40 -> thresholds `60` (remaining 80 -> not reached), later `30,20,...` fire; ids differ from the old ones.
  - moved start earlier past a threshold (09:50 now, new start 09:56): thresholds >= 6 are silent; `5`... fire as time passes.
  - start moved after the contest began: reschedule event, no second `contest_starting` (now), no start countdown.
  - contest 11: reschedule silent. Two contests independent.
- [ ] **Step 2:** `sbt "testOnly palevo.ContestTimeTrackerSpec"` — expect FAIL.
- [ ] **Step 3:** Implement. **Step 4:** PASS. **Step 5:** Commit: `palevo: announce contest reschedules`.

### Task 7: Exactly one host

**Files:**
- Modify: `app/live/LiveWiring.scala`, `app/actors/StatusActor.scala`, `app/models/StatusActorModel.scala`, `app/MyApplicationLoader.scala`, `app/actors/LiveMonitorActor.scala`
- Test: `test/live/LiveWiringSpec.scala`, `test/live/LiveMonitorActorSpec.scala` (using `LiveActorFixtures`)

**Interfaces:**
- Produces: `sealed trait TimeEventHost; case object LiveMonitorHost; case object StatusActorHost` and `LiveWiring.timeEventHost(cfg: LiveMonitorConfig): TimeEventHost` (monitor iff `cfg.enabled`).
- `StatusActor.props(db, palevo, teamLookup, liveHooks, timeEvents: Boolean, countdown: CountdownConfig)` and the same two trailing parameters on `StatusActorModel`; loader passes `LiveWiring.timeEventHost(liveConfig) == StatusActorHost` and `CountdownConfig(configuration)`.
- `LiveMonitorActor.props` / `propsWith` take a trailing `countdown: CountdownConfig = CountdownConfig.Default`; the actor owns `new ContestTimeTracker(countdown)`.

Monitor calls `observe(c, clock.now())`, sending results to `palevoSink`, each call in `Try` (a failure logs a warning and never stops the actor): in the `Tick` handler for every contest (`st.live.contest`), in `contestRow` inside the branch that applies a changed row, and in `add`. `StatusActor` keeps its call in `NewMultiContestState` only when `timeEvents`.

- [ ] **Step 1: Tests:**
  - `LiveWiringSpec`: `timeEventHost` is `LiveMonitorHost` when enabled and `StatusActorHost` when disabled (never both, via a `Set` of the two assertions).
  - `LiveMonitorActorSpec`, patterned on "palevo hears the first solve": (a) Fixture at 08:55 with the default contest (starts 10:00); advance the fake clock to 09:00 and send `Tick`: sink holds `contest_starting` with `in_minutes` 60; (b) send `ContestChanged` with the start moved: sink holds one `contest_rescheduled` immediately, without a Tick; (c) a contest with id 11 gets neither; (d) a sink that throws on the first call does not stop the actor (existing `ThrowOnceSink`); (e) first observation at startup is silent (no events for a monitor started at 09:30).
- [ ] **Step 2:** `sbt "testOnly live.LiveWiringSpec live.LiveMonitorActorSpec"` — expect FAIL.
- [ ] **Step 3:** Implement.
- [ ] **Step 4:** `sbt test` — expect PASS (all existing specs, including other `StatusActor`/`LiveMonitorActor` call sites).
- [ ] **Step 5:** Commit: `Contest time events come from the live monitor, from StatusActor only when it is off`.

### Task 8: Docs

**Files:** `README.md` ("Configuration: palevo")

- [ ] **Step 1:** Document `palevo.countdown.start` / `palevo.countdown.end` (defaults, empty disables, invalid falls back), the renamed event types and `in_minutes`, `contest_rescheduled`, the id <= 10 rule and which actor produces time events in each mode.
- [ ] **Step 2:** Commit: `README: contest time events`.

---

# Part C: deadline timers instead of the 1 s tick (separate commits, after Part B is verified)

### Task 9: `nextDeadline`

**Files:** `app/palevo/ContestTimeTracker.scala`, `app/live/LiveContest.scala`, tests in `ContestTimeTrackerSpec` and `test/live/LiveContestSpec.scala`

**Interfaces:**
- Produces: `object ContestTimeTracker { def nextDeadline(c: Contest, now: DateTime, countdown: CountdownConfig): Option[DateTime] }` — pure, over a row: the earliest instant strictly after `now` among start, freeze (if freeze < end), expose, end, and, for id <= 10, `start - T min` and `end - T min` for each configured T. Fired state is irrelevant (an early wake is harmless).

- [ ] **Step 1: Tests:** before start returns `start - 60 min`; at 09:00 returns 09:30 (T=30) not start; between 10:00 and 14:00 returns `end - 20 min` or freeze, whichever is earlier; after expose returns None; contest 11 ignores thresholds but keeps start/freeze/end/expose; disabled countdown config.
- [ ] **Step 2:** FAIL. **Step 3:** implement. **Step 4:** PASS. **Step 5:** Commit `ContestTimeTracker.nextDeadline`.

### Task 10: LiveMonitorActor sleeps until the next deadline

**Files:** `app/actors/LiveMonitorActor.scala`, `app/live/*` as needed, `test/live/LiveMonitorActorSpec.scala`, `README.md` (timings)

**Interfaces:**
- `LiveTimings`: replace `tick` with `heartbeat: FiniteDuration = 30.seconds`.
- `LiveMonitorActor.wakeDelay(deadline: Option[DateTime], now: DateTime, heartbeat: FiniteDuration): FiniteDuration` in the companion: `max(50 ms, deadline - now + 50 ms)`, capped at `heartbeat` when None.
- The `Tick` message and its handler stay (they are the idempotent check, and the test entry point with `scheduled = false`). `startTimers` arms a fixed-delay heartbeat with `Tick` and a single deadline timer (separate key) with `Tick`; after every `Tick`, `ContestChanged` and `add` the deadline timer is re-armed from the earliest `nextDeadline` over all contests (use both the tracker's rule and `LiveContest` boundaries so board rebuilds at expose are also covered). `expireRejudges()` runs inside the `Tick` handler (the heartbeat is fine-grained enough for a 30 minute age; spec said "own timer", this is a simplification).

- [ ] **Step 1: Tests:** `wakeDelay` unit tests (past deadline -> 50 ms; far deadline -> exact; None -> heartbeat); smoke test with `scheduled = true`, real `SystemLiveClock`, `heartbeat = 1.hour`, a contest starting in ~1 s: the sink receives `contest_starting` (now) within 3 s with no manual `Tick`; and a `ContestChanged` moving the start 1 s ahead re-arms: the new start fires `contest_starting` (now) in 3 s. Update existing tests that mention `timings.tick`.
- [ ] **Step 2:** FAIL. **Step 3:** implement. **Step 4:** `sbt test` PASS.
- [ ] **Step 5:** Commit `Live monitor wakes at the next deadline instead of every second`.

---

## Self-review

- **Spec coverage:** events/ids (Tasks 1-2, 4); tracker rules and thresholds (5, 6); exactly one host and fallback (7); test contests (5, 6, 7); palevo timezone/render/validation/docs (2, 3, 8); step 2 (9, 10); deploy order (header). Anti-goals: silent first observation (5, 7e), one event per multi-field edit (6), no past countdown (5).
- **Type consistency:** `ContestTimeTracker(countdown)`, `observe`, `CountdownConfig`, `TimeChange` used identically in Tasks 4-10; `TimeEventHost` only in 7.
- **Known soft spots for the executor:** the worked examples in Tasks 2 and 5 were computed by hand (Saratov is UTC+4 all year); re-check the arithmetic when encoding them.
