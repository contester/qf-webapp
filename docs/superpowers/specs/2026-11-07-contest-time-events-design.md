# Contest time events for palevo: countdowns, reschedules, one producer

Date: 2026-11-07. Covers qf-webapp (producer) and palevo (renderer). Related: `2026-11-05-live-monitor-design.md`,
palevo's `docs/superpowers/specs/2026-11-01-palevo-design.md`.

## Intent

palevo announces `contest_started`, `contest_frozen` and `contest_finished` (renamed below). People in the contest channels also want
to be told what is coming and what changed:

- a countdown before the start (60, 30, 20, 10, 5, 2, 1 minutes) and before the end (20, 10, 5, 1 minutes);
- a message when an operator moves a contest ("start time changed to 19:30"), so everybody navigates the
  announcements correctly.

Today the three phase events come from `ContestPhaseTracker`, fed by `StatusActor`'s 10-second database rescan, which is
a relic: contest edits are pushed (`ContestChanged`) to both `StatusActor` and the reactive `LiveMonitorActor`, and
the live monitor already feeds palevo its standings events. This work puts every contest-time event behind one tracker
and one producer.

## Goals

1. The three phase events are renamed to `contest_starting`, `contest_freezing` and `contest_ending` (were
   `contest_started`, `contest_frozen`, `contest_finished`) and gain an optional `in_minutes`: absent or 0 means
   "now", N > 0 means "in N minutes". One new event, `contest_rescheduled`. (Operator's decision: one event per
   phase instead of separate countdown types.)
2. One tracker class, `ContestTimeTracker` (the existing `ContestPhaseTracker`, extended), produces phase,
   countdown and reschedule events. It is a plain single-threaded class: `observe(contest, now): Seq[PalevoEvent]`.
3. **Exactly one host calls it.** `LiveMonitorActor` when the live monitor is enabled; `StatusActor` (as today)
   when it is disabled, switched by `LiveWiring.startOldMonitor(cfg)`.
4. Step 2, separate commit: replace the live monitor's 1-second `Tick` with deadline timers (below).

## Non-goals

- No type filter on palevo routes. Routes select by audience and contest only; every route that covers the contest
  receives the new events. (Earlier discussion assumed type filters; they do not exist.) A filter is a possible
  follow-up.
- No freeze or expose countdown. `contest_freezing` can carry `in_minutes` (same shape), but the tracker does not
  produce it yet; adding a `palevo.countdown.freeze` list later is a small change. Reschedule covers freeze changes.
- No `recent_announcements` chat tool (parked), no change to chat history.
- The 10-second rescan in `StatusActor` stays: other state (`contestStates`) uses it. Removing it is separate.
- No localisation of the messages; templates are English like the existing ones.

## Anti-goals (failure even if it "works")

- No announcement replay on application restart, or on flipping the live-monitor flag mid-contest: the first
  observation of a contest only records state (as today).
- Never two producers at once, never zero. A double would be absorbed by palevo's dedupe, but the switch must make it
  impossible.
- Never a countdown for a moment that has already passed or is passing in this tick; the phase event covers it.
- One edit is one announcement: a row change that moves four times (the "move contest" action rewrites start, freeze,
  end, expose in one transaction) produces one `contest_rescheduled` event, not four.
- A slow or down palevo must not block either actor (the sink is already fire-and-forget).

## Events (qf-webapp -> palevo)

All have audience public and the contest as `contest`. `at` is the moment the event is about (ISO-8601 UTC, no
millis); `ts` is when the event was produced.

| type | data | id |
|---|---|---|
| `contest_starting` | `{}` (now) or `{in_minutes: 10, at}` | now: `qf-contest_starting-<cid>`; ahead: `qf-contest_starting-<cid>-in<minutes>-<atMillis>` |
| `contest_freezing` | same | same pattern with `contest_freezing` |
| `contest_ending` | same | same pattern with `contest_ending` |
| `contest_rescheduled` | `{changes: [{what: "start"\|"freeze"\|"end", old: <ISO>, new: <ISO>}]}` | `qf-contest_rescheduled-<cid>-<nowMillis>` |

`in_minutes` is either absent/0 (then `at` is not sent) or > 0 (then `at` is required). The target time is in the ahead
ids on purpose: after a reschedule the same threshold may legitimately fire again for the new time, and palevo dedupes
by id. The "now" ids keep their current form apart from the renamed type.

### Rename and compatibility

- palevo accepts the old type names for one release and maps them on parse (`contest_started` ->
  `contest_starting`, `contest_frozen` -> `contest_freezing`, `contest_finished` -> `contest_ending`), so a
  qf-webapp that has not been deployed yet keeps working. Everything after `Parse` sees only the new names. Remove the
  mapping in a later release.
- A template override in `templates_dir` named after an old type (`contest_started.tmpl`, `.html.tmpl`) is still
  honoured when no file with the new name exists (my call: silently dropping a customised wording would be worse than
  a fallback). Deploy order: palevo first, then qf-webapp.
- Docs and tests that name the old types are updated (palevo design docs, `palevo-send` samples, the tests).

## Tracker behaviour

State per contest: last-seen row times (start, freeze, end, expose), the phase, and the set of fired countdown
thresholds for the current start and for the current end.

- **First observation** of a contest (process start, new contest, or the other host just took over): record the
  phase and the times; mark every countdown threshold already reached as fired; emit nothing.
- **Phase events** keep their current semantics (`phase > prev`, freeze only when freeze < end).
- **Countdown.** A threshold T fires when `0 < remaining <= T minutes` and it has not fired for this target time.
  The start countdown runs only before the start; the end countdown only between start and end. If one wake crosses
  several thresholds (late tick, restart gap), only the smallest crossed one fires and the larger ones are marked.
  Remaining time at or below zero never fires.
- **Reschedule.** If start, freeze or end differs from the last-seen row, emit one `contest_rescheduled` with the
  changed fields, then reset the fired sets against the new times (thresholds already reached under the new times are
  marked silently; later ones will fire). A change of expose time alone is not announced (my call: it is not part of
  the participant-facing schedule); an expose change together with others is simply not listed. Freeze is listed only
  when freeze is enabled (freeze < end) before or after the change.
- Phase never moves backwards: moving the start after the contest began announces the change but does not repeat
  `contest_starting` (now).
- Thresholds are configuration: `palevo.countdown.start = [60,30,20,10,5,2,1]`, `palevo.countdown.end = [20,10,5,1]`
  (minutes; defaults as listed, an empty list disables). Same place as `palevo.url`/`palevo.token`: deployment
  config, documented in README.md ("Configuration: palevo"); there is no checked-in application.conf.

## Hosts

- **Live monitor** (`LiveMonitorActor`): `observe(c, clock.now())` for every contest on `Tick` (events go to
  `palevoSink`), and on every `contestRow` (so an edit is announced at once, and so is a back-door database edit
  caught by the 5-minute poll) and in `add`. Uses the injected `LiveClock`, so tests drive time.
- **Old path** (`StatusActor`): the existing call in `NewMultiContestState`, taken only when
  `LiveWiring.startOldMonitor(liveConfig)`; passed in as a constructor argument like `liveHooks`. Countdown precision
  there is the 10 s rescan; edits still arrive at once via `ContestChanged`.
- **Test contests are skipped.** `copyContestsForTesting` rewrites contests 11..20 (start = now), so the three new
  events (countdowns and reschedules) are produced only for contests with id <= 10, the same bound the live
  monitor's static-file writer already uses (`cid < 11`). Phase events keep today's behaviour for every contest.
  The bound is one named constant in the tracker (decided by the operator for now; revisit if real contests above
  10 appear).

## palevo side

- `event.Types` is renamed as above plus `contest_rescheduled`; the legacy-name mapping lives next to it. Templates
  for the three renamed types branch on `in_minutes`; wording now: `Contest started`, `Scoreboard frozen`,
  `Contest finished` (unchanged); ahead: `Contest starts in 10 minutes (19:30)`, `Scoreboard freezes in 10 minutes
  (18:00)`, `Contest ends in 5 minutes (00:30)`. `contest_rescheduled` is a new template:
  `[Contest 12] Start time changed to 19:30 (was 18:00)`; with several changes, one clause each. `palevo-send` samples
  follow.
- Rendering times needs a zone: new top-level `timezone` in palevo.toml (IANA name, default the server's local zone;
  my call, independent of `chat.tools.timezone`). Times arrive as UTC and are converted in the renderer. Whether it
  applies on reload or needs a restart follows how the renderer is built today; document it either way.
- Validation in `decode`: `in_minutes` >= 0, and `at` required (non-zero) when it is > 0; at least one change for
  reschedules.
- Docs: event table in the palevo design doc, the webhook contract in qf-webapp's README/docs.

## Step 2: deadline timers instead of the 1 s tick (separate commit)

The tick exists to detect crossings of start/freeze/expose in `LiveContest.onTick` and, now, to drive the tracker.
Replace it with a single timer armed for the earliest future instant among: start, freeze, expose, end, and the
countdown thresholds. Re-arm on every `ContestChanged`, `add` and wake. Plus a slow heartbeat (30 s) so that a wall-clock
step or a suspended VM cannot strand the monitor. Both fire the same idempotent "check against now" handler, which is
today's `Tick` handler: the `Tick` message stays as the test entry point (`scheduled = false`). Rejudge expiry gets its
own timer. `ContestTimeTracker` exposes `nextDeadline(now)`; `LiveContest` exposes its boundaries. `LiveMonitor`
`crossed()` does not cover `endTime`; the tracker does.

## Testing

- Tracker unit tests with explicit instants: first observation silent; each threshold once; late tick fires only the
  smallest; restart mid-countdown; reschedule resets thresholds and re-fires with a new id; shift of all four times is
  one event; expose-only change silent; contest 11 gets no countdown or reschedule but still gets phase events; start moved after start; freeze disabled; thresholds empty.
- Wiring tests for both modes: exactly one producer (`startOldMonitor` style).
- `PalevoEventSpec`: JSON shape and ids of the renamed and the new events, in the now and the ahead form.
- `LiveMonitorActor` test with the fake clock: tick past a threshold produces the event; `ContestChanged` produces
  the reschedule at once.
- palevo: `event` parse tests (including the legacy-name mapping and that the old name never survives `Parse`), render tests (IRC and HTML) for each type, timezone conversion, bad data rejected.

## Left to the builder

Class and file names beyond those given, the exact message wording, the config key plumbing, how `StatusActor`
receives the switch, and the order of commits (tracker + qf-webapp wiring, palevo side, then step 2).
