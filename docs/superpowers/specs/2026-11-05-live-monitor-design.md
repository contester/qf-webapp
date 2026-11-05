# Live monitor (interactive scoreboard) — design

Status: draft for approval. Decisions made by the user are stated as such; items marked **(call)** are the
author's choices and are open to change.

## Intent

Today the scoreboard ("monitor") is recomputed from scratch every 20 s by `MonitorActor` and shown by a full
page load. Teams see a stale board and nothing says "your submit is being judged". The live monitor shows
pending submits as `?` immediately, updates the table in the browser without a reload as submits arrive and
finish, and still goes dark safely: if it misbehaves during a contest the operator disables it in config,
restarts qf, and the existing board is back untouched.

## Goals

- Show a submit as pending the moment it is inserted, and update the cell when judging finishes.
- Keep two boards per contest: the **freezing board** (contestants, static files) and the **real board**
  (admins who may see past the freeze). Both update from the same events.
- After the initial load, updates stream to the browser (Server-Sent Events) and patch the table in place.
- Under the freeze, a logged-in team sees the real result of its own submits in its own row (it knows them
  anyway) while its position, rank and scores stay those of the freezing board.
- An unauthenticated public page and stream of the freezing board, fully dynamic like the rest.
- Keep serving static HTML copies of the freezing board if qf is down.
- Be safe to leave on: a bug in the live path must never break submitting, judging acks, or the old board.
- A periodic full recompute cross-checks the live state at a configurable interval.

## Non-goals

- School mode (`MonitorSchool`, Rational scoring). Same as today, only ACM scoring is supported.
- Per-test judging progress. A submit is either pending or finished.
- Changing which palevo events exist or what they say. Only the component that feeds them changes (see Palevo).
- Removing the hardcoded `contest.id < 11` static-file limit or the 20 s old refresh.
- Scala.js. The browser side is plain JavaScript in `app/assets/js/main.js` (user decision).
- Event replay or sequence numbers. Reconnect = full board again (user decision).

## Anti-goals (would be a failure even if it works)

- Any real-board data reaching a contestant stream, a public page, or a static file, other than a team's own
  row cells on that team's own stream. Static files and the public stream never carry the own-row overlay.
- The live path throwing into, blocking, or slowing the submit request, the RabbitMQ ack in `StatusActor`,
  or the rejudge actions.
- Live state silently diverging from the database with nobody noticing.

## Scale

Contests 1–2: 160 teams. Contests 3–4: 70 teams. Expect ~160 team connections plus ~30 admin connections at
once. One in-memory broadcast per board is enough: no batching or fan-out service (user: "rookie numbers",
the existing event streams already handled this last year).

## Configuration (user decisions)

- `monitor.disable_new_monitor` (boolean, default `false`). The live monitor is **on unless this is `true`**.
  When `true`, none of the live code is constructed, all hooks are no-ops, and the pages and static files
  behave exactly as before this feature.
- `monitor.static_interval` (duration, default `30s`): static files are written at most this often.
- `monitor.crosscheck_interval` (duration): how often the full recompute is compared with the live state.
  Default `60s`; `0` disables. **(call: default value)**
- `monitor.static_location` already exists and keeps its meaning.

Disable procedure: set `monitor.disable_new_monitor = true`, restart qf.

## Cell model

A cell is the state of one (team, problem), derived from that team's submits for that problem in arrival
order, using the existing `ACMScorer` for finished submits so scoring stays identical to today.

Display states:

| State | Shown | Meaning |
|---|---|---|
| empty | blank | no counted attempts, nothing pending |
| failed | `-X` | X counted attempts, none solved |
| solved | `+X` and time | solved; X = failed attempts before the solve |
| pending | `?X` | an attempt is waiting for a result; X = counted failures before it (`?` when X = 0) |

(`X` formatting follows the current `ACMCell.toString`; the user defined `?X` as "the `-X` the cell would
otherwise show".)

Rules:

- Walk the team's submits for the problem in arrival order. The first pending submit decides: the cell is
  `?X` with X counted so far, and nothing after it is counted yet. **(call)** This means a success judged out
  of order is not counted until earlier submits finish, because an earlier success would change the solve time.
- A pending submit after a counted solve is ignored (the cell is already solved).
- A pending cell contributes nothing to the team's score (solved count / penalty); only the text changes.
  **(call)**

### What "pending" means on each board

- **Real board:** a submit is pending from insertion until its testing finishes.
- **Freezing board:** same, **and additionally** every submit that arrived after `freeze_time` is pending
  until `expose_time`, whatever its judged result. This is a deliberate change: today the frozen board
  drops post-freeze submits entirely.
- A submit that arrived before the freeze and finishes (or is rejudged) after the freeze is shown with its
  real result on the freezing board. This is intentional (user: freeze covers only the last hour; rejudges
  of pre-freeze submits legitimately change the frozen board).
- A contest with `freeze_time = end_time` has no freeze (`Contest.frozen` already treats it that way, commit
  `f4503bc` in contester-dbmodel).
- At `expose_time` the freezing board becomes equal to the real board (sent as a whole-board event).

## Ranking

Reuse the existing ordering and tie rule (`Foo.groupAndRank` / `ACM.Score`): more solved, then lower penalty,
then lower last-submit penalty; ties share a rank and the next rank skips; rated teams before not-rated
teams, then by `teamFullName`; not-rated teams show rank `*`. The order is a total order over teams, so
every team has exactly one position.

The incremental update and the batch computation must share one implementation of cells, scores and ordering
(a pure function from submits/teams/problems to a board), so a rebuild can never disagree with the
incremental path by construction. **(call)**

## Memory model (user decision: keep submits per team and problem)

The live monitor keeps, per contest, enough in memory to apply any event without reading the database and
without recomputing the board.

- **Submit record**: `id`, `team`, `problem`, `arrivedAt` (the database instant, not a "frozen" flag), `arrivedSeconds`
  from contest start, and a state: `Pending` or `Done(compiled, passed, taken)`. The raw outcome is stored,
  not a derived "success", so the existing `ACMScorer` rules (not compiled, failed on the first test, success)
  are reused unchanged and a rejudge is just a replacement of the state.
- **Per (team, problem)**: the records sorted by `(arrivedAt, id)`. Observed worst case is ~30 per cell, usually
  under 10; a few thousand records per contest, well under a megabyte. Cells are recomputed from this list
  (O(k), k ≤ ~30); no incremental cell arithmetic.
- **Per board** (freezing, real): for each team its derived row (cells, score) and rank; the teams in
  ranking order with an index by team id; per-problem solved counts for the table header.
- **Contest times** (`freeze_time`, `expose_time`, `end_time`) are read from the contest the monitor already
  tracks. Because the record holds `arrivedAt`, an admin editing those times is handled by rebuilding both boards
  **from memory** (no database read).

Applying one event (arrive / finish / rejudge request):

1. Find or create the submit record (an id not seen before is loaded from the database once), update its state.
2. Recompute that (team, problem) cell for **both** boards, then that team's score on each (≈ one cell per
   problem).
3. Per board: if neither the cell nor the score nor the rank-determining data changed, emit nothing for that
   board (e.g. a post-freeze finish changes the real board only: nothing goes to the freezing stream).
4. Otherwise reposition the team in the board's order, compute the affected position range and its new ranks,
   adjust the header counts by the change in the cell's solved flag, and emit one `row` event for that board.
5. Call the palevo observation and mark static files dirty.

A rejudge request sets the record to `Pending` in memory and its finish sets `Done`: no database access
is needed on either step.

Database reads happen only: at startup (all submits of every loaded contest, including unfinished), in the
cross-check, and for a submit id never seen before. **(call)** All contests are loaded at startup as the old
monitor does; the builder may load closed contests lazily if startup time matters.

## Events fed to the live monitor

All sent as fire-and-forget messages (`!`), never `ask`, from existing code paths, guarded by the config flag
and wrapped so an exception cannot escape into the caller:

1. **Submit arrived** — `Application.submitPost` after the insert (`Application.scala` ~l.126). The insert
   currently returns only the id; it must also return the database-assigned `submit_time_absolute`, so the
   freeze decision uses the DB clock (same column and comparison as `afterFreeze`, `contestxml`, and
   `contest.freezeTime`).
2. **Testing finished** — `StatusActor` on `FinishedTesting` (`StatusActor.scala` ~l.299). `FinishedTesting`
   already carries `compiled`, `passed`, `taken`; the monitor needs those, not the thinner `AnnoSubmit`.
3. **Rejudge requested** — `AdminApplication` sends the same `SubmitMessage` to `contester.submitrequests`
   for rejudges (`AdminApplication.scala` ~l.182 and ~l.193). The submit goes back to pending until its new
   testing finishes. **(call)** The builder must check what the database says about such a submit while the
   rejudge runs (whether `finished` is reset) and make the cross-check not flag in-flight rejudges as
   mismatches (e.g. skip submit ids with a rejudge in flight).
4. **Unknown submit id** in a finished/rejudge event (race, or inserted by another path): load that one row
   from the database.
5. **Time** — a timer fires the start (public/team boards become visible), the freeze→expose flip (and any other time-based change such as a submit
   crossing the freeze is already decided at insertion).

Team/problem changes (a team added, renamed, flips `not_rated`, problem list edited) are not events; they are
picked up by the cross-check or by the next restart. When the cross-check finds the structure changed, it
replaces the state and sends a whole-board event.

## Startup, cross-check, failure isolation

- On startup, for every contest, compute both boards in full from the database (problems, teams, **all**
  submits including unfinished) **before** subscribing to or handling any event. Events that arrive during
  the load are queued and applied afterwards (the existing `MonitorActor` `Stash` pattern).
- Cross-check (every `monitor.crosscheck_interval`): load fresh data, build both boards with the same pure
  function, compare with the live state. On any difference: log the contest, board and differing teams at
  `warn`, replace the live state with the rebuilt one, and send a whole-board event to connected clients.
- Any exception while applying one event: log, mark that contest dirty, rebuild it from the database on the
  next cross-check tick (or immediately, rate-limited). Never rethrow to the sender.
- When the live monitor is enabled, the old `MonitorActor` is **not started**: the live monitor writes the
  static files and feeds palevo (see Palevo), so nothing re-reads all submits every 20 s any more.
  When disabled, the old `MonitorActor` behaves exactly as today (board, static files, palevo).

## Palevo (user decision: the live monitor feeds it)

- Exactly one component feeds `StandingsAnnouncer`: the live monitor when enabled, the old `MonitorActor`
  when `monitor.disable_new_monitor = true`. Never both (every announcement would go out twice).
- The live monitor calls `StandingsAnnouncer.observe(contest, freezingBoard, realBoard, now)` after each
  applied event (and after a startup load, a cross-check correction and the expose flip). Announcements are
  therefore near-immediate instead of up to 20 s late.
- The public tracker is fed the **freezing** board and the jury tracker the **real** board, exactly as now.
  `?` cells are not solves, so they produce no announcements. A pre-freeze submit judged after the freeze
  still shows its real result on the freezing board and is announced publicly, as today.
- `StandingsTracker` currently matches on `ACM.Status`. Give it a small input type (per row: team, rank,
  solved count, and solved cells with `arrivedSeconds`) that both `ACM.Status` and the live board produce.
  Existing tracker specs must keep passing unchanged in meaning. **(call: shape of the input type)**
- The first observation of a contest after startup only records state (existing behaviour), so a restart
  announces nothing. The startup load must therefore complete, and be observed, before events are handled.
- A cross-check correction replaces the state and observes it. If that would announce a new leader or first
  solve, it is announced like any other change (trackers only announce each first solve once). **(call)** This
  is a known risk: a bug in the live path can produce a public announcement. The mitigation is the switch,
  which moves palevo back to the old path after a restart.
- `ContestPhaseTracker` (started/frozen/finished) stays in `StatusActor` and is not touched.

## Transport (SSE) and the streams

Three audiences, three kinds of stream. The server chooses which one a request gets, from its session (or lack
of one); the client cannot ask for a board.

- **Public** (no login): the freezing board, exactly what the static file shows. No own-row overlay and no
  highlighted row. See "Public endpoint".
- **Team** (logged in, `SecuredAction`, under `/monitor`): the freezing board for the team's own contest, plus
  the own-row overlay. See "Own row under the freeze".
- **Admin** (under `/admin/monitor/:contestId`): the real board if `account.canSeeAll(contestId)`, otherwise
  the same as a team without an own row (the freezing board; same rule as `AdminApplication.monitor`).

Each board has its own broadcast. The real-board broadcast is only reachable through the admin route behind
`canSeeAll`, and through the per-team overlay channel described below. Reuse the existing patterns
(`Concur.broadcast`, `EventSource.flow`, a periodic ping) used by `StatusActor.JoinUser/JoinAdmin`.
Every connection (first load, reconnect, page refresh) starts with a **whole board** event. No replay.

## Own row under the freeze (user decision)

A logged-in team already knows the outcome of its own submits. During the freeze (`freeze_time` ≤ now <
`expose_time`) its stream shows:

- its own row's **cells** as on the real board: real `+X`/`-X` as soon as judged, and `?`/`?X` only while a
  submit is actually being judged (not "until expose");
- everything else exactly as on the freezing board: the team's **position, rank, solved and penalty columns**,
  all other rows, and the header counts. So the team learns which problems it solved but not where it ranks.
  **(call)** The score columns stay frozen so the team cannot infer its rank by comparing real totals with the
  frozen rows around it. The consequence is that its own cells and its own totals may visibly disagree during
  the freeze; this is accepted.
- At `expose_time` the whole-board event replaces everything.

Mechanics:

- The shared freezing broadcast is not changed. A per-connection step for team T swaps T's row cells for T's
  real-board cells in the `board` event and in `row` events about T (keeping the freezing position, rank and
  score).
- When T's real-board cells change but T's freezing row does not (a post-freeze submit), the monitor emits a
  small `cells` event ({team, cells}) on a per-(contest, team) channel. The team's connection merges this
  channel with the freezing stream. Only T's connections receive it.
- The overlay uses only T's own real-board cells; no other team's real data and no real-board totals, ranks,
  positions or counts are ever put on a team stream.
- Admin accounts without `canSeeAll` have no own row and get the plain freezing board.

## Public endpoint (user decision)

- An unauthenticated page and an unauthenticated SSE stream of the freezing board, in addition to the static
  files. Same board a team sees under the freeze without an own row; the freeze rules apply to every row.
  Paths **(call)**: `GET /public/monitor/:contestId` (page) and `GET /public/monitor/:contestId/events`
  (stream). No session or cookie is read or created.
- Which contests are public is configuration, not "any id": `monitor.public_contests` (list of contest ids),
  **default 1–10** (user decision: the non-jury range). Any other id returns 404. This is the same range the
  existing static files cover (`contest.id < 11`).
- Before `start_time` the public board shows no problems or rows, like the logged-in monitor does today
  (`contest.started`); at `start_time` the monitor sends the whole board (a timer event, like the expose
  flip).
- Abuse guard **(call)**: `monitor.public_max_connections` (default 1000) caps concurrent public streams; over
  the cap returns 503. The stream sets `Cache-Control: no-cache` and `X-Accel-Buffering: no` so a reverse
  proxy does not buffer it.
- The public page does not auto-reload (the static files keep their 60 s reload). It uses the same markup and
  `main.js` renderer as the other live pages.

### Event types (JSON in the SSE `data` field)

- `board` — the whole board: problem list, per-problem solved counts, and every row in order
  (team id, team display fields, rank, score, cells). Sent on connect, at `expose_time`, and after a
  cross-check correction.
- `row` — one team's row changed and/or moved. Carries: team id, new position, the row's score, rank and
  cells, and the **rank range**: the first and last position affected and the new rank value for each
  position in that range. Positions outside the range keep their rank (a team moving from position p to q
  only changes the rank of positions between q and p, inclusive; rows after the larger of the two have the
  same number of rows before them, so their rank is unchanged). **(call: the exact field names)**
- The per-problem solved counts shown in the table header (`(N)` under each problem letter) change when a
  team's cell becomes or stops being solved. They are not rank-related, so `row` also carries the new
  counts for the problems whose count changed. **(call: user asked whether anything else changes; this is
  the one other thing.)**
- `cells` — only the cells of one team's row, no move and no rank change. Sent on the per-team overlay
  channel only (see "Own row under the freeze").
- ping — keep-alive.

Client (`main.js`): on `board` rebuild the table body; on `row` move/replace the row element, apply cells,
then rewrite the rank cells for the positions in the range and update the header counts. Highlight of the
current team's row (the `info` class) must survive updates.

## Static files (user decisions)

- Written from the freezing board, atomically (temp file + move) as today, via the existing
  `staticmonitor` view adapted for `?` cells.
- Rate-limited to at most one write per `monitor.static_interval` per contest (default 30 s): a dirty flag
  plus a timer, with a trailing write so the final state is always written. Unchanged boards are not
  rewritten. **(call: skip-if-unchanged)**
- Static files must remain correct with no JavaScript and without qf running.
- Initial HTML for the live page and the static files render from the same Twirl markup; the JavaScript
  renderer for `board`/`row` produces the same markup. Keep these two in sync with a test or shared fixture.
  **(call)**

## Testing expectations

- Pure board logic is unit-tested without a database: cell states (including out-of-order finish, pending
  after solve), freeze/expose rules for both boards, ranking and ties, not-rated teams, `freeze = end`.
- Property-style test: apply a random sequence of arrive/finish/rejudge events incrementally, and assert the
  result equals a full rebuild after every step. This is the main guard against drift.
- Rank-range correctness: after each `row` event, applying the event to the previous board gives the new
  board exactly.
- Leak test: the public stream and the static file never contain a value that differs between the two boards.
  A team's stream differs from the public stream only in that team's own row cells (and `cells` events).
- Own-row overlay: during the freeze a team's own cells show the real result, while its position, rank, score
  columns and header counts equal the freezing board; after `expose_time` it equals the real board.
- Public endpoint: no session needed, unlisted contest id gives 404, nothing before `start_time`, the
  connection cap returns 503.
- Config: with `disable_new_monitor = true`, no live actor exists, hooks do nothing, and the old
  `MonitorActor` feeds palevo; with it `false`, the old actor is not started and the live monitor feeds palevo.
- Palevo: feeding the tracker from the live boards produces the same events as feeding it the equivalent
  batch `ACM.Status` for the same submits (including the frozen/jury split and the live window).
- DB-backed specs exist but self-cancel without a test database; new tests must not need one.

## Left to the builder

Actor and class layout, JSON field names, exact SSE route paths, how the Twirl and JS renderers share
markup, the exact timers, and the mechanism for the cross-check's rejudge exclusion.
