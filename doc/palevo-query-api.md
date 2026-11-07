# palevo query API

A read-only JSON API of qf-webapp for palevo's chat bot, under `/api/palevo/v1`. The bot's tool layer passes
response bodies to a language model verbatim, so field names and meanings below are a stable contract. All
responses are `application/json`, all requests are `GET`.

## Authentication and configuration

    palevo.query.token = "..."                        # value of PALEVO_QF_TOKEN in palevo
    palevo.query.contests = [1,2,3,4,5,6,7,8,9,10]    # optional; default 1..10

- Every request carries `Authorization: Bearer <token>`. The scheme word `Bearer` is case-sensitive and followed
  by exactly one space. The token is compared in constant time (SHA-256 digests of the presented and the
  configured token are compared, so neither its content nor its length leaks). No cookie or session is read or created.
- `palevo.query.token` unset or blank: the API is disabled and **every route answers 404 with an empty body**
  (also when a header is sent).
- Missing or wrong token: `401` with `{"error":"unauthorized"}` (the body never contains the token).
- `palevo.query.contests` bounds every request: a contest outside this set is never shown, never queried for data and never
  mentioned, whatever `contests` or `contest` say.

## Common parameters

Contest-bound endpoints (`/contest`, `/standings`, `/team`, `/problems`, `/printing`) require:

| parameter | meaning |
|---|---|
| `audience` | required. `public`: what the public may see (frozen standings during a freeze). `all`: the jury's view (live standings, jury-only fields). Anything else, or missing: 400. |
| `contests` | required. `*` or a comma list of ids (`1,2`). Restricts the candidate contests (intersected with `palevo.query.contests`). Anything else: 400. |
| `contest` | optional contest id. Must be among the candidates, otherwise 400 `contest N is not available in this scope`; a non-number is 400 `contest must be an id`. |

### Contest selection

Among the candidates (allowed and listed in `contests`):

1. `contest` given: exactly that contest.
2. Otherwise all contests that are **running** now, by id (finished and upcoming contests are then ignored).
3. Otherwise (nothing running) **both** the contest that **finished** most recently (latest end time, higher id on
   a tie) **and** the contest that **starts** next (earliest start time, lower id on a tie), whichever of the two
   exist, sorted by id. So between contests 3 (over) and 4 (tomorrow) the array holds 3 and then 4; with only
   finished contests just the latest one, with only upcoming ones just the soonest one.
4. No candidate at all: `{"as_of": ..., "contests": [], "note": "no contest"}`.

The array can therefore mix an `over` and a `before` contest; each element is answered according to its own
`status` (see below: a `before` contest has no problems, rows or statistics).

Every contest object starts with `id`, `name`, `status` (`before` | `running` | `over`) and `view`.

### Freeze and `view`

| freeze state of the contest at `as_of` | `audience=public` | `audience=all` |
|---|---|---|
| in effect (freeze time reached, results not yet exposed) | `frozen` | `live-jury` |
| none configured, not yet reached, or already exposed | `no-freeze` | `no-freeze` |

`public` data always comes from the freezing board and never reveals what the freeze hides; `all` reads the real
board. A `public` response never contains a jury-only field (see below).

### Envelope

    {"as_of": "2026-11-06T12:00:00Z", "contests": [ {contest object}, ... ]}

`as_of` is the server time, ISO-8601 UTC without milliseconds. Per-endpoint fields are added to each contest
object. `/computers` has no `contests` array.

Before a contest starts (`status` `before`) no problems, rows or statistics are returned and the board is never
read (`/contest` still gives its times and `teams`, `/standings` and `/problems` just the base object). When the audience's board is unavailable, `/standings`, `/team` and `/problems` answer the contest with
`"note": "board unavailable"` and empty `rows` / `candidates` / `problems`, and `/contest` with `audience=public`
answers it with that note and no `problems`. The board is unavailable when the live board is off or too slow,
and for `audience=public` also when the live monitor does not show the contest to the public (a contest that is
in `palevo.query.contests` but not in `monitor.public_contests`).

### Errors

`{"error": "<short English text>"}` with status 400 (bad parameter), 401, 500 (`internal error`; details are
only logged). Disabled API: 404, empty body.

## Endpoints

Examples below are real output of the test fixture (`as_of` 2026-11-06T12:00:00Z, contest 1 running with the
freeze in effect, a public board of 3 teams and a real board of 5 teams).

### GET /contest

Times, team counts and the problem list. Fields added: `start`, `end`, `freeze` (null when no freeze is
configured), `elapsed_seconds`, `remaining_seconds` (both clamped to the contest duration), `teams`, and once
started `problems` (`id`, `name`) in the order of the audience's board. When that board is unavailable,
`audience=all` lists the contest's problems by id instead, while `audience=public` gets no `problems` and
`"note": "board unavailable"`.

`teams` is an **object** `{"regular": X, "out_of_competition": Y, "total": X+Y}` counting the contest's
participants (the teams registered for it), for every selected contest including `before` ones:

- `out_of_competition`: participants carrying the **not-rated** flag, i.e. the teams shown with rank `*` on the
  standings (e.g. guest teams). A jury team counts as out of competition only if it
  carries that flag; there is no other notion of a jury team here.
- `regular`: all other participants. `total` is their sum.

It comes from the participant list, not from the board, so it is known before the start, does not depend on the
live monitor and is the same for both audiences (it reveals no problems or results). A participant that is
disabled still counts. If the count lookup fails, `teams` is left out and the rest of the contest object is
answered as usual. Do not confuse it with `/standings`' `teams`, which is a plain number: the rows on the board.

    GET /contest?audience=public&contests=*

    {"as_of":"2026-11-06T12:00:00Z","contests":[{"id":1,"name":"Contest 1","status":"running","view":"frozen",
    "start":"2026-11-06T10:00:00Z","end":"2026-11-06T13:00:00Z","freeze":"2026-11-06T11:00:00Z",
    "elapsed_seconds":7200,"remaining_seconds":3600,"teams":{"regular":4,"out_of_competition":1,"total":5},
    "problems":[{"id":"A","name":"Apples"},{"id":"B","name":"Bananas"}]}]}

Between contests (from `PalevoQueryContestSpec`: contest 3 ended at 10:00 with its results exposed, contest 4
starts tomorrow; nothing is running, so both are selected):

    GET /contest?audience=public&contests=*

    {"as_of":"2026-11-06T12:00:00Z","contests":[{"id":3,"name":"Contest 3","status":"over","view":"no-freeze",
    "start":"2026-11-06T06:00:00Z","end":"2026-11-06T10:00:00Z","freeze":"2026-11-06T09:00:00Z",
    "elapsed_seconds":14400,"remaining_seconds":0,"teams":{"regular":2,"out_of_competition":1,"total":3},
    "problems":[{"id":"A","name":"Apples"},{"id":"B","name":"Bananas"},{"id":"C","name":"Cherries"}]},
    {"id":4,"name":"Contest 4","status":"before","view":"no-freeze",
    "start":"2026-11-07T08:00:00Z","end":"2026-11-07T13:00:00Z","freeze":"2026-11-07T11:00:00Z",
    "elapsed_seconds":0,"remaining_seconds":18000,"teams":{"regular":40,"out_of_competition":3,"total":43}}]}

### GET /standings?top=N

The first rows of the audience's board. `top`: default 10, **values above 30 are cut to 30**; 0, negative or
non-integer is 400 `top must be a positive integer`. Fields added: `teams` (rows on the board) and `rows`.
Each row: `rank` is a **JSON string** (`"1"`, or `"*"` for a team that is not rated), `team` (display name),
`team_id`, `solved` and `penalty` are **numbers**.

    GET /standings?audience=public&contests=*&top=2

    {"as_of":"2026-11-06T12:00:00Z","contests":[{"id":1,"name":"Contest 1","status":"running","view":"frozen",
    "teams":3,"rows":[{"rank":"1","team":"School MSU #1: Beta","team_id":12,"solved":1,"penalty":5},
    {"rank":"2","team":"School SGU #1: Alpha","team_id":11,"solved":0,"penalty":0}]}]}

Between contests (same fixture as the second `/contest` example): rows for the finished contest, only the base
object for the upcoming one.

    GET /standings?audience=public&contests=*&top=2

    {"as_of":"2026-11-06T12:00:00Z","contests":[{"id":3,"name":"Contest 3","status":"over","view":"no-freeze",
    "teams":2,"rows":[{"rank":"1","team":"School 1 #1: Team1: Ivanov, Petrov","team_id":1,"solved":1,"penalty":10},
    {"rank":"2","team":"School 2 #1: Team2: Ivanov, Petrov","team_id":2,"solved":0,"penalty":0}]},
    {"id":4,"name":"Contest 4","status":"before","view":"no-freeze"}]}

### GET /team?q=TEXT or id=N

Finds teams on the audience's board. `q` is matched (ignoring case and punctuation, Latin look-alike letters folded to Cyrillic) against the school
short name joined with the team number (`sgu1`), the school short name, the team-name part before `:` and each
comma-separated member after `:`; the school full name is not matched. At most 5 candidates are returned; `id` is the team id and **wins over `q`** when both are given. Neither: 400 `q or id is required`; a
non-integer `id`: 400. Fields added per contest: `candidates` (`id`, `name` (display name), `school_short`
(school short name), `num` (team number, a JSON number); best matches first) and, only when exactly one
candidate remains, `team`. With several candidates there is no `team`; ask again with `id`.

Each field match scores: exact 3, prefix 2, substring 1; a team scores its best field. If exactly one team has
an **exact** match (score 3), it is resolved alone: `candidates` holds just that team and `team` is present, even
when other teams match by prefix or substring (`q=СГУ1` resolves `СГУ #1` although `СГУ #10` starts with
`сгу1`). Several exact matches (`q=СГУ` equals the school short name of every СГУ team) stay ambiguous, and so
do several matches none of which is exact (a bare number such as `q=1` is only a substring of `сгу1`, `мгу11`,
...). Team fields: `rank` (string), `solved`, `penalty`, `id`, `name`, `problems` (per problem id a cell:
`state` `ok` | `fail` | `pending` | `empty`, `attempts`, and `minutes` for `ok`; `attempts`: for `ok`, the number of
submits up to and including the solving one; for `fail`, the number of failed submits; for `pending`, the number of
failed submits before the first pending or freeze-hidden submit, those pending or hidden submits themselves not
being counted, so `"state":"pending","attempts":1` means 1 earlier failure and then at least one pending or hidden
submit), `last_submit_at` and
`seconds_since_last_submit` (absent when the team never submitted), and for `audience=all` only `room`.

`room` is the area of the computer of the team's latest submit **from a computer listed in the location
table**; if there is none it falls back to the area of the team's latest print job; absent when unknown. It is
never present for `audience=public`, and the room is not even looked up then.

Resolved:

    GET /team?audience=public&contests=*&q=Alpha

    {"as_of":"2026-11-06T12:00:00Z","contests":[{"id":1,"name":"Contest 1","status":"running","view":"frozen",
    "candidates":[{"id":11,"name":"School SGU #1: Alpha","school_short":"SGU","num":1}],
    "team":{"rank":"2","solved":0,"penalty":0,"id":11,"name":"School SGU #1: Alpha",
    "problems":{"A":{"state":"pending","attempts":1},"B":{"state":"empty","attempts":0}},
    "last_submit_at":"2026-11-06T11:57:00Z","seconds_since_last_submit":180}}]}

The same request with `audience=all` shows the live cells (`"A":{"state":"ok","attempts":1,"minutes":1}`) and
ends with `"room":"Room 7"`.

Ambiguous (`q=SGU`, two exact matches):

    {"as_of":"2026-11-06T12:00:00Z","contests":[{"id":1,"name":"Contest 1","status":"running","view":"frozen",
    "candidates":[{"id":11,"name":"School SGU #1: Alpha","school_short":"SGU","num":1},
    {"id":13,"name":"School SGU #2: Gamma","school_short":"SGU","num":2}]}]}

### GET /problems?window=15m

Per problem, from the audience's board: `solved` (teams that solved it), `teams_attempted`, `attempts` (the sum over all teams of the cell `attempts` defined under /team, so
pending and freeze-hidden submits themselves are not counted); plus activity: `recent_submits` (submits that **arrived** within the window,
whatever their outcome) and `pending` (submits not yet tested, whatever the freeze; a hidden submit stops
counting once it is tested, and this does not reveal its outcome. It can therefore differ from the number of
`pending` cells on the frozen board). `window` is `<n>m`, n from 1 to 120, default `15m`; anything else is 400.
Once the contest has started, the contest object also gets `window_minutes`.

Under `audience=public` with a freeze in effect, `solved`, `teams_attempted` and `attempts` come from the
**frozen** board, while `recent_submits` and `pending` count submits regardless of the freeze (`pending` only
those not yet tested).
If the activity lookup fails the counts degrade to 0 and the rest is still answered.

    GET /problems?audience=public&contests=*

    {"as_of":"2026-11-06T12:00:00Z","contests":[{"id":1,"name":"Contest 1","status":"running","view":"frozen",
    "window_minutes":15,"problems":[
    {"id":"A","name":"Apples","solved":1,"teams_attempted":2,"attempts":2,"recent_submits":1,"pending":1},
    {"id":"B","name":"Bananas","solved":0,"teams_attempted":1,"attempts":2,"recent_submits":0,"pending":0}]}]}

How the counts add up in this example (public, frozen board): problem A has Alpha `pending` with 1 earlier failure
(1) and Beta `ok` on the first try (1), so `attempts` is 2; problem B has Gamma `fail` with 2 failed submits, so
`attempts` is 2 and `teams_attempted` is 1.

### GET /computers?q=TEXT or area=NAME

Both audiences; `audience` is required and the answer is the same for `public` and `all` (a computer row has no
address, only `name`, `area` and `printer`). **Global**: not tied to a contest; the
`contests` parameter is optional (validated if given) and the response has no `contests` array. `q` needs at least one digit: its digits are compared with the computer name's trailing digits (leading zeros
ignored) and its letters are a prefix of the name's letters; a `q` without digits returns no computers, not a 400.
`area` is compared after normalisation (ignores case, spaces and punctuation, folds look-alike letters). When both
are given, `q` wins. Neither: 400. At most **10**
computers, sorted by name. `ambiguous` is true when several computers are returned and they belong to more than
one area.

    GET /computers?audience=public&q=46

    {"as_of":"2026-11-06T12:00:00Z","computers":[{"name":"lin46","area":"Linux room","printer":"HP-2"},
    {"name":"wi46","area":"Windows room","printer":"HP-1"}],"ambiguous":true}

### GET /printing?team=N&area=NAME

Both audiences, contest-bound with the usual selection. Per selected contest the
`print_jobs` (newest first, at most 10; `team` and `area` filter, `area` ignoring case; a non-integer `team` is
400), plus the global `printers` list of unresolved printer problems. Job fields: `id`, `team`, `computer`
(name, or address when not in the location table), `area` (empty when unknown), `printed`, `error`, `pages`,
`arrived`. State comes from the job's latest attempt (`print_job_state`): `printed` is true only for status
`printed`; **false can mean queued or failed**, and `error` is non-empty only when it failed. With no selectable
contest the answer carries `"note": "no contest"`. Printer entries: `agent` (the print agent that reported the
problem), `printer`, `condition`.

For `audience=public` the same jobs and printers are returned, minimised: printer entries carry only `printer` and
`condition` (**no `agent`**), and a job's `computer` is **never a raw address**: a job from a computer missing from
the location table (whose `computer` would be its IPv4 or IPv6 address) has `"computer": ""` (its `area` is
already empty). All other job fields are as for `all`.

    GET /printing?audience=all&contests=*

    {"as_of":"2026-11-06T12:00:00Z","contests":[{"id":1,"name":"Contest 1","status":"running","view":"live-jury",
    "print_jobs":[{"id":1,"team":11,"computer":"wi46","area":"Windows room","printed":true,"error":"",
    "pages":3,"arrived":"2026-11-06T11:55:00Z"},{"id":3,"team":12,"computer":"10.20.30.40","area":"",
    "printed":false,"error":"","pages":1,"arrived":"2026-11-06T11:54:00Z"},{"id":4,"team":13,"computer":"fe80::1",
    "area":"","printed":false,"error":"jam","pages":2,"arrived":"2026-11-06T11:53:00Z"}]}],
    "printers":[{"agent":"agent1","printer":"HP-1","condition":"paper out"}]}

    GET /printing?audience=public&contests=*

    {"as_of":"2026-11-06T12:00:00Z","contests":[{"id":1,"name":"Contest 1","status":"running","view":"frozen",
    "print_jobs":[{"id":1,"team":11,"computer":"wi46","area":"Windows room","printed":true,"error":"",
    "pages":3,"arrived":"2026-11-06T11:55:00Z"},{"id":3,"team":12,"computer":"","area":"","printed":false,
    "error":"","pages":1,"arrived":"2026-11-06T11:54:00Z"},{"id":4,"team":13,"computer":"","area":"",
    "printed":false,"error":"jam","pages":2,"arrived":"2026-11-06T11:53:00Z"}]}],
    "printers":[{"printer":"HP-1","condition":"paper out"}]}

## Jury-only data

The key `room` (in `/team`) appears only for `audience=all`, and for `public` the room is not even looked up.
`/computers` and `/printing` answer both audiences, but a `public` response never carries a print agent (`agent`)
or a computer's raw address (see `/printing`). The `/contest` `teams` counts are
public and identical for both audiences. `view` `live-jury` and the real board (including teams, problems
and results hidden by the freeze) are likewise `all` only. The test `PalevoQueryLeakSpec` checks this for every
endpoint, audience and freeze state.
