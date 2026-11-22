# qf

qf is the contest web front end: contestants log in with their round credentials, submit solutions, run custom
tests, read clarifications and ask questions, and see their per-test results; admins watch the live points board,
the submit status, rejudge, answer questions and manage a few round settings. Testing is done by director through
its teacher-web bridge (`cmd/bridge-teacherweb` in the director repository); qf never talks to director itself.

## Database

qf uses teacher-web's PostgreSQL database (schema `public`) and never migrates it: the tables, the
`credential_unrated` enum and the notification triggers are teacher-web's goose migrations (00005 and later).
Seasons, contests, rounds, participants, credentials and admins are teacher-web's; qf reads them and writes only
contest data (submits, custom tests, clarifications and requests, messages, extra info), `participants.unrated`
and, from the admin languages page, `season_languages`. A qf contest id in URLs is a `contest_rounds.id`. Only rounds
of the current season (`seasons.is_current`) with both `starts_at` and `ends_at` set exist for qf.

Scoring is always points (there are no ACM or school modes). Admins are teacher-web's `admins` (email + bcrypt);
every admin may do everything in every round.

## Configuration

Besides the usual Play keys (`play.http.secret.key`, `play.application.loader = "MyApplicationLoader"`):

    slick.dbs.default.profile = "org.stingray.contester.dbmodel.MyPostgresProfile$"
    slick.dbs.default.db.driver = "org.postgresql.Driver"
    slick.dbs.default.db.url = "jdbc:postgresql://host:5432/contester"
    slick.dbs.default.db.user = "..."
    slick.dbs.default.db.password = "..."
    fileserver.url = "http://bridge-host:port/fs/"   # the bridge's /fs2/ and /protopackage/ proxy (test outputs)
    fileserver.shortn = "..."                        # informational `contest` parameter of those requests

Optional: `notify.keepalive.*` (database notifications, below), `palevo.*` (IRC announcements, below), and
`play.i18n.langs` (the UI defaults to Russian, `["ru"]` from `conf/messages.ru`, regardless of the browser's
`Accept-Language`; set `play.i18n.langs = ["en"]` for English from `conf/messages`).

`play.http.secret.key` MUST be set to a long random secret in every deployment (e.g. `openssl rand -base64 48`),
never a default or a value from a repository: it signs the Play session and the CSRF token, which carry the admin
and contestant logins. The Silhouette crypter key `"bar"` in `MyApplicationLoader` only encrypts the authenticator
stored inside that signed session; it is not a secret and does not replace the Play secret.

CSRF: forms carry Play's token. The JavaScript POSTs of the admin buttons (rejudge, hide/show and delete of a
clarification) and of the contestant's message acknowledgement send `X-Requested-With: XMLHttpRequest` instead, which
the application's CSRF configuration accepts (a cross-site page cannot send that header without a CORS preflight,
and no CORS is enabled).

## Admin pages

`/admin` (log in with a teacher-web admin): the submit status (live), custom tests (with rerun), the live points
board, clarifications and questions, rejudge (per submit, or a problem's outdated/all submits), the rounds of the
current season (read only; rounds are edited in teacher-web), extra info pages per round, the round's participants
with the unrated toggle (rated / `not_rated` / `disqualified`, written to `participants.unrated`; the live board
follows through `participant_changed`), and the season languages page (the current season's `season_languages`, and
copying another season's list into it). With no current round the round pages say «Нет текущих туров».

## Tests

    sbt test

runs the unit specs; the database specs are skipped unless a teacher-web database is given:

    QF_TEST_URL=jdbc:postgresql://127.0.0.1:55433/contester QF_TEST_USER=postgres QF_TEST_PASSWORD=x sbt test

The database must be migrated by `teacherweb migrate` and seeded with teacher-web's `doc/testdata/seed.sql` (one
current season and round with Polygon contest 62042, participants `team1_1..3`, admin `admin@example.org`). The specs
create their own rounds, participants and seasons and delete them afterwards; they run one suite at a time. The seed
round's times are relative to the time of seeding, so re-seed before a run if it is hours old.

## Configuration: palevo (IRC announcements)

qf-webapp can post contest events (new clarifications, questions from teams and the answers to them, round start and
end with countdowns and reschedules, first solves and new leaders for the jury, the final standings) to
[palevo](https://github.com/contester/palevo), which announces them in the duty IRC channel. Add to the application configuration:

    palevo.url = "http://127.0.0.1:8081/events"
    palevo.token = "..."   # the same value as PALEVO_TOKEN in palevo's /etc/sysconfig/palevo

If either key is missing the feature is off. Delivery is best effort (3 attempts, in order, one event at a
time) and never affects contest operation. There is no freeze: standings events come from the one live points board.
While a round runs they go to the jury audience only (`first_solve_contest`, `first_solve_problem`, `new_leader`
with `"audience": "jury"`); the public hears nothing about the board until the round is over (`is_finished`, or its
end has come). Then one public `final_standings` event names the top three places (team, points, problems at full
points), and it is sent again (`"updated": true`) only when a later change, such as a rejudge, alters them. A
restart after the end announces nothing. palevo must know the `final_standings` type; until it does, it rejects
the event and qf drops it.

### Contest time events

palevo is told when a contest starts and ends (`contest_starting`, `contest_ending`; there is no freeze event),
that these are coming (the same events with `in_minutes`: "starts in 10 minutes"), and when an operator moves a
contest (`contest_rescheduled`: "start time changed to 19:30"). Optional keys:

    palevo.countdown.start = [60, 30, 20, 10, 5, 2, 1]   # minutes before the start (default as shown)
    palevo.countdown.end = [20, 10, 5, 1]                # minutes before the end (default as shown)

An empty list turns that countdown off; a value that is not positive is ignored with a warning (the default is used).
A late or restarted application announces only the latest threshold it has crossed, and nothing for the ones already
passed when it started. Countdowns and reschedules are announced only for contests 1 to 10 (the others are test
copies); the phase events themselves are announced for every contest. A reschedule lists only `start` and `end`.

The events are the edges of the round clock (`actors.RoundClock`, below): each countdown threshold is an edge of
its own, so no timer other than the clock's one deadline timer is involved. Countdown edges go to palevo only, never
to browsers.

## Database notifications

Testing results, custom test results and changes to rounds, credentials and participants reach qf as PostgreSQL
notifications (`LISTEN testing_finished, eval_finished, round_changed, credential_changed, participant_changed`;
the triggers are teacher-web's migrations). `NotifyListener` holds one connection of its own, opened from
`slick.dbs.default.db.url` / `user` / `password` (not from the pool) with `ApplicationName=qf-notify-listener`,
and reconnects with a backoff of 1 s doubling to 30 s. After every connect (the first one included) it catches up
once, instead of replaying the notifications it missed: it drops the whole public board cache, makes the admin live
monitor reload every round, re-reads the current rounds (as if each had a `round_changed`) and drops rounds that are
gone. No timer is involved.

A silently dropped connection (NAT or firewall timeout, dead server) is detected by TCP keep-alive, set on our
socket and, through the `options` startup parameter, on the server too (optional, seconds):

    notify.keepalive.idle = 60
    notify.keepalive.interval = 10
    notify.keepalive.count = 6

Submits and custom tests are requests by themselves: the insert triggers notify the bridge. An admin rejudge sets
`submits.testing_id` to NULL (the bridge hears `submit_new` again); a custom test rerun inserts a copy of the test.

## Scoreboard and live monitor

The scoreboard is a points board: per problem the best points of a team's finished, compiled submits (the points of
each submit's current testing: a rejudge replaces them, never adds), the total, and places by total (equal totals
share a place, the next one skips: 1, 1, 3). Not rated participants are listed after the ranked ones without a place;
disqualified ones are not listed. Admins always see the real live board; public, joined multi-round, and grade-segregated scoreboards live in `teacher-web`.

- `/admin/monitor/:id` (admins): the live board. The page carries the board the stream at `/admin/monitor/:id/events`
  starts with; the stream then sends a `row` event for each changed row (a finished testing, an arrival or a rejudge:
  `?N` marks N submits in flight, never counted as the best) and a whole `board` event when a row appears or goes
  (a credential deactivated, a participant disqualified). Credential and participant edits (`credential_changed`,
  `participant_changed`) re-read the round's participants and update the rows without a reload; a change of the
  round's problems (`round_changed`) reloads the round. The page is sent `Cache-Control: no-store`.

There are no static monitor files and no `monitor.*` settings any more (`monitor.disable_new_monitor`,
`monitor.public_contests`, `monitor.public_max_connections`, `monitor.static_location` and the old interval keys are
ignored). The live actor feeds palevo's standings events (see the palevo section); it has no timer of its own and
hears the end of a round from the round clock.

## No polling

qf never re-reads the database on an interval. Contests, teams and problems are loaded once at start-up and then
follow the notifications (`round_changed`, `credential_changed`, `participant_changed`). Round starts and ends, and
palevo's countdown thresholds, come from one exact single-shot timer (`actors.RoundClock`), armed for the next deadline and re-armed on every round change
and after it fires; each edge is pushed to the round's participants and admins as a `round_started` / `round_ended`
server-sent event, and a moved round as `round_rescheduled`. On start-up the clock announces nothing that already
happened (a running round gets no `round_started`). The only other timers are SSE keep-alive comments and
retries after a failure. `round_changed` also fires for a deleted round, for any change of a round's
`problems` rows and, for every round of the season, when a season becomes (or stops being) current
(teacher-web migration 00009), so problems and season switches need no restart either.
