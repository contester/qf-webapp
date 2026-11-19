# QF Internal Site

## Configuration: palevo (IRC announcements)

qf-webapp can post contest events (new clarifications, questions from teams and the answers to them, tasks, contest start/freeze/end with countdowns and reschedules, first solves,
new leader, print errors) to [palevo](https://github.com/contester/palevo), which announces them in the
duty IRC channel. Add to the application configuration:

    palevo.url = "http://127.0.0.1:8081/events"
    palevo.token = "..."   # the same value as PALEVO_TOKEN in palevo's /etc/sysconfig/palevo

If either key is missing the feature is off. Delivery is best effort (3 attempts, in order, one event at a
time) and never affects contest operation. Scoreboard events are computed from the pre-freeze monitor only,
so nothing is announced about the scoreboard after the freeze.

### Contest time events

palevo is told when a contest starts, freezes and ends (`contest_starting`, `contest_freezing`, `contest_ending`),
that these are coming (the same events with `in_minutes`: "starts in 10 minutes"), and when an operator moves a
contest (`contest_rescheduled`: "start time changed to 19:30"). Before 2026-11-07 the phase events were called
`contest_started`, `contest_frozen` and `contest_finished`; deploy palevo first, it still accepts the old names.
Optional keys:

    palevo.countdown.start = [60, 30, 20, 10, 5, 2, 1]   # minutes before the start (default as shown)
    palevo.countdown.end = [20, 10, 5, 1]                # minutes before the end (default as shown)

An empty list turns that countdown off; a value that is not positive is ignored with a warning (the default is used).
A late or restarted application announces only the latest threshold it has crossed, and nothing for the ones already
passed when it started. Countdowns and reschedules are announced only for contests 1 to 10 (the others are test
copies); the phase events themselves are announced for every contest. Expose-time changes alone are not announced.

The events come from the live monitor.

## Database notifications

Testing results, custom test results and changes to rounds, credentials and participants reach qf as PostgreSQL
notifications (`LISTEN testing_finished, eval_finished, round_changed, credential_changed, participant_changed`;
the triggers are teacher-web's migrations). `NotifyListener` holds one connection of its own, opened from
`slick.dbs.default.db.url` / `user` / `password` (not from the pool) with `ApplicationName=qf-notify-listener`,
and reconnects with a backoff of 1 s doubling to 30 s. A notification missed while it is disconnected is not
replayed: the pages read the database, so it only delays a live update until a reload.

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
disqualified ones are not listed. There is no freeze: admins always see the real board, everybody else sees it once the
round is over (`is_finished`, or `ends_at` has come).

- `/admin/monitor/:id` (admins): the live board. The page carries the board the stream at `/admin/monitor/:id/events`
  starts with; the stream then sends a `row` event for each changed row (a finished testing, an arrival or a rejudge:
  `?N` marks N submits in flight, never counted as the best) and a whole `board` event when a row appears or goes
  (a credential deactivated, a participant disqualified). Credential and participant edits (`credential_changed`,
  `participant_changed`) re-read the round's participants and update the rows without a reload; a change of the
  round's problems (`round_changed`) reloads the round. The page is sent `Cache-Control: no-store`.
- `/monitor` (a participant) and `/public/monitor/:id` (anyone, no session): 403 «Результаты будут опубликованы после
  окончания тура» while the round runs, then the final board rendered from the database. The navbar shows the
  scoreboard tab to participants only once their round is over. There is no live stream for them.
- The public page is rendered on demand and kept per round and language until a notification can have changed it:
  a finished testing, a rejudge, a credential or participant change, a round change or removal (`PublicBoardCache`).
  It never expires by time, and only rounds that are over are ever cached.

There are no static monitor files and no `monitor.*` settings any more (`monitor.disable_new_monitor`,
`monitor.public_contests`, `monitor.public_max_connections`, `monitor.static_location` and the old interval keys are
ignored). The live actor feeds palevo's time and standings events; standings events reach the public audience only
once the round is over.

## No polling

qf never re-reads the database on an interval. Contests, teams and problems are loaded once at start-up and then
follow the notifications (`round_changed`, `credential_changed`, `participant_changed`). Round starts and ends come
from one exact single-shot timer (`actors.RoundClock`), armed for the next deadline and re-armed on every round change
and after it fires; each edge is pushed to the round's participants and admins as a `round_started` / `round_ended`
server-sent event, and a moved round as `round_rescheduled`. On start-up the clock announces nothing that already
happened (a running round gets no `round_started`). The only other timers are SSE keep-alive comments and
retries after a failure. `round_changed` also fires for a deleted round, for any change of a round's
`problems` rows and, for every round of the season, when a season becomes (or stops being) current
(teacher-web migration 00009), so problems and season switches need no restart either.

## TODO

- Reactive
    - teams
    - contests (half-done in status)
    - submits
- Team editing
- diff solutions
- В запуске на сервере подсвечивать тип результата (Хороший, Плохой, в очереди)
- Панель статуса в админке для наблюдателей:
    - Таймер \[+ кнопки для сдвига админам\]
