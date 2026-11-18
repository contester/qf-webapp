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

The events come from the live monitor, or from the status actor when `monitor.disable_new_monitor = true`: exactly
one of them produces them (`LiveWiring.timeEventHost`).

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

## Live monitor

The scoreboard is pushed to browsers over server-sent events and replaces the polling monitor. Configuration
(all optional):

    monitor.disable_new_monitor = false  # true switches the live monitor off (restart qf); the old monitor runs instead
    monitor.public_contests = [1,...,10] # contests served on /public/monitor
    monitor.public_max_connections = 1000
    monitor.static_location = "/path"    # directory for the static pages; unset = no static files

Static files are written only for contests with id < 11 (hardcoded, independent of `monitor.public_contests`), when
the board changes. The old keys `monitor.static_interval` and `monitor.crosscheck_interval` are ignored: nothing in qf
runs on an interval (see "No polling" below).
With the live monitor on, the live actor feeds palevo's standings events; with it off, the old monitor does.

Three audiences, each page with an `/events` stream next to it:

- `/monitor` (and `/monitor/events`): a team, which sees its own cells.
- `/admin/monitor/:id`: admins. There is no switch: the server picks the stream. An admin with `canSeeAll` for the
  contest gets the real board; other admins with spectate permission get the freezing board.
- `/public/monitor/:id`: anyone. The freezing board (identical to the static file); it is empty before the contest
  starts and becomes the real board at expose_time.

Each page already contains the table, rendered on the server from the same board its stream starts with (so it
shows up before the stream connects); the stream then replaces it. If the live monitor does not answer within a
second the page is served with an empty box and the stream fills it. These pages are sent `Cache-Control: no-store`.

Editing a team (not rated, name, school, number) updates the team list in memory and tells the live monitor, which
rebuilds the boards and sends every stream a whole `board` event.

A change of a round in teacher-web (`round_changed`) reaches the status actor, its round clock and the live monitor at
once; a round whose times are cleared, or that is no longer in the current season, is dropped from all of them.

Between freeze_time and expose_time a submit that arrived after the freeze is shown as `?` on the freezing board
(a contest with freeze_time >= end_time has no freeze). A `?` also appears on every board for a submit that is not
judged yet. It is yellow and numbered by attempt: `?1` for a team's first try, `?2` after one failure; it turns into `+`/`+N` or `-N` when judged. A team sees the real result of its own cells, but not its rank, the solved and penalty columns or any
other team. At expose_time the public and team boards become the real board; `canSeeAll` admins always see it.

Rollback: set `monitor.disable_new_monitor = true` and restart qf. No live actor is created, the hooks do
nothing, and the old monitor (palevo and static files) starts again. Remove the key and restart to go back.
With the live monitor on, a malformed `public_contests` or `public_max_connections` stops qf at boot (fail-fast).
`monitor.disable_new_monitor = true` ignores all the other live keys, so it always works as the rollback switch.

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
