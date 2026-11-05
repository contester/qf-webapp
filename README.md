# QF Internal Site

## Configuration: palevo (IRC announcements)

qf-webapp can post contest events (new clarifications and tasks, contest start/freeze/finish, first solves,
new leader, print errors) to [palevo](https://github.com/contester/palevo), which announces them in the
duty IRC channel. Add to the application configuration:

    palevo.url = "http://127.0.0.1:8081/events"
    palevo.token = "..."   # the same value as PALEVO_TOKEN in palevo's /etc/sysconfig/palevo

If either key is missing the feature is off. Delivery is best effort (3 attempts, in order, one event at a
time) and never affects contest operation. Scoreboard events are computed from the pre-freeze monitor only,
so nothing is announced about the scoreboard after the freeze.

## Live monitor

The scoreboard is pushed to browsers over server-sent events and replaces the polling monitor. Configuration
(all optional):

    monitor.disable_new_monitor = false  # true switches the live monitor off (restart qf); the old monitor runs instead
    monitor.static_interval = 30s        # how often the static public pages are rewritten
    monitor.crosscheck_interval = 60s    # reload from the database and compare; 0 disables
    monitor.public_contests = [1,...,10] # contests served on /public/monitor
    monitor.public_max_connections = 1000
    monitor.static_location = "/path"    # directory for the static pages; unset = no static files

Static files are written only for contests with id < 11 (hardcoded, independent of `monitor.public_contests`).
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
rebuilds the boards and sends every stream a whole `board` event; no waiting for the cross-check. Editing a school
is not covered: it shows up at the next cross-check.

Editing a contest (the edit form, or moving it earlier or later) tells the status actor and the live monitor the new
row at once. The live monitor still re-reads the contests every 5 minutes (`LiveTimings.contestPoll`), only to find
contests that were added or changed in the database behind the application's back; such a change can take that long to
show. A poll that read the database before an edit was sent is ignored for that contest.

Between freeze_time and expose_time a submit that arrived after the freeze is shown as `?` on the freezing board
(a contest with freeze_time >= end_time has no freeze). A `?` also appears on every board for a submit that is not
judged yet. A team sees the real result of its own cells, but not its rank, the solved and penalty columns or any
other team. At expose_time the public and team boards become the real board; `canSeeAll` admins always see it.

Rollback: set `monitor.disable_new_monitor = true` and restart qf. No live actor is created, the hooks do
nothing, and the old monitor (palevo and static files) starts again. Remove the key and restart to go back.
With the live monitor on, a malformed `static_interval`, `crosscheck_interval`, `public_contests` or
`public_max_connections` stops qf at boot (fail-fast).
`monitor.disable_new_monitor = true` ignores all the other live keys, so it always works as the rollback switch.

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
