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

Three audiences, each page with an `/events` stream next to it:

- `/monitor` (and `/monitor/events`): a team, which sees its own cells.
- `/admin/monitor/:id`: admins, with a switch between the frozen and the real board.
- `/public/monitor/:id`: anyone, always the frozen board.

Between freeze_time and expose_time a submit that arrived after the freeze is shown as `?` on the public and
admin-frozen boards. A team sees the real result of its own cells, but its rank, the solved and penalty columns
and every other team stay as on the public board. At expose_time all streams switch to the real board.

Rollback: set `monitor.disable_new_monitor = true` and restart qf. No live actor is created, the hooks do
nothing, and the old monitor (palevo and static files) starts again. Remove the key and restart to go back.

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
