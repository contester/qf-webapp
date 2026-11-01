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
