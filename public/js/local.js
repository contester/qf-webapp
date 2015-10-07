function maybeCheckNotifyPermission() {
    if (Notification.permission !== "granted") {
        Notification.requestPermission();
        return false;
    }
    return true;
}

function notifyMe(n_title, n_icon, n_body) {
    if (!Notification) {
        alert('Desktop notifications not available in your browser. Try Chromium.');
        return;
    }

    if (Notification.permission !== "granted")
        Notification.requestPermission();
    else {
        var notification = new Notification(n_title, {
          icon: n_icon,
          body: n_body,
        });

        notification.addEventListener('click', function () {
            window.focus()
            this.close()
        });

        window.setTimeout(function() {
            notification.close()
            }, 120000);

    }

}

    function pad(num, size) {
    var s = num+"";
    while (s.length < size) s = "0" + s;
    return s;
}

    function formatSeconds(s) {
        var seconds = s % 60;
        var minutes = (s / 60) >> 0;
        var hours = (minutes / 60) >> 0;
        minutes %= 60;

        return pad(hours, 2) + ':' + pad(minutes, 2) + ':' + pad(seconds, 2);
    }

    var countdownState = {};
    var clrState = {};
    var pingState = {};

    function startIntervalUpdate() {
        countdownState.interval = setInterval(function() { intervalUpdateContestTimes() }, 1000);
    }

    function updateContestTimes(e, iconbase) {
        if (countdownState.interval) {
            clearInterval(countdownState.interval);
            countdownState.interval = null;
        }

        if (countdownState.initialOffset) {
            clearTimeout(countdownState.initialOffset);
            countdownState.initialOffset = null;
        }

        e.started = e.startTimeDelta <= 0;

        if (countdownState.contest && !countdownState.contest.started && e.started) {
            notifyMe(e.name, iconbase, 'Соревнование началось');
            location.reload();
        }

        countdownState.contest = e;

        if (!e.started) {
            e.timeval = e.startTimeDelta
        } else if (e.freezeTimeDelta > 0 && e.freezeTimeDelta < e.endTimeDelta) {
            e.timeval = e.freezeTimeDelta
        } else if (e.endTimeDelta > 0) {
            e.timeval = e.endTimeDelta
        } else if (e.exposeTimeDelta > 0) {
            e.timeval = exposeTimeDelta
        } else {
            e.timeval = 0
        }

        if (e.timeval != 0) {
            e.flip = new Date(Date.now() + e.timeval);
        }

        intervalUpdateContestTimes();

        if (e.timeval != 0) {
            offset = (e.flip - Date.now()) % 1000;
            if (offset > 10) {
                countdownState.initialOffset = setTimeout(function() { startIntervalUpdate() }, offset - 1);
            } else {
                startIntervalUpdate()
            }
        }
    }

    function intervalUpdateContestTimes() {
        if (countdownState.contest.started) {
            $("#contest-starting-in").hide();
            $("#contest-ending-in").show();
        } else {
            $("#contest-starting-in").show();
            $("#contest-ending-in").hide();
        }

        if (countdownState.contest.ended) {
            $("contest-ended").show();
            $("contest-timer").hide();
        } else {
            $("contest-ended").hide();
            $("contest-timer").show();
        }

        if (countdownState.contest.flip) {
            $("#contest-timer-counter").text(formatSeconds(((countdownState.contest.flip - Date.now()) /  1000) >> 0));
        }
    }

function listenOnEvents(path, iconbase, ackMessagePath) {
    var source = new EventSource(path);

    var reconnect = function() {
        source.close();
        window.setTimeout(function() { listenOnEvents(path) }, 1000);
    }

    var resetPingState = function() {
        if (pingState && pingState.tm) {
            clearTimeout(pingState.tm);
            pingState.tm = null;
        }
        pingState.tm = setTimeout(function() { reconnect(); }, 60000);
    }

    source.onopen = function() {
        $("#connected1").removeClass("badge-error");
        $("#connected1").text("+");
        resetPingState();
    }

    source.addEventListener('submit', function(ev) {
        var obj = JSON.parse(ev.data);

        var tr = $('#result-' + obj.submitId);
        if (tr.length) {
            tr.html(obj.result.message);
        } else {
            $('#submits > tbody').prepend('<tr><th scope="row">' + obj.submitId + '</th></tr>')
        }

        if (ackMessagePath) {
            var icon = iconbase + 'error-icon.gif';
            if (obj.result.success) {
                icon = iconbase + 'baloons/baloon-' + obj.problem.toLowerCase() + '.png';
            }
            console.log(obj.submitId)
            notifyMe("Задача " + obj.problem, icon, obj.result.message)
            if (obj.msgid) {
                $.post(ackMessagePath, {'msgid': obj.msgid});
            }
        }
    })

    source.addEventListener('clarificationAnswered', function(ev) {
        var obj = JSON.parse(ev.data);
        if (ackMessagePath) {
            notifyMe("На вопрос по задаче " + obj.problem + " получен ответ", iconbase + 'icpc_logo.png', obj.text);
            if (obj.msgid) {
                $.post(ackMessagePath, {'msgid': obj.msgid});
            }
        }
    })

    source.addEventListener('clarificationState', function(ev) {
        var obj = JSON.parse(ev.data);
        var clrp = $("#clrPending")
        if (obj.ids.length) {
            clrp.show();
        } else {
            clrp.hide();
        }
    })

    source.addEventListener('clarificationPosted', function(ev) {
        var obj = JSON.parse(ev.data);
        console.log(obj);
        var clrp = $("#clrPending");
        clrp.text("!");
        clrp.show();
        var msg = "Сообщение жюри";
        if (obj.problem) {
          msg += " по задаче " + obj.problem;
        }
        notifyMe(msg, iconbase + 'icpc_logo.png', obj.text);
    })

    source.addEventListener('contest', function(ev) {
        var obj = JSON.parse(ev.data);
        updateContestTimes(obj, iconbase);
    })

    source.addEventListener('ping', function(ev) {
        resetPingState();
    })

    source.onerror = function(ev) {
        $("#connected1").addClass("badge-error");
        $("#connected1").text("!");
        console.log("Error")
        console.log(ev)
        if (pingState && pingState.tm) {
            clearTimeout(pingState.tm);
            pingState.tm = null;
        }

        if (source.readyState == 2) {
            reconnect();
        }
    }
}

function listenOnAdmin(path, iconbase) {
    var source = new EventSource(path);

    var reconnect = function() {
        source.close();
        window.setTimeout(function() { listenOnEvents(path) }, 1000);
    }

    var resetPingState = function() {
        if (pingState && pingState.tm) {
            clearTimeout(pingState.tm);
            pingState.tm = null;
        }
        pingState.tm = setTimeout(function() { reconnect(); }, 60000);
    }

    source.onopen = function() {
        $("#connected1").removeClass("badge-error");
        $("#connected1").text("+");
        resetPingState();
    }

    source.addEventListener('submit', function(ev) {
        var obj = JSON.parse(ev.data);

        var tr = $('#result-' + obj.submitId);
        if (tr.length) {
            tr.html(obj.result.message);
        } else {
            $('#submits > tbody').prepend('<tr><th scope="row">' + obj.submitId + '</th></tr>')
        }
    })

    source.addEventListener('clarificationRequestState', function(ev) {
        var obj = JSON.parse(ev.data);
        var clrp = $("#clrPending")
        clrp.text(obj.pending);
        if (obj.pending) {
            clrp.show()
            if (obj.newRequest) {
                notifyMe("Новый вопрос жюри", iconbase + 'icpc_logo.png', 'Новый вопрос жюри')
            }
        } else {
            clrp.hide()
        }
    })

    source.addEventListener('contest', function(ev) {
        var obj = JSON.parse(ev.data);
        updateContestTimes(obj, iconbase);
    })

    source.addEventListener('ping', function(ev) {
        resetPingState();
    })

    source.onerror = function(ev) {
        $("#connected1").addClass("badge-error");
        $("#connected1").text("!");
        console.log("Error")
        console.log(ev)
        if (pingState && pingState.tm) {
            clearTimeout(pingState.tm);
            pingState.tm = null;
        }

        if (source.readyState == 2) {
            reconnect();
        }
    }
}