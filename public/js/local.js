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

        var div = document.createElement("div");

        div.innerHTML = n_body;
        var text = div.innerText || div.textContent || div.text;

        var notification = new Notification(n_title, {
          icon: n_icon,
          body: text,
        });

        notification.addEventListener('click', function () {
            window.focus()
            this.close()
        });

        window.setTimeout(function() {
            notification.close()
            }, 600000);

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

    function startIntervalUpdate(iconbase, admin) {
        if (!countdownState.interval)
            countdownState.interval = setInterval(function() { intervalUpdateContestTimes(iconbase, admin) }, 1000);
    }

    function updateContestState(e) {
        var now = Date.now();
        if (e.startTime > now) {
            e.state = "before"
            e.flipMs = e.startTime - now
        } else if (e.endTime > now) {
            e.state = "running"
            e.flipMs = e.endTime - now
        } else {
            e.state = "finished"
            e.flipMs = null
        }
    }

    function updateContestTimes(e, iconbase, admin) {
        e.norm = Date.now()
        e.startTime = new Date(e.norm + e.startTimeDelta)
        e.endTime = new Date(e.norm + e.endTimeDelta)

        e.prevState = countdownState.contest ? countdownState.contest.state : null;
        countdownState.contest = e;

        intervalUpdateContestTimes(iconbase, admin);
    }

    function intervalUpdateContestTimes(iconbase, admin) {
        updateContestState(countdownState.contest)
        if (countdownState.contest.prevState != countdownState.contest.state) {
            var notify = null;
            switch (countdownState.contest.state) {
                case "before":
                    $("#contest-timer").show();
                    $("#contest-ended").hide();
                    $("#contest-starting-in").show();
                    $("#contest-ending-in").hide();
                    startIntervalUpdate(iconbase, admin);
                    break;
                case "running":
                    notify = "началось";
                    $("#contest-timer").show();
                    $("#contest-ended").hide();
                    $("#contest-starting-in").hide();
                    $("#contest-ending-in").show();
                    startIntervalUpdate(iconbase, admin);
                    break;
                case "finished":
                    notify = "закончилось"
                    $("#contest-timer").hide();
                    $("#contest-ended").show();
                    $("#contest-starting-in").hide();
                    $("#contest-ending-in").hide();
                    if (countdownState.interval) {
                        clearInterval(countdownState.interval);
                        countdownState.interval = null;
                    }
            }
            if (countdownState.contest.prevState && notify) {
                notifyMe(countdownState.contest.name, iconbase, "Соревнование " + notify);
            }
            if (countdownState.contest.prevState == "before" && countdownState.contest.state == "running" && !admin) {
                location.reload();
            }
        }

        if (countdownState.contest.flipMs) {
            $("#contest-timer-counter").text(formatSeconds((countdownState.contest.flipMs /  1000) >> 0));
        }
        countdownState.contest.prevState = countdownState.contest.state
    }

function toggleClarification(target, btnid) {
    $('#cl-button-' + btnid).html('...');
    $.post(target, {}, function() {
        $('#cl-button-' + btnid).html('<span class="caret"></span>')
    });
}

function deleteClarification(target, btnid) {
    $('#cl-button-' + btnid).html('...');
    $.post(target, {}, function() {
        $('#cl-button-' + btnid).html('<span class="caret"></span>')
        $('#cl-row-' + btnid).remove();
    });
}

function ackWaiterTask(target, btnid) {
    $('#wa-' + btnid).html('...');
    $.post(target, {}, function() {
        $('#wa-' + btnid).html('<span class="caret"></span>')
    });
}

function unackWaiterTask(target, btnid) {
    $('#wa-' + btnid).html('...');
    $.post(target, {}, function() {
        $('#wa-' + btnid).html('<span class="caret"></span>')
    });
}

function deleteWaiterTask(target, btnid) {
    $('#wa-del-' + btnid).html('...');
    $.post(target, {}, function() {
        $('#wa-del-' + btnid).html('<span class="caret"></span>')
    });
}

function rejudgePost(target, btnid) {
    $('#rejudge-' + btnid).html('...');
    $('#result-' + btnid).html('...')
    $.post(target, {}, function() {
        $('#rejudge-' + btnid).html('<span class="caret"></span>')
    });
}

function reprintPost(target, btnid) {
    $('#rejudge-' + btnid).html('...');
    $.post(target, {}, function() {
        $('#rejudge-' + btnid).html('<span class="caret"></span>')
    });
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
            if (obj.msgid) {
                setTimeout(function() {
                var exists = localStorage.getItem(obj.msgid);
                if (!exists) {
                    var icon = iconbase + 'error-icon.gif';
                    if (obj.result.success) {
                        icon = iconbase + 'baloons/baloon-' + obj.problem.toLowerCase() + '.png';
                    }
                    notifyMe("Задача " + obj.problem, icon, obj.result.message)
                    localStorage.setItem(obj.msgid, "true");
                }
                $.post(ackMessagePath, {'msgid': obj.msgid});
                }, Math.random() * 3000)
            }
        }
    })

    source.addEventListener('clarificationAnswered', function(ev) {
        var obj = JSON.parse(ev.data);
        if (ackMessagePath) {
            if (obj.msgid) {
                setTimeout(function() {
                    var exists = localStorage.getItem(obj.msgid);
                    if (!exists) {
                      notifyMe("На вопрос по задаче " + obj.problem + " получен ответ", iconbase + 'icpc_logo.png', obj.text);
                      localStorage.setItem(obj.msgid, "true");
                    }
                    $.post(ackMessagePath, {'msgid': obj.msgid});
                }, Math.random() * 3000)
            }
        }
    })

    source.addEventListener('clarificationState', function(ev) {
        var obj = JSON.parse(ev.data);
        var clrp = $("#clrPending")
        if (obj.unseen) {
            clrp.show();
        } else {
            clrp.hide();
        }
    })

    source.addEventListener('clarificationPosted', function(ev) {
        var obj = JSON.parse(ev.data);
        setTimeout(function() {
        var exists = localStorage.getItem(obj.text);
        console.log(exists);
        if (!exists) {
            var clrp = $("#clrPending");
            clrp.text("!");
            clrp.show();
            var msg = "Сообщение жюри";
            if (obj.problem) {
              msg += " по задаче " + obj.problem;
            }
            notifyMe(msg, iconbase + 'icpc_logo.png', obj.text);
            localStorage.setItem(obj.text, "true");
        }
                }, Math.random() * 3000)

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
        console.log("Error: " + ev)
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
        updateContestTimes(obj, iconbase, true);
    })

    source.addEventListener('ping', function(ev) {
        resetPingState();
    })

    source.addEventListener('waiterTaskUpdated', function(ev) {
        var obj = JSON.parse(ev.data);

        var tr = $('#wt-id-' + obj.id);
        if (tr.length) {
            tr.replaceWith(obj.content);
        } else {
            $('#waitertasks > tbody').prepend(obj.content);
        }
    })

    source.addEventListener('waiterTaskHeader', function(ev) {
        var obj = JSON.parse(ev.data);
        var p = $('#tasksPending');
        if (obj.outstanding) {
            p.text(obj.outstanding);
            p.show();
        } else {
            p.text('-');
            p.hide();
        }
    })

    source.addEventListener('waiterTaskDeleted', function(ev) {
        var obj = JSON.parse(ev.data);

        var tr = $('#wt-id-' + obj.id);
        if (tr.length) {
            tr.remove();
        }
    })

    source.onerror = function(ev) {
        $("#connected1").addClass("badge-error");
        $("#connected1").text("!");
        console.log("Error: " + ev)
        if (pingState && pingState.tm) {
            clearTimeout(pingState.tm);
            pingState.tm = null;
        }

        if (source.readyState == 2) {
            reconnect();
        }
    }
}