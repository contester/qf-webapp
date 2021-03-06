function askForPermission() {
    if (Notification.permission !== "granted") {
        Notification.requestPermission();
    }
}

function cleanupBody(body) {
        var div = document.createElement("div");
        div.innerHTML = body;
        return div.innerText || div.textContent || div.text;
}

function notifyMe(n_title, n_icon, n_body) {
    askForPermission();
    var notification = new Notification(n_title, {
      icon: n_icon,
      body: cleanupBody(n_body),
    });

    notification.addEventListener('click', function () {
        window.focus();
        this.close();
    });

    window.setTimeout(function() {notification.close();}, 600000);
}

function pad(num, size) {
    'use strict';
    var s = num+"";
    while (s.length < size) s = "0" + s;
    return s;
}

function formatSeconds(s) {
    'use strict';
    var seconds = s % 60;
    var minutes = (s / 60) >> 0;
    var hours = (minutes / 60) >> 0;
    minutes %= 60;

    return pad(hours, 2) + ':' + pad(minutes, 2) + ':' + pad(seconds, 2);
}

    var countdownState = {};
    var clrState = {};

    function startIntervalUpdate(iconbase, admin) {
        if (!countdownState.interval)
            countdownState.interval = setInterval(function() { intervalUpdateContestTimes(iconbase, admin); }, 1000);
    }

    function updateContestState(e) {
        var now = Date.now();
        if (e.startTime > now) {
            e.state = "before";
            e.flipMs = e.startTime - now;
        } else if (e.endTime > now) {
            e.state = "running";
            e.flipMs = e.endTime - now;
        } else {
            e.state = "finished";
            e.flipMs = null;
        }
    }

    function updateContestTimes(e, iconbase, admin) {
        e.norm = Date.now();
        e.startTime = new Date(e.norm + e.startTimeDelta);
        e.endTime = new Date(e.norm + e.endTimeDelta);

        e.prevState = countdownState.contest ? countdownState.contest.state : null;
        countdownState.contest = e;

        intervalUpdateContestTimes(iconbase, admin);
    }

    function intervalUpdateContestTimes(iconbase, admin) {
        updateContestState(countdownState.contest);
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
                    notify = "закончилось";
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
        countdownState.contest.prevState = countdownState.contest.state;
    }

function btn(target, btnid) {
    var b = $('#' + btnid);
    b.html('...');
    return $.post(target).done(function() { b.html('<span class="caret"></span>'); }).fail(function() { b.html('!'); });
}

function btnDelete(target, btnid, rowid) {
    return btn(target, btnid).done(function() {$('#' + rowid).remove(); });
}

function rejudgePost(target, btnid) {
    $('#result-' + btnid).html('...');
    return btn(target, 'rejudge-' + btnid);
}


function setConnectedBadge(state) {
    var conn = $('#connected1');
    if (state) {
        conn.removeClass("badge-error");
        conn.text("+");
    } else {
        conn.addClass("badge-error");
        conn.text("No connection");
    }
}

function listenV2(path, setup, state) {
    var source = new EventSource(path);
    var reconnect = function() {
        source.close();
        window.setTimeout(function() { listenV2(path, setup, state); }, 1000);
    };

    var pingState = {};
    var clearPingState = function() {
        if (pingState && pingState.tm) {
            clearTimeout(pingState.tm);
            pingState.tm = null;
        }
    };

    var resetPingState = function() {
        clearPingState();
        pingState.tm = setTimeout(function() { reconnect(); }, 60000);
    };

    source.onopen = function() {
        if (state) {
            state(true);
        }
        resetPingState();
    };

    source.addEventListener('ping', function(ev) {
        resetPingState();
    });

    source.onerror = function(ev) {
        state(false);
        console.log("Error: " + ev);
        clearPingState();
        if (source.readyState == 2) {
            reconnect();
        }
    };

    var addJsonListener = function(name, handler) {
        source.addEventListener(name, function(ev) {
            var p = JSON.parse(ev.data);
            resetPingState();
            handler(p);
        });
    };

    setup(addJsonListener);

    return source;
}

function listenOnEvents(path, iconbase, ackMessagePath) {
    var setup = function(add) {
        add('submit', function(obj) {
            var tr = $('#result-' + obj.submitId);
            if (tr.length) {
                tr.html(obj.result.message);
            } else {
                $('#submits > tbody').prepend('<tr><th scope="row">' + obj.submitId + '</th></tr>');
            }

            if (ackMessagePath) {
                if (obj.msgid) {
                    setTimeout(function() {
                    var exists = sessionStorage.getItem(obj.msgid);
                    if (!exists) {
                        var icon = iconbase + 'error-icon.gif';
                        if (obj.result.success) {
                            icon = iconbase + 'baloons/baloon-' + obj.problem.toLowerCase() + '.png';
                        }
                        notifyMe("Problem " + obj.problem, icon, obj.result.message);
                        sessionStorage.setItem(obj.msgid, "true");
                    }
                    $.post(ackMessagePath, {'msgid': obj.msgid});
                    }, Math.random() * 3000);
                }
            }
        });

        add('clarificationAnswered', function(obj) {
            if (ackMessagePath) {
                if (obj.msgid) {
                    setTimeout(function() {
                        var exists = sessionStorage.getItem(obj.msgid);
                        if (!exists) {
                          notifyMe("Your question about problem " + obj.problem + " was answered", iconbase + 'icpc_logo.png', obj.text);
                          sessionStorage.setItem(obj.msgid, "true");
                        }
                        $.post(ackMessagePath, {'msgid': obj.msgid});
                    }, Math.random() * 3000);
                }
            }
        });

        add('clarificationState', function(obj) {
            var clrp = $("#clrPending");
            if (obj.unseen) {
                clrp.show();
            } else {
                clrp.hide();
            }
        });

        add('clarificationPosted', function(obj) {
            setTimeout(function() {
                var exists = sessionStorage.getItem(obj.text);
                if (!exists) {
                    var clrp = $("#clrPending");
                    clrp.text("!");
                    clrp.show();
                    var msg = "Clarification posted";
                    if (obj.problem) {
                      msg += " for problem " + obj.problem;
                    }
                    notifyMe(msg, iconbase + 'icpc_logo.png', obj.text);
                    sessionStorage.setItem(obj.text, "true");
                }
            }, Math.random() * 3000);

        });


        add('contest', function(obj) {
            updateContestTimes(obj, iconbase);
        });
    };

    return listenV2(path, setup, setConnectedBadge);
}

function listenOnAdmin(path, iconbase) {
    var setup = function(add) {
        add('submit', function(obj) {
            var tr = $('#result-' + obj.submitId);
            if (tr.length) {
                tr.html(obj.result.message);
            } else {
                $('#lastSubmitId').html('<b>' + obj.submitId + '</b>');
            }
        });

        add('adminSubmit', function(obj) {
            var tr = $('#result-row-' + obj.submitId);
            if (tr.length) {
                tr.html(obj.rendered);
            } else {
                $('#submits > tbody').prepend(obj.rendered);
            }
        });

        add('clarificationRequestState', function(obj) {
            var clrp = $("#clrPending");
            clrp.text(obj.pending);
            if (obj.pending) {
                clrp.show();
                if (obj.newRequest) {
                    notifyMe("Новый вопрос жюри", iconbase + 'icpc_logo.png', 'Новый вопрос жюри');
                }
            } else {
                clrp.hide();
            }
        });

        add('contest', function(obj) {
            updateContestTimes(obj, iconbase, true);
        });

        add('waiterTaskUpdated', function(obj) {
            var tr = $('#wt-id-' + obj.id);
            if (obj.content) {
            if (tr.length) {
                tr.replaceWith(obj.content);
            } else {
                $('#waitertasks > tbody').prepend(obj.content);
            }
            } else {
                tr.remove();
            }
        });

        add('waiterTaskHeader', function(obj) {
            var p = $('#tasksPending');
            if (obj.outstanding) {
                p.text(obj.outstanding);
                p.show();
            } else {
                p.text('-');
                p.hide();
            }
            if (obj.text) {
                notifyMe("Новое задание дежурным", iconbase + 'icpc_logo.png', obj.text);
            }
        });
    };

    return listenV2(path, setup, setConnectedBadge);
}