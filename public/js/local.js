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

    var vticker;
    var ctstate;

    function updateContestTimes(e, iconbase) {
        if (vticker) {
            clearInterval(vticker);
            vticker = null;
        }

        e.started = e.startTimeDelta <= 0;

        if (ctstate && !ctstate.started && e.started) {
            notifyMe(e.name, iconbase, 'Соревнование началось');
            location.reload();
        }

        ctstate = e;

        if (!e.started) {
            e.timeval = e.startTimeDelta
        } else if (e.freezeTimeDelta > 0 && e.freezeTimeDelta < e.endTimeDelta) {
            e.timeval = e.freezeTimeDelta
        } else if (e.endTimeDelta > 0) {
            e.timeval = e.endTimeDelta
        } else if (e.exposeTimeDelta > 0) {
            e.timeval = exposeTimeDelta
        }

        if (e.timeval != 0) {
            ctstate.flip = new Date(Date.now() + e.timeval);
        }

        intervalUpdateContestTimes();

        if (e.timeval != 0) {
            vticker = setInterval(function() { intervalUpdateContestTimes() }, 1000);
        }
    }

    function intervalUpdateContestTimes() {
        if (ctstate.started) {
            $("#contest-starting-in").hide();
            $("#contest-ending-in").show();
        } else {
            $("#contest-starting-in").show();
            $("#contest-ending-in").hide();
        }

        if (ctstate.ended) {
            $("contest-ended").show();
            $("contest-timer").hide();
        } else {
            $("contest-ended").hide();
            $("contest-timer").show();
        }

        if (ctstate.flip) {
            $("#contest-timer-counter").text(formatSeconds(((ctstate.flip - Date.now()) /  1000) >> 0));
        }
    }

function listenOnSocket(path, iconbase) {
    $(function() {
            var chatSocket = new ReconnectingWebSocket(path)

            var receiveEvent = function(event) {
                var obj = JSON.parse(event.data);

                if (obj.kind == 'contest') {
                    updateContestTimes(obj.data, iconbase + 'icpc_logo.png');
                }

                if (obj.kind == 'submit') {
                    var icon = iconbase + 'error-icon.gif';
                    if (obj.data.result.success) {
                        icon = iconbase + 'baloons/baloon-' + obj.data.problem.toLowerCase() + '.png';
                    }
                    console.log(obj.data.submitId)
                    $('#result-' + obj.data.submitId).html(obj.data.result.message)
                    notifyMe("Problem " + obj.data.problem, icon, obj.data.result.message)
                }

                if (obj.kind == 'custom') {
                    notifyMe("Server-side test", iconbase + 'icpc_logo.png', 'Completed.');
                }

                if (obj.msgid) {
                    chatSocket.send(JSON.stringify({ 'kind': 'ack', 'msgid': obj.msgid }))
                }
            }

            chatSocket.onmessage = receiveEvent
            chatSocket.onerror = function(ev) {
                console.log("error " + ev)
            }
            chatSocket.onclose = function(ev) {
                console.log("close " + ev)
            }
        })

}

function listenOnEvents(path) {
    var source = new EventSource(path);

    source.onopen = function() {
        $("#connected1").removeClass("badge-error");
        $("#connected1").text("+");
    }

    source.onmessage = function(ev) {
        console.log(ev.data)
    }

    source.addEventListener('submit', function(ev) {
        var obj = JSON.parse(event.data);
        console.log(obj);
        var tr = $('#result-' + obj.submitId);
        if (tr.length) {
            tr.html(obj.result.message);
        } else {
            console.log("huh")
            $('#submits > tbody').prepend('<tr><th scope="row">' + obj.submitId + '</th></tr>')
        }
    })

    source.onerror = function(ev) {
        $("#connected1").addClass("badge-error");
        $("#connected1").text("!");
        console.log("Error")
        console.log(ev)

        if (source.readyState == 2) {
            source.close()
            window.setTimeout(function() { listenOnEvents(path) }, 1000)
        }
    }
}