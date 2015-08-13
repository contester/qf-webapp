function maybeCheckNotifyPermission() {
    if (Notification.permission !== "granted") {
        Notification.requestPermission();
        return false;
    }
    return true;
}

    function notifyMe(n_title, n_body) {
      if (!Notification) {
        alert('Desktop notifications not available in your browser. Try Chromium.');
        return;
      }

  if (Notification.permission !== "granted")
    Notification.requestPermission();
  else {
    var notification = new Notification(n_title, {
      icon: '@routes.Assets.at("icpc_logo.png")',
      body: n_body,
    });

    notification.addEventListener('click', function () {
    window.focus()
            this.close()
    })

    window.setTimeout(function() {
        notification.close()
        }, 120000)

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

    function updateContestTimes(e) {
        if (vticker) {
            clearInterval(vticker);
        }

        if (ctstate && !ctstate.started && e.started) {
            notifyMe('Соревнование началось', e.name);
            location.reload();
        }

        ctstate = e;
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

function listenOnSocket(path) {

    $(function() {
            var chatSocket = new ReconnectingWebSocket(path)

            var receiveEvent = function(event) {
                var obj = JSON.parse(event.data);

                if (obj.kind == 'contest') {
                    updateContestTimes(obj.data);
                }

                if (obj.kind == 'submit') {
                    notifyMe("Problem " + obj.data.problem, obj.data.result.message)
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
