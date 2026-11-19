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
            if (countdownState.contest.prevState && notify && !countdownState.quiet) {
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
    if (!window.confirm('Rejudge submit ' + btnid + '?')) {
        return;
    }
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
        // Server-rendered rows (not stored): the finished attempt's row.
        add('submitRows', function(obj) {
            obj.rows.forEach(function(row) {
                var old = $('#result-' + row.id).closest('tr');
                if (old.length) {
                    old.replaceWith(row.html);
                } else {
                    $('#submits > tbody').prepend(row.html);
                }
            });
        });

        // The popup event (stored until acked). Also keeps the verdict cell right if the rows did not come.
        add('submit', function(obj) {
            // The status page fed by the live monitor (listenOnStatus) takes care of its rows.
            if (window.statusLive) { return; }
            var tr = $('#result-' + obj.submitId);
            if (tr.length) {
                tr.toggleClass('success', !!obj.result.success);
                var href = tr.closest('tr').find('th a').attr('href');
                if (href) {
                    tr.empty().append($('<a>').attr('href', href).text(obj.result.message));
                } else {
                    tr.text(obj.result.message);
                }
            } else {
                $('#submits > tbody').prepend('<tr><th scope="row">' + obj.submitId + '</th><td id="result-' + obj.submitId + '"></td></tr>');
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

// The admin status page, fed by the live monitor. A row that is on the page is replaced where it stands (the verdict
// arrives, the statistics fill in). A submit that is not on the page is held back, never put in: the table does not
// move under the cursor. A bar above it counts them and a click puts them in.
//   canRejudge: the viewer sees the rejudge buttons; newest: this is the first page; teamFilter: team id, 0 for all.
function listenOnStatus(path, canRejudge, newest, teamFilter) {
    var held = {};
    var down = false;
    var bar = $('#newSubmits');
    var rowOf = function(obj) {
        var tr = $($.parseHTML($.trim(canRejudge ? obj.rw : obj.ro)));
        if (teamFilter) { tr.find('a.team-link').replaceWith(function() { return $(this).text(); }); }
        return tr;
    };
    var shownIds = function() {
        return $('#submits > tbody > tr').map(function() { return parseInt(this.id.replace('result-row-', ''), 10); }).get();
    };
    // On the first page everything above its oldest row belongs to it; whatever is older is on another page.
    var belongsHere = function(id) {
        var ids = shownIds();
        return ids.length === 0 || id > Math.min.apply(null, ids);
    };
    // Puts a row among the shown ones, newest first, where its id says.
    var insertRow = function(id, tr) {
        var after = $('#submits > tbody > tr').filter(function() {
            return parseInt(this.id.replace('result-row-', ''), 10) < id;
        }).first();
        if (after.length) { after.before(tr); } else { $('#submits > tbody').append(tr); }
    };
    var showBar = function() {
        var n = Object.keys(held).length;
        if (down) {
            bar.text('Связь прерывалась, возможны пропущенные попытки. Обновите страницу.');
        } else if (n) {
            bar.text('Новых попыток: ' + n + '. Показать');
        } else {
            bar.hide();
            return;
        }
        bar.show();
    };
    bar.on('click', function() {
        if (down) { window.location.reload(); return; }
        Object.keys(held).map(Number).forEach(function(id) { insertRow(id, held[id]); });
        held = {};
        showBar();
    });

    var setup = function(add) {
        add('ready', function() {
            // A drop in between may have lost rows; the first connection is not one.
            if (down) { showBar(); }
        });
        add('statusRow', function(obj) {
            if (teamFilter && obj.team !== teamFilter) { return; }
            var on = $('#result-row-' + obj.id);
            if (on.length) {
                on.replaceWith(rowOf(obj));
            } else if (held[obj.id]) {
                held[obj.id] = rowOf(obj);
            } else if (newest && belongsHere(obj.id)) {
                held[obj.id] = rowOf(obj);
                showBar();
            }
        });
    };

    var opened = false;
    return listenV2(path, setup, function(state) {
        if (state) {
            // Opened again after a drop: events were missed.
            if (opened && down) { showBar(); }
            opened = true;
        } else if (opened) {
            down = true;
        }
    });
}

function listenOnContester(path, table) {
    var labels = {
        teamFeed: 'Team feed', adminMonitor: 'Admin monitor', adminFeed: 'Admin feed', statusRows: 'Status rows'
    };
    var names = {};
    $('#contester-names span').each(function() { names[$(this).data('id')] = $(this).text(); });
    var t = $(table);
    var cell = function(tag, text) { return $('<' + tag + '/>').text(text); };
    var setup = function(add) {
        add('counts', function(obj) {
            var head = $('<tr/>').append(cell('th', 'Contest'));
            obj.kinds.forEach(function(k) { head.append(cell('th', labels[k] || k)); });
            var total = obj.kinds.map(function() { return 0; });
            var body = [];
            Object.keys(obj.contests).sort(function(a, b) { return a - b; }).forEach(function(cid) {
                var row = $('<tr/>').append(cell('td', names[cid] ? cid + ' - ' + names[cid] : cid));
                obj.contests[cid].forEach(function(n, i) {
                    total[i] += n;
                    row.append(cell('td', n || '').addClass(n ? '' : 'text-muted'));
                });
                body.push(row);
            });
            var foot = $('<tr/>').append(cell('th', 'Total'));
            total.forEach(function(n) { foot.append(cell('th', n)); });
            t.find('thead').empty().append(head);
            t.find('tbody').empty().append(body);
            t.find('tfoot').empty().append(foot);
        });
    };
    return listenV2(path, setup, function(ok) {
        var badge = $('#contester-state');
        badge.toggleClass('badge-error', !ok).text(ok ? '+' : 'No connection');
    });
}

function listenOnAdmin(path, iconbase) {
    var setup = function(add) {
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
    };

    return listenV2(path, setup, setConnectedBadge);
}
// Admin live monitor (points). The markup must match app/views/livetable.scala.html exactly (contract documented
// there, pinned by test/resources/live/livetable.html; check with `node test/js/livetable-check.js`).
function liveEsc(s) {
    return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;').replace(/'/g, '&#x27;');
}

// A place is a number, or null (shown empty) for a team that is not rated.
function liveRankText(place) {
    return (place === null || place === undefined) ? '' : String(place);
}

// One problem cell, <td> included. c: {b: best as text (absent: none), n: attempts, f: full, p: in flight}; max as text.
function liveCellHtml(c, max) {
    c = c || {n: 0, f: false, p: 0};
    var has = c.b !== undefined && c.b !== null;
    var cls = 'text-center points-cell' + (c.f ? ' full-score' : '') + (has ? '' : ' text-muted');
    var h = has ? liveEsc(c.b) + '<br/><small class="text-muted">из ' + liveEsc(max) + '</small>' : '—';
    if (c.p > 0) h += '<br/><span class="text-warning">?' + c.p + '</span>';
    return '<td class="' + cls + '">' + h + '</td>';
}

// What colour a cell flashes when it changes, from its new markup: in flight, with points, or without.
function liveCellKind(html) {
    if (html.indexOf('text-warning') >= 0) return 'pend';
    if (html.indexOf('<small') >= 0) return 'ok';
    return 'fail';
}

// Which parts of a replaced row flash (see flashstyle.scala.html): the cells whose markup changed, and the whole
// row when it moved or its place changed. oldCells/newCells are the outerHTML of the problem cells, in order.
function liveFlashPlan(oldCells, newCells, moved) {
    return {
        row: !!moved,
        cells: newCells.map(function(h, i) { return h !== oldCells[i] ? liveCellKind(h) : null; })
    };
}

function liveCellsOf(tr) {
    return tr.children('td.points-cell').map(function() { return this.outerHTML; }).get();
}

// Adds the flash classes to a row that replaced an old one, and takes them off when the animation is over.
function liveFlash(tr, oldCells, moved) {
    var tds = tr.children('td.points-cell');
    var plan = liveFlashPlan(oldCells, liveCellsOf(tr), moved);
    var classes = [];
    if (plan.row) { tr.addClass('flash-move'); classes.push('flash-move'); }
    tds.each(function(i) {
        if (plan.cells[i]) { $(this).addClass('flash-' + plan.cells[i]); classes.push('flash-' + plan.cells[i]); }
    });
    if (classes.length) {
        setTimeout(function() {
            tr.removeClass('flash-move');
            tds.removeClass('flash-ok flash-pend flash-fail');
        }, 2600);
    }
}

function liveRowHtml(problems, row) {
    var h = '<tr data-team="' + row.team + '"><th scope="row">' + liveEsc(liveRankText(row.place)) + '</th>' +
        '<td>' + row.number + '</td><td>' + liveEsc(row.name) + '<br/><small>' + liveEsc(row.school) + '</small></td>';
    for (var i = 0; i < problems.length; i++) {
        h += liveCellHtml(row.cells ? row.cells[problems[i].id] : null, problems[i].max);
    }
    return h + '<td class="text-center"><strong>' + liveEsc(row.total) + '</strong></td></tr>';
}

function liveTableHtml(board, labels) {
    var h = '<table class="table table-hover table-bordered table-responsive points-board"><thead><tr>' +
        '<th>' + liveEsc(labels.rank) + '</th><th>№</th><th>' + liveEsc(labels.team) + '</th>';
    for (var i = 0; i < board.problems.length; i++) {
        var p = board.problems[i];
        h += '<th style="text-align: center;" title="' + liveEsc(p.name) + '">' + liveEsc(p.id) +
            '<div style="font-size: 65%;">' + liveEsc(p.name) + ' (' + liveEsc(p.max) + ')</div></th>';
    }
    h += '<th style="text-align: center;">' + liveEsc(labels.total) + '</th></tr></thead><tbody>\n';
    for (var j = 0; j < board.rows.length; j++) {
        h += liveRowHtml(board.problems, board.rows[j]) + '\n';
    }
    return h + '</tbody></table>';
}

// Fills containerSelector from the admin live stream at eventsPath.
function liveMonitor(eventsPath, containerSelector) {
    var box = $(containerSelector);
    var labels = {
        rank: box.attr('data-label-rank') || '',
        team: box.attr('data-label-team') || '',
        total: box.attr('data-label-total') || ''
    };
    var problems = [];

    var body = function() { return box.find('tbody').first(); };

    var setup = function(add) {
        add('board', function(b) {
            problems = b.problems;
            box.html(liveTableHtml(b, labels));
        });

        add('row', function(e) {
            var tbody = body();
            if (!tbody.length) return;
            var old = tbody.children('tr[data-team="' + e.team + '"]');
            var had = old.length > 0;
            var oldIndex = old.index();
            var oldRank = old.children('th').first().text();
            var oldCells = liveCellsOf(old);
            old.remove();
            var tr = $(liveRowHtml(problems, e.row));
            var rows = tbody.children('tr');
            if (e.pos < rows.length) {
                rows.eq(e.pos).before(tr);
            } else {
                tbody.append(tr);
            }
            rows = tbody.children('tr');
            if (had) liveFlash(tr, oldCells, oldIndex !== tr.index() || oldRank !== tr.children('th').first().text());
            for (var i = 0; i < e.ranks.length; i++) {
                rows.eq(e.rankFrom + i).children('th').first().text(liveRankText(e.ranks[i]));
            }
        });
    };

    return listenV2(eventsPath, setup, setConnectedBadge);
}
