// Manual check (not run by sbt): node test/js/status-check.js
// The admin status page (listenOnStatus in main.js) against a DOM: rows that are on the page are replaced where they
// stand, a submit that is not is held back until the bar is clicked, and only the pages it belongs to hold it.
//
// Needs jsdom, which the project does not depend on: `npm install jsdom` somewhere and run with
// NODE_PATH=<that>/node_modules, or point JSDOM_PATH at the module. jQuery is taken from the build's webjars
// (run `sbt compile` or `sbt test` once), or from JQUERY_PATH.
'use strict';
const fs = require('fs');
const path = require('path');
const assert = require('assert');

const root = path.join(__dirname, '..', '..');
let JSDOM;
try {
    JSDOM = require(process.env.JSDOM_PATH || 'jsdom').JSDOM;
} catch (e) {
    console.error('status-check: jsdom is not installed (npm install jsdom; NODE_PATH or JSDOM_PATH)');
    process.exit(2);
}
const jqueryPath = process.env.JQUERY_PATH || [
    'target/web/web-modules/main/webjars/lib/jquery/jquery.js',
    'target/web/public/test/public/lib/jquery/jquery.js'
].map(p => path.join(root, p)).find(p => fs.existsSync(p));
if (!jqueryPath) {
    console.error('status-check: no jQuery found (sbt compile, or JQUERY_PATH)');
    process.exit(2);
}
const jquery = fs.readFileSync(jqueryPath, 'utf8');
const main = fs.readFileSync(path.join(root, 'app/assets/js/main.js'), 'utf8');

// The rows are the ones the server renders: pinned by views.AdminStatusViewSpec (submit 4242, team 7777, verdict
// @@VERDICT@@), with the rejudge cell (rw) and without (ro).
const pinned = (name) => fs.readFileSync(path.join(root, 'test/resources/admin', name), 'utf8').trim();
const fixtures = {rw: pinned('statusrow-rw.html'), ro: pinned('statusrow-ro.html')};
const rowHtml = (id, verdict, team, variant) =>
    fixtures[variant].split('4242').join(String(id)).split('7777').join(String(team)).split('@@VERDICT@@').join(verdict);
const ev = (id, team, verdict) => ({id, team, rw: rowHtml(id, verdict, team, 'rw'), ro: rowHtml(id, verdict, team, 'ro')});

// A page with the rows `ids` (newest first), the stream opened with the given options. Returns helpers.
function page(ids, opts) {
    const o = Object.assign({canRejudge: true, newest: true, teamFilter: 0}, opts);
    const dom = new JSDOM('<!doctype html><body><div id="newSubmits" style="display: none"></div>' +
        '<table id="submits"><tbody>' + ids.map(i => rowHtml(i, 'WA', 1, 'rw')).join('') + '</tbody></table></body>',
        {runScripts: 'outside-only', url: 'http://localhost/admin/submits/1'});
    const w = dom.window;
    const sources = [];
    w.EventSource = class {
        constructor(p) { this.path = p; this.listeners = {}; this.readyState = 1; sources.push(this); }
        addEventListener(n, f) { this.listeners[n] = f; }
        close() { this.readyState = 2; }
    };
    w.eval(jquery);
    w.eval(main);
    w.listenOnStatus('/events', o.canRejudge, o.newest, o.teamFilter);
    const src = () => sources[sources.length - 1];
    const $ = w.$;
    return {
        w, $, sources,
        open() { src().onopen(); },
        drop() { src().readyState = 1; src().onerror({}); },
        send(name, obj) { src().listeners[name]({data: JSON.stringify(obj)}); },
        // Array.from: jsdom's own arrays are another realm's, which deepStrictEqual does not take for equal.
        ids() { return Array.from($('#submits > tbody > tr').map(function () { return Number(this.id.replace('result-row-', '')); }).get()); },
        verdict(id) { return $('#result-' + id).text().trim(); },
        bar() { return $('#newSubmits'); },
        barShown() { return $('#newSubmits').css('display') !== 'none'; }
    };
}

let n = 0;
const check = (name, f) => { f(); n++; };

check('a submit that is not on the page is held, not put in', () => {
    const p = page([100, 99, 98]);
    p.open(); p.send('ready', {});
    p.send('statusRow', ev(101, 1, 'Running...'));
    assert.deepStrictEqual(p.ids(), [100, 99, 98]);
    assert.ok(p.barShown());
    assert.ok(p.bar().text().includes('1'), p.bar().text());
    p.send('statusRow', ev(102, 2, 'Running...'));
    assert.ok(p.bar().text().includes('2'), p.bar().text());
    assert.deepStrictEqual(p.ids(), [100, 99, 98]);
});

check('a click puts the held rows in, newest first, and clears the bar', () => {
    const p = page([100, 99]);
    p.open();
    p.send('statusRow', ev(101, 1, 'Running...'));
    p.send('statusRow', ev(102, 1, 'Running...'));
    p.bar().click();
    assert.deepStrictEqual(p.ids(), [102, 101, 100, 99]);
    assert.ok(!p.barShown());
    // The next one is held again.
    p.send('statusRow', ev(103, 1, 'Running...'));
    assert.deepStrictEqual(p.ids(), [102, 101, 100, 99]);
    assert.ok(p.barShown());
});

check('a row that is on the page is replaced where it stands, without the bar', () => {
    const p = page([100, 99, 98]);
    p.open();
    p.send('statusRow', ev(99, 1, 'Accepted'));
    assert.deepStrictEqual(p.ids(), [100, 99, 98]);
    assert.strictEqual(p.verdict(99), 'Accepted');
    assert.ok(!p.barShown());
});

check('a held row is updated by a later event, and counted once', () => {
    const p = page([100]);
    p.open();
    p.send('statusRow', ev(101, 1, 'Running...'));
    p.send('statusRow', ev(101, 1, 'Accepted'));
    assert.ok(p.bar().text().includes('1') && !p.bar().text().includes('2'), p.bar().text());
    p.bar().click();
    assert.strictEqual(p.verdict(101), 'Accepted');
    assert.deepStrictEqual(p.ids(), [101, 100]);
});

check('an out-of-order arrival lands where its id says, also after a click', () => {
    const p = page([100, 99]);
    p.open();
    p.send('statusRow', ev(102, 1, 'Running...'));
    p.bar().click();
    p.send('statusRow', ev(101, 1, 'Running...'));
    assert.ok(p.barShown());
    p.bar().click();
    assert.deepStrictEqual(p.ids(), [102, 101, 100, 99]);
});

check('a submit older than the page belongs to another page and is ignored', () => {
    const p = page([100, 99, 98]);
    p.open();
    p.send('statusRow', ev(50, 1, 'Accepted'));
    assert.deepStrictEqual(p.ids(), [100, 99, 98]);
    assert.ok(!p.barShown());
});

check('a page that is not the newest never holds or inserts, but its rows still update', () => {
    const p = page([60, 59, 58], {newest: false});
    p.open();
    p.send('statusRow', ev(101, 1, 'Running...'));
    p.send('statusRow', ev(61, 1, 'Running...'));
    assert.deepStrictEqual(p.ids(), [60, 59, 58]);
    assert.ok(!p.barShown());
    p.send('statusRow', ev(59, 1, 'Accepted'));
    assert.strictEqual(p.verdict(59), 'Accepted');
});

check('with a team filter other teams are ignored and the team link is plain text', () => {
    const p = page([100, 99], {teamFilter: 1});
    p.open();
    p.send('statusRow', ev(101, 2, 'Running...'));
    assert.ok(!p.barShown());
    p.send('statusRow', ev(102, 1, 'Running...'));
    assert.ok(p.barShown());
    p.bar().click();
    assert.deepStrictEqual(p.ids(), [102, 100, 99]);
    assert.strictEqual(p.$('#result-row-102 a.team-link').length, 0);
    assert.ok(p.$('#result-row-102').text().includes('Team 1'));
});

check('without a filter the team link stays a link', () => {
    const p = page([100]);
    p.open();
    p.send('statusRow', ev(101, 3, 'Running...'));
    p.bar().click();
    assert.strictEqual(p.$('#result-row-101 a.team-link').length, 1);
});

check('the rejudge variant is for those who can rejudge, the other for the rest', () => {
    const rw = page([100], {canRejudge: true});
    rw.open(); rw.send('statusRow', ev(101, 1, 'x')); rw.bar().click();
    assert.strictEqual(rw.$('#result-row-101 #rejudge-101').length, 1);
    const ro = page([100], {canRejudge: false});
    ro.open(); ro.send('statusRow', ev(101, 1, 'x')); ro.bar().click();
    assert.strictEqual(ro.$('#result-row-101 #rejudge-101').length, 0);
});

check('an empty page takes the first row', () => {
    const p = page([]);
    p.open();
    p.send('statusRow', ev(5, 1, 'Running...'));
    p.bar().click();
    assert.deepStrictEqual(p.ids(), [5]);
});

check('a connection that dropped and came back asks for a reload; the first connection does not', () => {
    const p = page([100]);
    p.open(); p.send('ready', {});
    assert.ok(!p.barShown());
    p.drop();
    p.open(); p.send('ready', {});
    assert.ok(p.barShown());
    assert.ok(/Обновите/.test(p.bar().text()), p.bar().text());
});

check('a failed first connection is not a drop', () => {
    const p = page([100]);
    p.drop();
    p.open(); p.send('ready', {});
    assert.ok(!p.barShown());
});

console.log(`status-check: OK (${n} checks)`);
