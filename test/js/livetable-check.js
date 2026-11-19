// Manual check (not run by sbt): node test/js/livetable-check.js
// The browser renderer in main.js must produce the same markup as livetable.scala.html for the shared fixture.
'use strict';
const fs = require('fs');
const path = require('path');
const vm = require('vm');
const assert = require('assert');

const root = path.join(__dirname, '..', '..');
const ctx = {};
vm.createContext(ctx);
vm.runInContext(fs.readFileSync(path.join(root, 'app/assets/js/main.js'), 'utf8'), ctx);

const fixture = (name) => fs.readFileSync(path.join(root, 'test/resources/live', name), 'utf8').trim();
const board = JSON.parse(fixture('livetable.json'));
const labels = {rank: 'label.rank', team: 'label.team', total: 'label.total'};

assert.strictEqual(ctx.liveTableHtml(board, labels), fixture('livetable.html'));

// A `row` event omits empty problems: they render as cells without points.
const problems = [{id: 'A', name: 'a', max: '10'}, {id: 'B', name: 'b', max: '20'}];
const row = {team: 2, number: 7, name: 'N', school: 'S, C', place: 3, total: '5', cells: {B: {n: 1, f: false, p: 0, b: '5'}}};
assert.strictEqual(ctx.liveRowHtml(problems, row),
  '<tr data-team="2"><th scope="row">3</th><td>7</td><td>N<br/><small>S, C</small></td>' +
  '<td class="text-center points-cell text-muted">—</td>' +
  '<td class="text-center points-cell">5<br/><small class="text-muted">из 20</small></td>' +
  '<td class="text-center"><strong>5</strong></td></tr>');
// Places in a row event are numbers, null for a team that is not rated.
assert.strictEqual(ctx.liveRankText(null), '');
assert.strictEqual(ctx.liveRankText(4), '4');

// Flash: a changed cell flashes by what it became, an unchanged one not at all; the row when it moved.
const td = (cls, h) => `<td class="text-center points-cell${cls}">${h}</td>`;
const plan = ctx.liveFlashPlan(
  [td(' text-muted', '—'), td('', '3<br/><small class="text-muted">из 10</small>'), td(' text-muted', '—')],
  [td(' text-muted', '—<br/><span class="text-warning">?1</span>'), td(' full-score', '10<br/><small class="text-muted">из 10</small>'), td(' text-muted', '—')], true);
assert.deepStrictEqual(Array.from(plan.cells), ['pend', 'ok', null]);
assert.strictEqual(plan.row, true);
assert.strictEqual(ctx.liveFlashPlan(['a'], ['a'], false).row, false);

console.log('livetable-check: OK');
