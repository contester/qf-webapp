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
const labels = {rank: 'label.rank', team: 'label.team', penalty: 'label.penalty'};

assert.strictEqual(ctx.liveTableHtml(board, labels, 1), fixture('livetable.html'));

// A `row` event omits empty problems: they render as empty cells.
const row = {team: 2, school: 'S', name: 'N', rank: '3', solved: 0, penalty: 0, cells: {B: {t: 'fail', s: '-1'}}};
assert.strictEqual(ctx.liveRowHtml(['A', 'B'], row, 0),
  '<tr data-team="2"><th scope="row">3</th><td>2</td><td>S<br/><small>N</small></td><td>0</td><td>0</td>' +
  '<td class="text-center"></td><td class="text-center"><span class="text-muted">-1</span></td></tr>');
// `ranks` in a row event are numbers, 0 for not rated.
assert.strictEqual(ctx.liveRankText(0), '*');
assert.strictEqual(ctx.liveRankText(4), '4');

// Flash: a changed cell flashes by what it became, an unchanged one not at all; the row when it moved.
const cell = (cls, t) => `<span class="${cls}">${t}</span>`;
const plan = ctx.liveFlashPlan(
  ['', cell('text-warning', '?2'), cell('text-muted', '-1'), cell('text-success', '+')],
  [cell('text-muted', '-1'), cell('text-success', '+1'), cell('text-muted', '-1'), ''], true);
assert.deepStrictEqual(Array.from(plan.cells), ['fail', 'ok', null, null]);
assert.strictEqual(plan.row, true);
assert.strictEqual(ctx.liveFlashPlan(['a'], ['a'], false).row, false);

console.log('livetable-check: OK');
