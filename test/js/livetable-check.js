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
assert.strictEqual(
  ctx.liveTableHtml(board, labels, 2),
  fixture('livetable.html').replace('<tr data-team="2">', '<tr data-team="2" class="info">')
);

const teamsOnly = {
  problems: [],
  rows: [
    {team: 1, number: 2, name: '<Alpha>', school: 'Lyceum & Co, City', place: 1, total: '0', cells: {}},
    {team: 2, number: 5, name: "Beta's", school: 'School 2, City', place: null, total: '0', cells: {}}
  ]
};
const teamsOnlyHtml = ctx.liveTableHtml(teamsOnly, labels, 1);
assert(teamsOnlyHtml.includes('<tr data-team="1" class="info">'));
assert(teamsOnlyHtml.includes('<tr data-team="2">'));
assert(!teamsOnlyHtml.includes('points-cell'));

// A `row` event omits empty problems: they render as cells without points.
const problems = [{id: 'A', name: 'a', max: '10'}, {id: 'B', name: 'b', max: '20'}];
const row = {team: 2, number: 7, name: 'N', school: 'S, C', place: 3, total: '5', cells: {B: {n: 1, f: false, p: 0, b: '5'}}};
const expectedRow =
  '<tr data-team="2"><th scope="row">3</th><td>7</td><td>N<br/><small>S, C</small></td>' +
  '<td class="text-center"><strong>5</strong></td>' +
  '<td class="text-center points-cell"></td>' +
  '<td class="text-center points-cell">5</td></tr>';
assert.strictEqual(ctx.liveRowHtml(problems, row), expectedRow);
assert.strictEqual(ctx.liveRowHtml(problems, row, 99), expectedRow);
assert.strictEqual(ctx.liveRowHtml(problems, row, 2),
  expectedRow.replace('<tr data-team="2">', '<tr data-team="2" class="info">'));

// Pending-only, CE-only (0), and mixed (0 + ?1) cells.
const edgeRow = {team: 2, number: 7, name: 'N', school: 'S, C', place: 3, total: '0', cells: {
  A: {n: 1, f: false, p: 1},
  B: {n: 1, f: false, p: 0}
}};
assert.strictEqual(ctx.liveRowHtml(problems, edgeRow),
  '<tr data-team="2"><th scope="row">3</th><td>7</td><td>N<br/><small>S, C</small></td>' +
  '<td class="text-center"><strong>0</strong></td>' +
  '<td class="text-center points-cell"><span class="text-warning">?1</span></td>' +
  '<td class="text-center points-cell">0</td></tr>');
// Places in a row event are numbers, null for a team that is not rated.
assert.strictEqual(ctx.liveRankText(null), '');
assert.strictEqual(ctx.liveRankText(4), '4');

// Flash: a changed cell flashes by what it became, an unchanged one not at all; the row when it moved.
const td = (cls, h) => `<td class="text-center points-cell${cls}">${h}</td>`;
const plan = ctx.liveFlashPlan(
  [td('', ''), td('', '3'), td('', ''), td('', '0')],
  [td('', '<span class="text-warning">?1</span>'), td(' full-score', '10'), td('', ''), td('', '0')], true);
assert.deepStrictEqual(Array.from(plan.cells), ['pend', 'ok', null, null]);
assert.strictEqual(ctx.liveCellKind(td('', '0')), 'fail');
assert.strictEqual(ctx.liveCellKind(td('', '')), 'fail');
assert.strictEqual(ctx.liveCellKind(td('', '5')), 'ok');
assert.strictEqual(ctx.liveCellKind(td('', '0<br/><span class="text-warning">?1</span>')), 'pend');
assert.strictEqual(plan.row, true);
assert.strictEqual(ctx.liveFlashPlan(['a'], ['a'], false).row, false);

// Pluralization of problem header points matches TestRows.formatPointsWithUnit.
[
  ['0', '0 баллов'], ['1', '1 балл'], ['2', '2 балла'], ['4', '4 балла'],
  ['5', '5 баллов'], ['11', '11 баллов'], ['14', '14 баллов'],
  ['21', '21 балл'], ['30', '30 баллов'], ['2.5', '2.5 балла']
].forEach(([n, want]) => assert.strictEqual(ctx.livePointsWithUnit(n), want));
const enLabels = {lang: 'en', pointsOne: 'point', pointsFew: 'points', pointsMany: 'points'};
[['0', '0 points'], ['1', '1 point'], ['2', '2 points'], ['21', '21 points'], ['2.5', '2.5 points']]
  .forEach(([n, want]) => assert.strictEqual(ctx.livePointsWithUnit(n, enLabels), want));

console.log('livetable-check: OK');
