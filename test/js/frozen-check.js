// Manual check (node test/js/frozen-check.js): the frozen line follows the clock. Drive updateFrozen via publicContestTimer with fake jQuery/Date.
const fs=require('fs');
let src=fs.readFileSync(require('path').join(__dirname, '../../app/assets/js/main.js') + '','utf8');
let shown=null, unf=null, counter=null, now=1000000, ints=[];
global.$=function(sel){return {show(){if(sel==='#contest-frozen')shown=true;if(sel==='#contest-unfreeze')unf=true},hide(){if(sel==='#contest-frozen')shown=false;if(sel==='#contest-unfreeze')unf=false},text(t){if(sel==='#contest-unfreeze-counter')counter=t}}};
global.Date=class extends Date{constructor(...a){if(a.length)super(...a);else super(now)} static now(){return now}};
global.setInterval=(f)=>{ints.push(f);return ints.length};global.clearInterval=()=>{};
global.window={};global.Notification={permission:'granted'};global.document={documentElement:{classList:{toggle(){}}}};global.location={reload(){}};
eval(src+';global.t=publicContestTimer;');
t(-3600000, 7200000, 3600000, 10800000); // started, ends in 2h, freeze in 1h, expose in 3h
const assert=require('assert');
assert.strictEqual(shown,false);
now+=3700000; ints.forEach(f=>f()); assert.strictEqual(shown,true); assert.strictEqual(unf,true); assert.strictEqual(counter,'01:58:20'); // expose at 3h, now 1h01m40s
now+=7200000; ints.forEach(f=>f()); assert.strictEqual(shown,false); assert.strictEqual(unf,false);
console.log('frozen-check: OK');
