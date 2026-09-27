import assert from 'node:assert/strict';
import test from 'node:test';
import fs from 'node:fs';
import {prepareDvpRows,calculateDvp,regularizeDvp,buildDvpSnapshot,DVP_VERSION} from '../app/dvp.ts';
function row(player,matchId,date,opponent,points=10,minutes=20,position='C'){
 return {player,team:'Home',matchId,date,season:'2025-2026',opponent,points,minutes,position,pace:80,rebounds:5,assists:2,threes:1,steals:1,blocks:1};
}
function fixture(independent=false){return ['A','B','C','D'].flatMap(p=>Array.from({length:12},(_,i)=>row(p,`${independent?p:''}g${i}`,`2025-10-${String(i+1).padStart(2,'0')}`,i<6?'Target':'Other',i<6?20+i%2:10+i%2)));}
const calculate=(rows,stat='points',basis='per36')=>calculateDvp(prepareDvpRows(rows).rows,'Target','C',stat,basis);
test('identical duplicate games have no influence; conflicts fail explicitly',()=>{
 const rows=fixture(),prep=prepareDvpRows([...rows,rows[0]]);assert.equal(prep.duplicates,1);assert.deepEqual(calculate([...rows,rows[0]]),calculate(rows));
 assert.throws(()=>prepareDvpRows([...rows,{...rows[0],points:99}]),/Conflicting DVP records/);
});
test('fallback positions never use a future, same-date, prior-season or other-team record',()=>{
 const rows=[row('A','1','2025-10-01','Other',10,20,null),row('A','2','2025-10-02','Other',10,20,'CTR'),row('A','3','2025-10-02','Other',10,20,null),row('A','4','2025-10-03','Other',10,20,null),{...row('A','5','2025-10-04','Other',10,20,null),team:'New team'},{...row('A','6','2026-10-01','Other',10,20,null),season:'2026-2027'}];
 const prepared=prepareDvpRows(rows).rows;assert.deepEqual(prepared.map(g=>g.position),['Unknown','C','Unknown','C','Unknown','Unknown']);assert.equal(prepared[3].positionSource,'earlier same-season team appearance');
 assert.equal(prepareDvpRows(rows,'2025-10-02').rows.length,1);
});
test('distinct games, exposure weights and effective players have exact meanings',()=>{
 const d=calculate(fixture());assert.equal(d.games,6);assert.equal(d.appearances,24);assert.equal(d.players,4);assert.equal(d.effectivePlayers,4);assert.equal(d.targetMinutes,480);assert.equal(d.baselineMinutes,480);assert.equal(d.difference,18);assert.equal(d.sufficient,true);assert.ok(d.standardError>0);assert.equal(d.details[0].share,0.25);
});
test('missing stats never become zero and equality of counts does not conceal missingness',()=>{
 const rows=fixture();rows[0].rebounds=null;const d=calculate(rows,'rebounds');assert.equal(d.missingStatAppearances,1);assert.equal(d.difference,0);assert.equal(d.appearances,23);assert.equal(calculate(rows,'pra').appearances,23);
});
test('precise minutes govern eligibility and sparse estimates are withheld',()=>{
 const sparse=[row('A','a','2025-10-01','Target',2,4+59/60),...Array.from({length:6},(_,i)=>row('A',`b${i}`,`2025-11-0${i+1}`,'Other'))];const d=calculate(sparse);assert.equal(d.eligibleTargetAppearances,0);assert.equal(d.difference,null);assert.equal(d.sufficient,false);assert.equal(regularizeDvp([d])[0].regularized,null);
});
test('game-cluster uncertainty recognises observations sharing a match',()=>{
 const shared=calculate(fixture()),independent=calculate(fixture(true));assert.equal(shared.difference,independent.difference);assert.ok(shared.standardError>independent.standardError*1.8);
});
test('per-possession exposure separates an artificial pace effect',()=>{
 const rows=fixture().map(g=>({...g,points:10,pace:g.opponent==='Target'?100:80}));assert.equal(calculate(rows).difference,0);assert.equal(calculate(rows,'points','per100').difference,-5);
 rows[0].pace=null;assert.equal(calculate(rows,'points','per100').missingPaceAppearances,1);
});
test('different team stints do not share player baselines',()=>{
 const rows=fixture().map(g=>g.player==='A'&&g.opponent==='Other'?{...g,team:'New team'}:g);const d=calculate(rows);assert.equal(d.players,3);assert.ok(!d.details.some(d=>d.player==='A'));
});
test('regularization shrinks noisy effects and does not fabricate precision',()=>{
 const base=calculate(fixture());const cells=['X','Y','Z'].map(team=>({...base,team,difference:2,standardError:4}));assert.ok(regularizeDvp(cells).every(c=>c.regularized===0));
 const stronger=regularizeDvp(cells.map(c=>({...c,difference:8,standardError:1})));assert.ok(stronger.every(c=>c.regularized>0&&c.regularized<8));assert.deepEqual(stronger[0].interval,base.interval);
});
test('unsupported stats and mixed seasons cannot produce misleading DVP',()=>{
 assert.throws(()=>calculate(fixture(),'minutes'),/Unsupported/);assert.throws(()=>calculate([...fixture(),{...fixture()[0],matchId:'extra',season:'2026-2027'}]),/one season/);
});
test('historical cutoff excludes all target-day and future observations',()=>{
 const rows=fixture();const a=buildDvpSnapshot(rows,'2025-2026','2025-10-10');const b=buildDvpSnapshot([...rows,row('A','future','2025-10-10','Target',999)],'2025-2026','2025-10-10');assert.deepEqual(a,b);
});
test('web snapshot equals canonical recalculation from its precise exported data',()=>{
 const data=JSON.parse(fs.readFileSync('public/data/nbl-stats.json','utf8'));const season=data.metadata.latestSeasonWithGames;const snapshot=JSON.parse(fs.readFileSync('public'+data.dvp.files[season],'utf8'));assert.equal(snapshot.version,DVP_VERSION);const fresh=buildDvpSnapshot(data.playerGames,season);assert.deepEqual(snapshot.cells,fresh.cells);assert.equal(snapshot.eligible,fresh.eligible);
});
