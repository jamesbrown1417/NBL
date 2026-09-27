// Chronological conditional-rate diagnostic. Never changes production estimates or gates.
import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {prepareDvpRows,calculateDvp,regularizeDvp,DVP_STATS,DVP_VERSION,statValue,DVP_RULES} from '../web/app/dvp.ts';
const root=path.resolve(path.dirname(fileURLToPath(import.meta.url)),'..');
const payload=JSON.parse(fs.readFileSync(path.join(root,'web/public/data/nbl-stats.json'),'utf8'));
const seasons=process.argv.slice(2).length?process.argv.slice(2):['2023-2024','2024-2025','2025-2026'];
const reports=[];
for(const season of seasons){
 const seasonRows=payload.playerGames.filter(g=>g.season===season);
 if(!seasonRows.length)throw new Error(`No data for ${season}`);
 const dates=[...new Set(seasonRows.map(g=>g.date))].sort(),teams=[...new Set(seasonRows.map(g=>g.team))].sort();
 const totals=Object.fromEntries(DVP_STATS.map(s=>[s,{n:0,models:Object.fromEntries(['baseline','raw','regularized','calibrated'].map(m=>[m,{absolute:0,squared:0}])),xy:0,xx:0,eligible:0,skippedNoEstimate:0}]));
 for(const date of dates){
  const history=prepareDvpRows(seasonRows,date).rows;
  if(new Set(history.map(g=>g.matchId)).size<60)continue;
  const cells=new Map();
  for(const stat of DVP_STATS)for(const pos of ['C','F','G'])for(const cell of regularizeDvp(teams.map(t=>calculateDvp(history,t,pos,stat))))cells.set(`${stat}|${pos}|${cell.team}`,cell);
  const updates=[];
  for(const target of seasonRows.filter(g=>g.date===date&&g.minutes!==null&&g.minutes>=DVP_RULES.minMinutes)){
   const stint=history.filter(g=>g.player===target.player&&g.team===target.team&&g.minutes!==null&&g.minutes>=DVP_RULES.minMinutes);
   // Position is taken from the latest previously recorded game, not today's box score.
   const pos=stint.at(-1)?.position;
   if(!pos||pos==='Unknown')continue;
   for(const stat of DVP_STATS){
    const observed=statValue(target,stat);if(observed===null)continue;
    const prior=stint.filter(g=>g.position===pos&&g.opponent!==target.opponent&&statValue(g,stat)!==null);
    if(prior.length<5)continue;
    const tally=totals[stat];tally.eligible++;
    const cell=cells.get(`${stat}|${pos}|${target.opponent}`);
    if(!cell?.sufficient||cell.regularized===null){tally.skippedNoEstimate++;continue;}
    const baseline=36*prior.reduce((n,g)=>n+statValue(g,stat),0)/prior.reduce((n,g)=>n+g.minutes,0);
    const y=36*observed/target.minutes;
    // Optional coefficient uses only scored observations from earlier dates.
    const beta=tally.xx>0?Math.max(0,Math.min(1,tally.xy/tally.xx)):0;
    const predictions={baseline,raw:baseline+cell.difference,regularized:baseline+cell.regularized,calibrated:baseline+beta*cell.regularized};
    tally.n++;
    for(const [model,prediction] of Object.entries(predictions)){const error=y-prediction;tally.models[model].absolute+=Math.abs(error);tally.models[model].squared+=error*error;}
    updates.push({tally,x:cell.regularized,y:y-baseline});
   }
  }
  for(const {tally,x,y} of updates){tally.xy+=x*y;tally.xx+=x*x;}
 }
 const stats=Object.fromEntries(Object.entries(totals).map(([stat,t])=>[stat,{n:t.n,eligible:t.eligible,skippedNoEstimate:t.skippedNoEstimate,finalCoefficient:t.xx?Math.max(0,Math.min(1,t.xy/t.xx)):0,models:Object.fromEntries(Object.entries(t.models).map(([m,e])=>[m,{mae:t.n?e.absolute/t.n:null,rmse:t.n?Math.sqrt(e.squared/t.n):null}]))}]));
 reports.push({season,stats});process.stdout.write(JSON.stringify({season,stats})+'\n');
}
const output={version:DVP_VERSION,rules:DVP_RULES,method:'Date-cutoff, same-team/position other-opponent pooled player baseline. Begin after 60 training matches. Seven stats. Rates per36 conditioned on observed target minutes >=5. Same observations for all four models. Prior position only. Shrinkage uses training cells only. Calibrated coefficient fit on earlier scored dates within each season; constrained to [0,1].',limitations:['Conditional-rate diagnostic, not a pregame prop forecast or profitability test.','No significance testing or prediction interval calibration.','Position and minutes availability gates select a subset of appearances.','Current historical source revisions are used; point-in-time source revision archives are unavailable.','Thresholds are provisional operational gates, not tuned or certified by this report.','Results do not activate a projection adjustment.'],reports};
const destination=path.join(root,'Reports/audits/dvp-v2-validation.json');fs.writeFileSync(destination,JSON.stringify(output,null,2));
