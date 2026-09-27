import fs from 'node:fs';
import path from 'node:path';
import {createHash} from 'node:crypto';
import {buildDvpSnapshot,prepareDvpRows,DVP_VERSION} from '../web/app/dvp.ts';
const [input,output]=process.argv.slice(2);
if(!input||!output)throw new Error('Usage: node --experimental-strip-types Scripts/export-dvp.mjs INPUT OUTPUT');
const payload=JSON.parse(fs.readFileSync(input,'utf8'));
const files={};
const directory=path.join(path.dirname(output),"dvp");
fs.mkdirSync(directory,{recursive:true});
for(const season of [...new Set(payload.playerGames.map(g=>g.season))].sort()){
 const snapshot=buildDvpSnapshot(payload.playerGames,season);
 const content=JSON.stringify(snapshot), hash=createHash('sha256').update(content).digest('hex').slice(0,20);
 const filename=`${season}-${hash}.json`, target=path.join(directory,filename);
 fs.writeFileSync(target+'.tmp',content);fs.renameSync(target+'.tmp',target);
 files[season]=`/data/dvp/${filename}`;
 process.stderr.write(`DVP ${season}: ${snapshot.eligible} eligible, ${snapshot.unknown} unclassified, ${snapshot.duplicates} duplicates removed\n`);
}
const prepared=prepareDvpRows(payload.playerGames);
// Keep raw position provenance in the main export; DVP snapshots contain causal fallbacks.
const keys=new Set();
payload.playerGames=payload.playerGames.filter(g=>{const key=JSON.stringify([g.season,g.matchId,g.player.normalize('NFKC').trim().replace(/\s+/g,' ').toLowerCase()]);if(keys.has(key))return false;keys.add(key);return true;});
payload.dvp={version:DVP_VERSION,files};
payload.metadata.dvpDuplicatesRemoved=prepared.duplicates;
fs.writeFileSync(output,JSON.stringify(payload));
