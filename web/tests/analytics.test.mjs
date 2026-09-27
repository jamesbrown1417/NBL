import assert from 'node:assert/strict';
import test from 'node:test';
import {distribution,rate,value} from '../app/analytics.ts';
import {position} from '../app/dvp.ts';
test('combinations preserve missing data and line equality is separate',()=>{assert.equal(value({points:10,rebounds:null},'pr'),null);assert.deepEqual(distribution([{points:10},{points:15},{points:20},{points:null}],'points',15),{n:3,mean:15,median:15,sd:Math.sqrt(50/3),over:1,under:1,equal:1});});
test('recorded position variants map consistently',()=>{for(const p of ['CEN','CENTRE','C/F','FC'])assert.equal(position(p),'C');for(const p of ['FWD','PF','F/G'])assert.equal(position(p),'F');for(const p of ['GRD','PG/SG','GUARD'])assert.equal(position(p),'G');assert.equal(position(null),'Unknown');});
test('efficiency weights shot attempts rather than averaging percentages',()=>{const rows=[{points:2,fgm:1,fga:1,threes:0,fta:0},{points:2,fgm:1,fga:9,threes:0,fta:0}];assert.equal(rate(rows,'efg'),20);assert.equal(rate(rows,'ts'),20);assert.equal(rate([],'efg'),null);});
