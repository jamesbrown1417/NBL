import json,csv,collections,statistics,math
from pathlib import Path
avg=statistics.mean
def pos(s):
 s=(s or '').upper()
 return 'C' if s in ['C','CEN','CENTER','CENTRE','C/F','FC'] else 'F' if s in ['F','FWD','PF','SF','F/G','FORWARD'] else 'G' if s in ['G','GRD','GUARD','PG','SG','PG/SG'] else 'Unknown'
def calc(rows,team,p,stat='points',weighted=True,minimum=5,keyteam=False):
 groups=collections.defaultdict(list)
 for g in rows:
  if g['position']==p and g['minutes'] is not None and g['minutes']>=minimum and g.get(stat) is not None: groups[(g['player'],g['team']) if keyteam else g['player']].append(g)
 ds=[]
 for player,gs in groups.items():
  a=[g for g in gs if g['opponent']==team];b=[g for g in gs if g['opponent']!=team]
  if a and b:
   rate=lambda r:36*sum(g[stat] for g in r)/sum(g['minutes'] for g in r) if weighted else avg(36*g[stat]/g['minutes'] for g in r)
   ds.append((player,rate(a)-rate(b),len(a),len(b)))
 return ds
r=list(csv.DictReader(open('/tmp/nbl-dvp-r-audit.csv')))
for g in r:
 for k in ['minutes','points','rebounds','assists','threes']:g[k]=float(g[k])
teams=sorted(set(g['opponent'] for g in r));cells=[]
for t in teams:
 for p in sorted(set(g['position'] for g in r)):
  a=calc(r,t,p,weighted=False,keyteam=True);b=calc(r,t,p,keyteam=True)
  if a and b:cells.append({'team':t,'position':p,'original':avg(x[1] for x in a),'weighted':avg(x[1] for x in b),'comparisons':len(a),'single_against':sum(x[2]==1 for x in a)})
print('R SAME-COHORT WEIGHTING:',json.dumps(sorted(cells,key=lambda c:abs(c['weighted']-c['original']),reverse=True)[:5]))
print('R sign changes',sum(c['original']*c['weighted']<0 for c in cells),'/',len(cells),' single matchup comparisons',sum(c['single_against'] for c in cells),'/',sum(c['comparisons'] for c in cells))
d=json.load(open('web/public/data/nbl-stats.json'));w=[dict(g,position=pos(g['position'])) for g in d['playerGames'] if g['season']=='2025-2026'];teams=sorted(set(g['team'] for g in w));cs=[]
for t in teams:
 for p in ['C','F','G']:
  ds=calc(w,t,p);v=avg(x[1] for x in ds) if ds else None;ds15=calc(w,t,p,minimum=15);v15=avg(x[1] for x in ds15) if ds15 else None
  if ds:
   omit=max(((abs(avg(y[1] for y in ds if y!=x)-v),x[0],avg(y[1] for y in ds if y!=x)) for x in ds),default=None) if len(ds)>1 else None
   cs.append(dict(team=t,position=p,dvp=v,min15=v15,players=len(ds),single=sum(x[2]==1 for x in ds),maxLeavePlayer=omit))
print('WEB THRESHOLD:',json.dumps(sorted(cs,key=lambda c:abs(c['dvp']-(c['min15'] or 0)),reverse=True)[:5]));print('WEB single',sum(c['single'] for c in cs),'/',sum(c['players'] for c in cs));print('WEB leave player',json.dumps(sorted(cs,key=lambda c:c['maxLeavePlayer'][0],reverse=True)[:3]));print('WEB unknown',sum(g['position']=='Unknown' and g['minutes'] is not None and g['minutes']>=5 for g in w))
# Chronological diagnostic, no prediction made until the training season has 60 distinct matches.
# Actual target minutes used only to express observed outcomes per 36; not a prop forecast.
history=[];errors=[];dates=sorted(set(g['date'] for g in w));seen=set()
for date in dates:
 targets=[g for g in w if g['date']==date]
 if len(seen)>=60:
  cache={(t,p):calc(history,t,p) for t in teams for p in ['C','F','G']}
  for g in targets:
   prior=[h for h in history if h['player']==g['player'] and h['minutes'] is not None and h['minutes']>=5 and h['points'] is not None]
   if len(prior)<5 or g['minutes'] is None or g['minutes']<5 or g['points'] is None or g['position']=='Unknown':continue
   ds=cache[(g['opponent'],g['position'])]
   if len(ds)<3:continue
   base=36*sum(h['points'] for h in prior)/sum(h['minutes'] for h in prior);adjust=avg(x[1] for x in ds);obs=36*g['points']/g['minutes']
   errors.append((obs-base,obs-base-adjust))
 history.extend(targets);seen.update(g['matchId'] for g in targets)
print('WALK FORWARD conditional points per36',json.dumps({'n':len(errors),'baseline_MAE':avg(abs(a) for a,b in errors),'plus_DVP_MAE':avg(abs(b) for a,b in errors),'baseline_RMSE':math.sqrt(avg(a*a for a,b in errors)),'plus_DVP_RMSE':math.sqrt(avg(b*b for a,b in errors))}))
Path('Reports/audits/dvp-audit-results.json').write_text(json.dumps({'season':'2025-2026','r_weighting_sensitivity':cells,'web_sensitivity':cs,'chronological_diagnostic':{'n':len(errors),'baseline_MAE':avg(abs(a) for a,b in errors),'plus_DVP_MAE':avg(abs(b) for a,b in errors),'baseline_RMSE':math.sqrt(avg(a*a for a,b in errors)),'plus_DVP_RMSE':math.sqrt(avg(b*b for a,b in errors))}},indent=2))
