"use client";
import { DvpPanel } from "./DvpPanel";
import { prepareDvpRows } from "./dvp";
import { useMemo, useState } from 'react';
import { average, display, distribution, metrics, numeric, rate, ratio, sum, value, type Box, type Dataset, type Player, type Team } from './analytics';
function Select({ label, value, onChange, options }: {
    label: string;
    value: string;
    onChange: (s: string) => void;
    options: string[];
}) { return <label className="field"><span>{label}</span><select value={value} onChange={e => onChange(e.target.value)}>{options.map(o => <option key={o} value={o}>{metrics[o] ?? o}</option>)}</select></label>; }
function Table({ headers, rows }: {
    headers: string[];
    rows: (string | number | React.ReactElement)[][];
}) { return <div className="table-wrap"><table><thead><tr>{headers.map((h, i) => <th key={i}>{h}</th>)}</tr></thead><tbody>{rows.map((r, i) => <tr key={i}>{r.map((v, j) => <td key={j}>{v}</td>)}</tr>)}</tbody></table>{!rows.length && <p className="research-note">No qualifying data for this selection.</p>}</div>; }
function Panel({ title, children }: {
    title: string;
    children: React.ReactNode;
}) { return <section className="panel data-panel"><div className="section-heading"><h2>{title}</h2></div>{children}</section>; }
export function PlayerDepth({ rows, data }: {
    rows: Player[];
    data: Dataset;
}) { const windows = [rows, rows.slice(0, 5), rows.slice(0, 10)]; return <Panel title="Opportunity, efficiency & consistency"><p className="research-note">Current filters apply. Recent windows use newest games. Percentages use combined attempts; — means unavailable. Full-game stats include overtime.</p><Table headers={['Metric', 'Selected sample', 'Last 5', 'Last 10']} rows={[...['minutes', 'fga', 'fgm', 'threeAttempts', 'threes', 'fta', 'ftm', 'offensiveRebounds', 'defensiveRebounds', 'fouls', 'foulsDrawn', 'paintPoints', 'fastBreakPoints', 'secondChancePoints'].map(k => [metrics[k] ?? k.toUpperCase(), ...windows.map(w => display(average(w.map(g => g[k]))))]), ...['fgPct', 'threePct', 'ftPct', 'twoAttempts', 'twoMade'].map(k => [({ fgPct: 'FG %', threePct: '3P %', ftPct: 'FT %', twoAttempts: '2PA', twoMade: '2PM' })[k]!, ...windows.map(w => display(shootingMetric(w, k)))]), ...['usage', 'astRate', 'orbRate', 'drbRate'].map(k => [({ usage: 'Estimated usage %', astRate: 'Estimated assist %', orbRate: 'Offensive rebound %', drbRate: 'Defensive rebound %' })[k]!, ...windows.map(w => display(playerAdvanced(w, data, k)))]), ...['efg', 'ts', '3par', 'ftr'].map(k => [({ efg: 'Effective FG %', ts: 'True shooting %', '3par': '3PA / FGA %', ftr: 'FTA / FGA %' })[k]!, ...windows.map(w => display(rate(w, k)))]), ...['points', 'rebounds', 'assists', 'threes'].flatMap(k => [[`${metrics[k]} median`, ...windows.map(w => display(distribution(w, k, 0).median))], [`${metrics[k]} standard deviation`, ...windows.map(w => display(distribution(w, k, 0).sd))], [`${metrics[k]} per 36 min`, ...windows.map(w => { const valid = w.filter(g => numeric(g.minutes) && g.minutes > 0 && value(g, k) !== null); return display(ratio(sum(valid.map(g => value(g, k))), sum(valid.map(g => g.minutes)), 36)); })]])]}/></Panel>; }
export function TeamDepth({ rows, data }: {
    rows: Team[];
    data: Dataset;
}) { const opponents = rows.flatMap(g => data.teamGames.filter(o => o.matchId === g.matchId && o.team === g.opponent)); const rating = (r: Team[]) => { const good = r.filter(g => numeric(g.possessions) && g.possessions > 0 && numeric(g.points)); return ratio(sum(good.map(g => g.points)), sum(good.map(g => g.possessions))); }; return <Panel title="Offence & opponent production"><p className="research-note">Opponent production uses the opposing box score from each selected game. Ratings use source possessions. Quarter scores exclude overtime; full-game totals include it.</p><Table headers={['Per game unless marked', 'For', 'Against']} rows={[...['points', 'fga', 'fgm', 'threeAttempts', 'threes', 'fta', 'ftm', 'offensiveRebounds', 'defensiveRebounds', 'assists', 'turnovers', 'steals', 'blocks', 'fouls', 'paintPoints', 'fastBreakPoints', 'secondChancePoints', 'benchPoints', 'q1', 'q2', 'q3', 'q4'].map(k => [metrics[k] ?? ({ fgm: 'FGM', ftm: 'FTM', benchPoints: 'Bench points', q1: 'Quarter 1', q2: 'Quarter 2', q3: 'Quarter 3', q4: 'Quarter 4' } as Record<string, string>)[k] ?? k, ...[rows, opponents].map(w => display(average(w.map(g => g[k]))))]), ...['fgPct', 'threePct', 'ftPct', 'twoAttempts', 'twoMade'].map(k => [({ fgPct: 'FG %', threePct: '3P %', ftPct: 'FT %', twoAttempts: '2PA', twoMade: '2PM' })[k]!, ...[rows, opponents].map(w => display(shootingMetric(w, k)))]), ['First half', ...[rows, opponents].map(w => display(average(w.map(g => sum([g.q1, g.q2])))))], ['Second half', ...[rows, opponents].map(w => display(average(w.map(g => sum([g.q3, g.q4])))))], ...['efg', 'ts', '3par', 'ftr'].map(k => [({ efg: 'Effective FG %', ts: 'True shooting %', '3par': '3PA / FGA %', ftr: 'FTA / FGA %' })[k]!, ...[rows, opponents].map(w => display(rate(w, k)))]), ['Points / 100 possessions', display(rating(rows)), display(rating(opponents))], ['Net rating', rating(rows) !== null && rating(opponents) !== null ? display(rating(rows)! - rating(opponents)!) : '—', '—'], ['Offensive rebound %', display(reboundRate(rows, opponents)), display(reboundRate(opponents, rows))], ['Turnovers / 100 possessions', ...[rows, opponents].map(w => { const valid = w.filter(g => numeric(g.possessions) && numeric(g.turnovers)); return display(ratio(sum(valid.map(g => g.turnovers)), sum(valid.map(g => g.possessions)))); })]]}/></Panel>; }
export function Research({ data, mode }: {
    data: Dataset;
    mode: 'defence' | 'shooting' | 'matchup';
}) {
    const [season, setSeason] = useState(data.metadata.latestSeasonWithGames), [stat, setStat] = useState('points'), [pos, setPos] = useState('C'), [selectedTeam, setTeam] = useState(''), [selectedPlayer, setPlayer] = useState(''), [sample, setSample] = useState('All games'), [venue, setVenue] = useState('All'), [role, setRole] = useState('All'), [line, setLine] = useState('15.5');
    const pg = data.playerGames.filter(g => g.season === season), tg = data.teamGames.filter(g => g.season === season), teams = [...new Set(tg.map(g => g.team))].sort(), players = [...new Set(pg.map(g => g.player))].sort(), team = teams.includes(selectedTeam) ? selectedTeam : teams[0] ?? '', player = players.includes(selectedPlayer) ? selectedPlayer : players[0] ?? '';
    const limit = sample === 'Last 5' ? 5 : sample === 'Last 10' ? 10 : Infinity;
    const seasonRows = pg.filter(g => g.player === player).sort((a, b) => b.date.localeCompare(a.date)), rows = seasonRows.filter(g => (venue === 'All' || g.homeAway === venue) && (role === 'All' || g.starter === (role === 'Starter'))).slice(0, limit);
    const positionRows = useMemo(() => prepareDvpRows(data.playerGames.filter(g => g.season === season)).rows, [data.playerGames, season]);
    const currentPosition = (name: string) => [...positionRows].reverse().find(g => g.player === name)?.position ?? "Unknown";
    const effectivePos = selectedPlayer === player ? pos : currentPosition(player);
    const result = distribution(rows, stat, Number(line));
    const allowed = (t: string) => { const games = tg.filter(g => g.opponent === t).sort((a, b) => b.date.localeCompare(a.date)).slice(0, limit); return { team: t, games: games.length, attempts: average(games.map(g => g.threeAttempts)), made: average(games.map(g => g.threes)), pct: ratio(sum(games.map(g => g.threes)), sum(games.map(g => g.threeAttempts))), share: rate(games, '3par'), pace: average(games.map(g => g.pace)) }; };
    const shooting = teams.map(allowed).sort((a, b) => (b.attempts ?? -1) - (a.attempts ?? -1));
    return <><div className="page-title-row"><div><p className="eyebrow">Matchup research</p><h1>{mode === 'defence' ? 'Defence by position' : mode === 'shooting' ? 'Three-point opportunities' : 'Matchup workspace'}</h1></div><Select label="Season" value={season} onChange={next => { setSeason(next); setPlayer(''); setPos('C'); }} options={[...new Set(tg.map(g => g.season)), ...data.metadata.seasons.filter(s => s !== season && data.teamGames.some(g => g.season === s))]}/></div><section className="panel control-panel">{mode !== 'shooting' && <><Select label="Stat" value={stat} onChange={setStat} options={mode === 'defence' ? ['points', 'rebounds', 'assists', 'threes', 'steals', 'blocks', 'pra'] : Object.keys(metrics)}/><Select label="Position group" value={mode === "matchup" ? effectivePos : pos} onChange={p => { setPos(p); setPlayer(player); }} options={['C', 'F', 'G', 'Unknown']}/><Select label="Opponent" value={team} onChange={setTeam} options={teams}/></>}{mode !== 'defence' && <Select label="Sample" value={sample} onChange={setSample} options={['All games', 'Last 5', 'Last 10']}/>} {mode === 'matchup' && <><Select label="Player" value={player} onChange={p => { setPlayer(p); setPos(currentPosition(p)); }} options={players}/><Select label="Venue" value={venue} onChange={setVenue} options={['All', 'home', 'away']}/><Select label="Role" value={role} onChange={setRole} options={['All', 'Starter', 'Bench']}/><label className="field"><span>Reference line</span><input type="number" step="0.5" value={line} onChange={e => setLine(e.target.value)}/></label></>}</section>
    {mode === 'defence' && <DvpPanel data={data} season={season} team={team} pos={pos} stat={stat}/>}
    {mode === 'shooting' && <Panel title="Opponent three-point production · ranked by attempts allowed"><p className="research-note">Volume and results, not openness or shot quality. Defender distance, contest level and shot-location data are unavailable. Recent windows are per defending team. Season league baseline: {display(average(tg.map(g => g.threeAttempts)))} 3PA and {display(average(tg.map(g => g.threes)))} 3PM per team game.</p><Table headers={['Rank', 'Defending team', 'Games', '3PA allowed', '3PM allowed', 'Opponent 3P %', '3PA / FGA %', 'Pace']} rows={shooting.map((r, i) => [i + 1, r.team, r.games, display(r.attempts), display(r.made), display(r.pct), display(r.share), display(r.pace)])}/></Panel>}
    {mode === 'matchup' && <><Panel title={`${player} · ${metrics[stat]} distribution`}><p className="research-note">{result.n} valid appearances. Historical frequencies are not predictive probabilities. Equal results are shown separately; settlement depends on market rules. Full-game totals include overtime.</p><Table headers={['Average', 'Median', 'Std deviation', 'Over', 'Under', 'Equal']} rows={[[display(result.mean), display(result.median), display(result.sd), line === '' ? '—' : `${result.over} / ${result.n}`, line === '' ? '—' : `${result.under} / ${result.n}`, line === '' ? '—' : `${result.equal} / ${result.n}`]]}/></Panel><Panel title={`${team} matchup context`}><Table headers={['Measure', 'Value']} rows={[['Opponent 3PA allowed', display(allowed(team).attempts)], ['Opponent pace', display(allowed(team).pace)], ['Selected-sample minutes', display(average(rows.map(g => g.minutes)))], ['Head-to-head appearances in sample', rows.filter(g => g.opponent === team).length], ['Head-to-head average', display(average(rows.filter(g => g.opponent === team).map(g => value(g, stat))))]]}/><p className="research-note">DVP uses the full selected season and the chosen position group. Player filters apply to distribution and head-to-head only. Availability, with/without lineups and shot quality are not modelled.</p></Panel><DvpPanel data={data} season={season} team={team} pos={effectivePos} stat={stat} compact/><PlayerDepth rows={rows} data={data}/><Panel title="Selected game distribution"><Table headers={['Date', 'Opponent', 'Minutes', metrics[stat], 'Line result']} rows={rows.map(g => { const v = value(g, stat); return [g.date, g.opponent, display(g.minutes), display(v), v === null || line === '' ? '—' : v > Number(line) ? 'OVER' : v < Number(line) ? 'UNDER' : 'EQUAL']; })}/></Panel></>}
    </>;
}
function reboundRate(rows: Team[], other: Team[]) { const pairs = rows.flatMap(g => { const o = other.find(x => x.matchId === g.matchId); return o && numeric(g.offensiveRebounds) && numeric(o.defensiveRebounds) ? [{ o: g.offensiveRebounds, d: o.defensiveRebounds }] : []; }); return ratio(sum(pairs.map(g => g.o)), sum(pairs.map(g => g.o + g.d))); }
function playerAdvanced(rows: Player[], data: Dataset, kind: string) {
    return average(rows.map(g => {
        const t = data.teamGames.find(t => t.matchId === g.matchId && t.team === g.team), o = data.teamGames.find(t => t.matchId === g.matchId && t.team === g.opponent);
        const teamMinutes = sum(data.playerGames.filter(p => p.matchId === g.matchId && p.team === g.team).map(p => p.minutes));
        if (!t || !o || !teamMinutes || !numeric(g.minutes) || g.minutes <= 0)
            return null;
        const share = g.minutes / (teamMinutes / 5);
        const n = (b: Box, k: string) => numeric(b[k]) ? b[k] as number : null;
        if (kind === 'usage') {
            const player = sum([n(g, 'fga'), numeric(g.fta) ? 0.44 * g.fta : null, n(g, 'turnovers')]), team = sum([n(t, 'fga'), numeric(t.fta) ? 0.44 * t.fta : null, n(t, 'turnovers')]);
            return ratio(player, team === null ? null : team * share);
        }
        if (kind === 'astRate')
            return ratio(n(g, 'assists'), numeric(t.fgm) && numeric(g.fgm) ? share * t.fgm - g.fgm : null);
        const denom = kind === 'orbRate' ? sum([t.offensiveRebounds, o.defensiveRebounds]) : sum([t.defensiveRebounds, o.offensiveRebounds]);
        return ratio(n(g, kind === 'orbRate' ? 'offensiveRebounds' : 'defensiveRebounds'), denom === null ? null : share * denom);
    }));
}
function shootingMetric(rows: Box[], kind: string) {
    if (kind === 'twoAttempts' || kind === 'twoMade') {
        const a = kind === 'twoAttempts' ? 'fga' : 'fgm', b = kind === 'twoAttempts' ? 'threeAttempts' : 'threes';
        return average(rows.map(g => numeric(g[a]) && numeric(g[b]) ? (g[a] as number) - (g[b] as number) : null));
    }
    const [a, b] = kind === 'fgPct' ? ['fgm', 'fga'] : kind === 'threePct' ? ['threes', 'threeAttempts'] : ['ftm', 'fta'];
    const valid = rows.filter(g => numeric(g[a]) && numeric(g[b]));
    return ratio(sum(valid.map(g => g[a])), sum(valid.map(g => g[b])));
}
