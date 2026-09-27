"use client";
import { useEffect, useState } from 'react';
import { DVP_STATS, DVP_VERSION, type DvpBasis, type DvpCell, type DvpSnapshot } from './dvp';
import { display, metrics, type Dataset } from './analytics';
const interval = (cell: DvpCell) => cell.interval ? `${display(cell.interval[0])} to ${display(cell.interval[1])}` : 'Unavailable';
const evidence = (cell: DvpCell) => !cell.sufficient ? 'Insufficient evidence' : cell.regularized === null ? 'Pooling unavailable' : cell.interval && cell.interval[0] <= 0 && cell.interval[1] >= 0 ? 'Direction uncertain' : 'Descriptive signal';
export function DvpPanel({ data, season, team, pos, stat, compact = false }: {
    data: Dataset;
    season: string;
    team: string;
    pos: string;
    stat: string;
    compact?: boolean;
}) {
    const url = data.dvp?.files[season];
    const [loaded, setLoaded] = useState<{
        url: string;
        snapshot?: DvpSnapshot;
        error?: string;
    }>();
    const [retry, setRetry] = useState(0);
    useEffect(() => {
        if (!url)
            return;
        const controller = new AbortController();
        fetch(url, { signal: controller.signal }).then(r => { if (!r.ok)
            throw new Error('Matchup evidence could not be loaded'); return r.json(); }).then((snapshot: DvpSnapshot) => {
            if (snapshot.version !== DVP_VERSION || snapshot.season !== season || !Array.isArray(snapshot.cells))
                throw new Error('Matchup evidence is incompatible with this app');
            setLoaded({ url, snapshot });
        }).catch(e => { if (!controller.signal.aborted)
            setLoaded({ url, error: e.message }); });
        return () => controller.abort();
    }, [url, season, retry]);
    if (!DVP_STATS.includes(stat as typeof DVP_STATS[number]))
        return <section className="panel research-note">Positional DVP is available for points, rebounds, assists, made threes, steals, blocks and PRA. It is not defined for {metrics[stat]?.toLowerCase() ?? stat}.</section>;
    if (!url)
        return <section className="panel research-note">No positional evidence is available for this season.</section>;
    if (loaded?.url !== url)
        return <section className="panel research-note" role="status">Loading positional evidence…</section>;
    if (loaded.error || !loaded.snapshot)
        return <section className="panel research-note" role="alert">{loaded.error ?? 'Matchup evidence is unavailable'}. <button onClick={() => { setLoaded(undefined); setRetry(n => n + 1); }}>Retry</button></section>;
    return <DvpSnapshotPanel snapshot={loaded.snapshot} team={team} pos={pos} stat={stat} compact={compact}/>;
}
export function DvpSnapshotPanel({ snapshot, team, pos, stat, compact = false }: {
    snapshot: DvpSnapshot;
    team: string;
    pos: string;
    stat: string;
    compact?: boolean;
}) {
    const [basis, setBasis] = useState<DvpBasis>('per36');
    const [estimate, setEstimate] = useState('Regularized');
    const cells = snapshot.cells.filter(c => c.stat === stat && c.basis === basis);
    const selected = cells.find(c => c.team === team && c.position === pos);
    const units = basis === 'per36' ? 'per 36 minutes' : 'per 100 estimated possessions';
    const teams = [...new Set(cells.map(c => c.team))].sort();
    const value = (cell: DvpCell) => estimate === 'Regularized' ? cell.regularized : cell.difference;
    return <section className="panel data-panel">
        <div className="section-heading"><div><h2>{compact ? `${team} · ${pos} matchup evidence` : `${metrics[stat]} · defence by position`}</h2><p>{units} above or below the same players’ other-opponent baseline</p></div></div>
        <div className="control-panel">
            <label className="field"><span>Measurement</span><select value={basis} onChange={e => setBasis(e.target.value as DvpBasis)}><option value="per36">Production per 36 minutes</option><option value="per100">Production per 100 estimated possessions</option></select></label>
            {!compact && <label className="field"><span>Estimate</span><select value={estimate} onChange={e => setEstimate(e.target.value)}><option>Regularized</option><option>Raw</option></select></label>}
        </div>
        <p className="research-note">Position coverage: {snapshot.eligible - snapshot.unknown} / {snapshot.eligible} eligible appearances. {snapshot.unknown} unclassified; {snapshot.inherited} filled from an earlier appearance with the same team that season. {snapshot.duplicates} duplicate records removed.</p>
        {!compact && <div className="table-wrap"><table><caption className="research-note">Positive means more production allowed. Muted cells have insufficient evidence or an interval spanning zero; colour is not a betting recommendation.</caption><thead><tr><th>Team</th><th>Centres</th><th>Forwards</th><th>Guards</th></tr></thead><tbody>{teams.map(t => <tr key={t}><td>{t}</td>{['C', 'F', 'G'].map(p => { const cell = cells.find(c => c.team === t && c.position === p)!; const directional = cell.sufficient && cell.interval && (cell.interval[0] > 0 || cell.interval[1] < 0) && value(cell) !== null && value(cell) !== 0; return <td key={p}><span className={`heat-cell ${directional ? value(cell)! > 0 ? 'permissive' : 'restrictive' : 'uncertain'}`} title={cell.reasons.join('; ')}>{display(value(cell))}<small>{evidence(cell)}</small><small>{cell.players} players · {cell.games} target games</small><small>Raw 95% interval: {interval(cell)}</small></span></td>; })}</tr>)}</tbody></table></div>}
        {selected ? <>
            <div className="section-heading"><h2>{team} vs {pos} · evidence and contributors</h2></div>
            <div className="table-wrap"><table><thead><tr><th>Measure</th><th>Value</th></tr></thead><tbody>
                <tr><td>Raw difference</td><td>{display(selected.difference)}</td></tr>
                <tr><td>Regularized difference</td><td>{display(selected.regularized)}</td></tr>
                <tr><td>Approximate 95% interval for raw difference</td><td>{interval(selected)}</td></tr>
                <tr><td>Evidence</td><td>{evidence(selected)}</td></tr>
                <tr><td>Raw effect retained after shrinkage</td><td>{selected.shrinkage === null ? '—' : `${display(selected.shrinkage * 100, 0)}%`}</td></tr>
                <tr><td>Players / effective players</td><td>{selected.players} / {display(selected.effectivePlayers)}</td></tr>
                <tr><td>Distinct target / baseline games</td><td>{selected.games} / {selected.baselineGames}</td></tr>
                <tr><td>Target appearances</td><td>{selected.appearances}</td></tr>
                <tr><td>Target / baseline minutes</td><td>{display(selected.targetMinutes, 0)} / {display(selected.baselineMinutes, 0)}</td></tr>
                <tr><td>Player-team comparisons excluded for low exposure</td><td>{selected.excludedPlayers}</td></tr>
                <tr><td>Target group appearances missing this stat</td><td>{selected.missingStatAppearances}</td></tr>
                {basis === 'per100' && <><tr><td>Target group appearances missing pace</td><td>{selected.missingPaceAppearances}</td></tr><tr><td>Target group appearances using estimated team pace</td><td>{selected.estimatedPaceAppearances}</td></tr></>}
            </tbody></table></div>
            {!selected.sufficient && <p className="research-note">{selected.reasons.join('. ')}. The regularized estimate is withheld.</p>}
            {!compact && <div className="table-wrap"><table><thead><tr><th>Player / team stint</th><th>Target / baseline games</th><th>Target / baseline minutes</th><th>Vs team</th><th>Vs others</th><th>Difference</th><th>Weight</th></tr></thead><tbody>{selected.details.map(d => <tr key={`${d.player}-${d.team}`}><td>{d.player}<small>{d.team}</small></td><td>{d.games} / {d.baselineGames}</td><td>{display(d.targetMinutes, 0)} / {display(d.baselineMinutes, 0)}</td><td>{display(d.against)}</td><td>{display(d.baseline)}</td><td>{display(d.difference)}</td><td>{display(d.share * 100, 0)}%</td></tr>)}</tbody></table></div>}
        </> : <p className="research-note">No supported position assignment is available. Choose C, F or G to inspect that group; unknown positions are never assigned to centres automatically.</p>}
        <details className="research-method"><summary>Method and limitations</summary><p>Each player-team stint needs at least 2 target games, 5 other-opponent games, 20 target minutes and 60 baseline minutes. Appearances under 5 minutes are excluded using precise minutes. Differences are weighted by exposure on both sides. The group needs 5 target games, 3 effective players, 120 target minutes and 360 baseline minutes. These are provisional evidence gates, not guarantees of prediction quality.</p><p>Regularization pulls noisy estimates toward zero, using the selected season’s variation and estimated uncertainty across teams. Intervals describe the raw estimate and use approximate game-cluster uncertainty, including target and baseline observations. They are not calibrated prediction intervals and do not fully model dependence across games from the same players.</p><p>{basis === 'per36' ? 'Per-minute production includes the influence of pace.' : 'Estimated player possessions use minutes × team pace / 40. Where source pace is missing, team pace is estimated from both box scores using FGA − offensive rebounds + turnovers + 0.44 × FTA, adjusted for overtime. This is not measured on-court exposure.'} Venue, role, teammate availability and shot quality remain unadjusted. Season-wide DVP does not change with the player sample filters. This descriptive estimate is not added to projections or fair odds.</p><p>Positions come from recorded box scores or earlier appearances in the same season and team. C/F and FC map to C; F/G maps to F. Player names are provisional identities because stable source player IDs are unavailable. Estimator {snapshot.version}.</p></details>
    </section>;
}
