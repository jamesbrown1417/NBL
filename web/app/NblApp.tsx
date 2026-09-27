"use client";
import { useEffect, useMemo, useState } from "react";
import { Activity, ChartNoAxesCombined, ChevronRight, Home, Moon, Search, Sun, UserRound, UsersRound } from "lucide-react";
import { PlayerDepth, TeamDepth, Research } from './Research';
import { OddsPanel } from './OddsPanel';
import { type Player as PlayerGame, type Team as TeamGame, type Dataset as Payload, value } from './analytics';
type View = "overview" | "player" | "team" | "defence" | "shooting" | "matchup" | "odds";
type TeamRow = {
    team: string;
    games: number;
    wins: number;
    points: number;
    opponentPoints: number;
    rebounds: number;
    assists: number;
    pace: number;
};
const playerStats = {
    points: { label: "Points", short: "PTS", get: (g: PlayerGame) => g.points }, rebounds: { label: "Rebounds", short: "REB", get: (g: PlayerGame) => g.rebounds }, assists: { label: "Assists", short: "AST", get: (g: PlayerGame) => g.assists },
    pr: { label: "Points + rebounds", short: "PR", get: (g: PlayerGame) => value(g, "pr") }, pa: { label: "Points + assists", short: "PA", get: (g: PlayerGame) => value(g, "pa") }, ra: { label: "Rebounds + assists", short: "RA", get: (g: PlayerGame) => value(g, "ra") }, threeAttempts: { label: "Three-point attempts", short: "3PA", get: (g: PlayerGame) => g.threeAttempts }, fga: { label: "Field-goal attempts", short: "FGA", get: (g: PlayerGame) => g.fga }, fta: { label: "Free-throw attempts", short: "FTA", get: (g: PlayerGame) => g.fta }, pra: { label: "Points + rebounds + assists", short: "PRA", get: (g: PlayerGame) => value(g, "pra") }, threes: { label: "Three-pointers made", short: "3PM", get: (g: PlayerGame) => g.threes },
    steals: { label: "Steals", short: "STL", get: (g: PlayerGame) => g.steals }, blocks: { label: "Blocks", short: "BLK", get: (g: PlayerGame) => g.blocks }, minutes: { label: "Minutes", short: "MIN", get: (g: PlayerGame) => g.minutes },
    turnovers: { label: "Turnovers", short: "TOV", get: (g: PlayerGame) => g.turnovers }, plusMinus: { label: "Plus / minus", short: "+/−", get: (g: PlayerGame) => g.plusMinus },
} as const;
type PlayerStat = keyof typeof playerStats;
const teamStats = { points: { label: "Points", short: "PTS", get: (g: TeamGame) => g.points }, opponentPoints: { label: "Opponent points", short: "OPP", get: (g: TeamGame) => g.opponentPoints }, rebounds: { label: "Rebounds", short: "REB", get: (g: TeamGame) => g.rebounds }, assists: { label: "Assists", short: "AST", get: (g: TeamGame) => g.assists }, turnovers: { label: "Turnovers", short: "TOV", get: (g: TeamGame) => g.turnovers }, pace: { label: "Pace", short: "PACE", get: (g: TeamGame) => g.pace } } as const;
type TeamStat = keyof typeof teamStats;
const valid = (v: number | null | undefined): v is number => v != null && Number.isFinite(v);
const mean = (values: Array<number | null | undefined>) => { const clean = values.filter(valid); return clean.length ? clean.reduce((a, b) => a + b, 0) / clean.length : 0; };
const fmt = (value: number, digits = 1) => Number.isFinite(value) ? value.toFixed(digits) : "—";
const prettySeason = (season: string) => season.replace(/^(\d{4})-(\d{2})\d{2}$/, "$1–" + season.slice(-2));
const byNewest = <T extends {
    date: string;
}>(a: T, b: T) => b.date.localeCompare(a.date);
export function NblApp() {
    const [data, setData] = useState<Payload | null>(null), [error, setError] = useState(""), [view, setView] = useState<View>("overview"), [dark, setDark] = useState(false);
    useEffect(() => { fetch("/data/nbl-stats.json").then(r => { if (!r.ok)
        throw new Error("Stats export is unavailable"); return r.json(); }).then(setData).catch(e => setError(e.message)); }, []);
    if (error)
        return <Status title="Unable to load NBL data" detail={`${error}. Rebuild the web stats export and refresh.`}/>;
    if (!data)
        return <Status title="Loading the NBL workspace" detail="Preparing player and team history…"/>;
    return <div className={`app-shell ${dark ? "dark" : ""}`}><aside className="sidebar">
    <button className="brand" onClick={() => setView("overview")} aria-label="NBL Analytics home"><span className="brand-mark">NBL</span><span><strong>NBL Analytics</strong><small>Performance workstation</small></span></button>
    <nav className="side-nav" aria-label="Main navigation"><Nav active={view === "overview"} icon={<Home />} label="Overview" onClick={() => setView("overview")}/><Nav active={view === "player"} icon={<UserRound />} label="Player Lab" onClick={() => setView("player")}/><Nav active={view === "team"} icon={<UsersRound />} label="Team Lab" onClick={() => setView("team")}/>{(["defence", "shooting", "matchup"] as const).map(v => <Nav key={v} active={view === v} icon={<Activity />} label={v === "defence" ? "Defence by position" : v === "shooting" ? "Shooting matchups" : "Matchup workspace"} onClick={() => setView(v)}/>)}<Nav active={view === "odds"} icon={<ChartNoAxesCombined />} label="Odds" onClick={() => setView("odds")}/></nav>
    <div className="sidebar-foot"><span className="preseason"><i />2026–27 season</span><button className="icon-button" onClick={() => setDark(v => !v)} aria-label={dark ? "Use light theme" : "Use dark theme"}>{dark ? <Sun /> : <Moon />}</button></div>
  </aside><div className="app-frame"><header className="topbar"><div><strong>{view === "overview" ? "League snapshot" : view === "player" ? "Player analysis" : view === "team" ? "Team analysis" : view === "odds" ? "Odds comparison" : "Matchup research"}</strong><span>{view === "odds" ? "Current NBL market snapshot" : "Historical NBL box scores"}</span></div><span className="season-pill">{view === "odds" ? "2026–27 odds" : "Player & team research"}</span></header><main className="workspace">{view === "overview" && <Overview data={data} goPlayer={() => setView("player")} goTeam={() => setView("team")}/>} {view === "player" && <PlayerLab data={data}/>} {view === "team" && <TeamLab data={data}/>} {(view === "defence" || view === "shooting" || view === "matchup") && <Research data={data} mode={view}/>} {view === "odds" && <OddsPanel/>}</main></div></div>;
}
function Overview({ data, goPlayer, goTeam }: {
    data: Payload;
    goPlayer: () => void;
    goTeam: () => void;
}) {
    const [season, setSeason] = useState(data.metadata.latestSeasonWithGames), games = data.teamGames.filter(g => g.season === season);
    const table = useMemo<TeamRow[]>(() => [...new Set(games.map(g => g.team))].map(team => { const r = games.filter(g => g.team === team); return { team, games: r.length, wins: r.filter(g => (g.points ?? 0) > (g.opponentPoints ?? 0)).length, points: mean(r.map(g => g.points)), opponentPoints: mean(r.map(g => g.opponentPoints)), rebounds: mean(r.map(g => g.rebounds)), assists: mean(r.map(g => g.assists)), pace: mean(r.map(g => g.pace)) }; }).sort((a, b) => b.wins / b.games - a.wins / a.games), [games]);
    const leaders = useMemo(() => { const p = data.playerGames.filter(g => g.season === season); return [...new Set(p.map(g => g.player))].map(player => { const r = p.filter(g => g.player === player); return { player, team: r.at(-1)?.team ?? "", games: r.length, points: mean(r.map(g => g.points)), rebounds: mean(r.map(g => g.rebounds)), assists: mean(r.map(g => g.assists)) }; }).filter(r => r.games >= 5).sort((a, b) => b.points - a.points).slice(0, 8); }, [data.playerGames, season]);
    return <><PageHead eyebrow="National Basketball League" title="Performance overview" detail="Compare league-wide form, team profiles and player leaders." action={<SeasonSelect seasons={data.metadata.seasons} value={season} onChange={setSeason}/>}/>
    <section className="metric-grid"><Metric label="Teams" value={table.length || "—"} detail="clubs tracked"/><Metric label="Games tracked" value={table.reduce((s, r) => s + r.games, 0) / 2 || "—"} detail="match results"/><Metric label="League scoring" value={table.length ? fmt(mean(table.map(r => r.points))) : "—"} detail="points per team"/><Metric label="Average pace" value={table.length ? fmt(mean(table.map(r => r.pace))) : "—"} detail="possessions"/></section>
    <section className="quick-grid"><button className="quick-card" onClick={goPlayer}><span className="quick-icon"><UserRound /></span><span><strong>Explore player form</strong><small>Trends, line rates and every game log</small></span><ChevronRight /></button><button className="quick-card" onClick={goTeam}><span className="quick-icon"><UsersRound /></span><span><strong>Compare team profiles</strong><small>Results, trends and roster leaders</small></span><ChevronRight /></button></section>
    <section className="dashboard-grid"><Panel title="Team form" subtitle={`${prettySeason(season)} snapshot`} extra="Sorted by win rate" wide><TeamTable rows={table}/></Panel><Panel title="Scoring leaders" subtitle="Minimum five appearances" extra="Per game"><table className="compact-table"><thead><tr><th>Player</th><th>PTS</th><th>REB</th><th>AST</th></tr></thead><tbody>{leaders.map(r => <tr key={r.player}><td><strong>{r.player}</strong><small>{r.team}</small></td><td>{fmt(r.points)}</td><td>{fmt(r.rebounds)}</td><td>{fmt(r.assists)}</td></tr>)}</tbody></table></Panel></section>
  </>;
}
function PlayerLab({ data }: {
    data: Payload;
}) {
    const [season, setSeason] = useState(data.metadata.latestSeasonWithGames), seasonGames = data.playerGames.filter(g => g.season === season), players = [...new Set(seasonGames.map(g => g.player))].sort();
    const [player, setPlayer] = useState(players[0] ?? ""), [stat, setStat] = useState<PlayerStat>("points"), [line, setLine] = useState(15.5), [venue, setVenue] = useState("all"), [opponent, setOpponent] = useState("all"), [sample, setSample] = useState("all"), [query, setQuery] = useState("");
    const changeSeason = (next: string) => { setSeason(next); setPlayer([...new Set(data.playerGames.filter(g => g.season === next).map(g => g.player))].sort()[0] ?? ""); setOpponent("all"); setQuery(""); };
    const base = seasonGames.filter(g => g.player === player).sort(byNewest), opponents = [...new Set(base.map(g => g.opponent))].sort();
    let rows = base.filter(g => (venue === "all" || g.homeAway === venue) && (opponent === "all" || g.opponent === opponent));
    if (sample !== "all")
        rows = rows.slice(0, Number(sample));
    const definition = playerStats[stat], values = rows.map(definition.get).filter(valid), hits = values.filter(v => v > line).length, chartRows = [...rows].filter(g => valid(definition.get(g))).reverse().slice(-16), filteredPlayers = query ? players.filter(p => p.toLowerCase().includes(query.toLowerCase())).slice(0, 8) : [];
    return <><PageHead eyebrow="Player lab" title={player || "No player data"} detail="Inspect production, recent form and reference-line performance." action={<SeasonSelect seasons={data.metadata.seasons} value={season} onChange={changeSeason}/>}/>
    <section className="control-panel panel"><div className="player-picker"><label htmlFor="player-search">Player</label><div className="search-control"><Search /><input id="player-search" value={query || player} onFocus={e => { setQuery(""); e.currentTarget.select(); }} onChange={e => setQuery(e.target.value)} placeholder="Search players"/>{filteredPlayers.length > 0 && <div className="search-results">{filteredPlayers.map(p => <button key={p} onClick={() => { setPlayer(p); setOpponent("all"); setQuery(""); }}>{p}<small>{seasonGames.find(g => g.player === p)?.team}</small></button>)}</div>}</div></div><Select label="Stat" value={stat} onChange={v => setStat(v as PlayerStat)} options={Object.entries(playerStats).map(([value, d]) => ({ value, label: d.label }))}/><label className="field"><span>Reference line</span><input type="number" step="0.5" value={line} onChange={e => setLine(Number(e.target.value))}/></label><Select label="Sample" value={sample} onChange={setSample} options={[{ value: "all", label: "All games" }, { value: "5", label: "Last 5" }, { value: "10", label: "Last 10" }, { value: "20", label: "Last 20" }]}/><Select label="Venue" value={venue} onChange={setVenue} options={[{ value: "all", label: "All venues" }, { value: "home", label: "Home" }, { value: "away", label: "Away" }]}/><Select label="Opponent" value={opponent} onChange={setOpponent} options={[{ value: "all", label: "All opponents" }, ...opponents.map(v => ({ value: v, label: v }))]}/></section>
    <section className="metric-grid"><Metric label="Games" value={values.length} detail="selected sample"/><Metric label={`Average ${definition.short}`} value={values.length ? fmt(mean(values)) : "—"} detail={`${definition.label.toLowerCase()} per game`}/><Metric label={`Over ${line}`} value={values.length ? `${Math.round(hits / values.length * 100)}%` : "—"} detail={`${hits} of ${values.length} games`}/><Metric label="Sample high" value={values.length ? fmt(Math.max(...values)) : "—"} detail={definition.short}/></section>
    <section className="dashboard-grid"><Panel title={`${definition.label} trend`} subtitle={`${prettySeason(season)} · most recent ${chartRows.length} games`} extra={`Line ${line}`} wide><TrendChart rows={chartRows.map(g => ({ label: new Date(g.date).toLocaleDateString("en-AU", { day: "numeric", month: "short" }), value: definition.get(g) ?? 0, meta: g.opponent }))} line={line}/></Panel><Panel title="Selected profile" subtitle={base[0]?.team ?? "No team"}><div className="profile-list"><Profile label="Position" value={base[0]?.position || "—"}/><Profile label="Starts" value={`${base.filter(g => g.starter).length} / ${base.length}`}/><Profile label="Minutes" value={base.length ? fmt(mean(base.map(g => g.minutes))) : "—"}/><Profile label="Last game" value={base[0]?.date ? new Date(base[0].date).toLocaleDateString("en-AU", { day: "numeric", month: "short", year: "numeric" }) : "—"}/></div></Panel></section>
    <PlayerDepth rows={rows} data={data}/><Panel title="Game log" subtitle={`${rows.length} games match the current filters`} extra="Newest first"><PlayerLog rows={rows} stat={stat} line={line}/></Panel></>;
}
function TeamLab({ data }: {
    data: Payload;
}) {
    const [season, setSeason] = useState(data.metadata.latestSeasonWithGames), teams = useMemo(() => [...new Set(data.teamGames.filter(g => g.season === season).map(g => g.team))].sort(), [data.teamGames, season]), [team, setTeam] = useState(teams[0] ?? ""), [stat, setStat] = useState<TeamStat>("points"), [sample, setSample] = useState("all"), [venue, setVenue] = useState("all");
    const changeSeason = (next: string) => { setSeason(next); setTeam([...new Set(data.teamGames.filter(g => g.season === next).map(g => g.team))].sort()[0] ?? ""); };
    const rows = data.teamGames.filter(g => g.season === season && g.team === team && (venue === "all" || g.homeAway === venue)).sort(byNewest).slice(0, sample === "all" ? Infinity : Number(sample)), wins = rows.filter(g => (g.points ?? 0) > (g.opponentPoints ?? 0)).length, def = teamStats[stat], chartRows = [...rows].filter(g => valid(def.get(g))).reverse().slice(-16);
    const roster = useMemo(() => { const pg = data.playerGames.filter(g => g.season === season && g.team === team); return [...new Set(pg.map(g => g.player))].map(player => { const r = pg.filter(g => g.player === player); return { player, games: r.length, points: mean(r.map(g => g.points)), rebounds: mean(r.map(g => g.rebounds)), assists: mean(r.map(g => g.assists)) }; }).sort((a, b) => b.points - a.points).slice(0, 10); }, [data.playerGames, season, team]);
    const margin = mean(rows.map(g => (g.points ?? 0) - (g.opponentPoints ?? 0)));
    return <><PageHead eyebrow="Team lab" title={team || "No team data"} detail="Follow results, performance trends and roster production." action={<SeasonSelect seasons={data.metadata.seasons} value={season} onChange={changeSeason}/>}/><section className="control-panel panel"><Select label="Team" value={team} onChange={setTeam} options={teams.map(v => ({ value: v, label: v }))}/><Select label="Sample" value={sample} onChange={setSample} options={[{ value: "all", label: "All games" }, { value: "5", label: "Last 5" }, { value: "10", label: "Last 10" }]}/><Select label="Venue" value={venue} onChange={setVenue} options={[{ value: "all", label: "All venues" }, { value: "home", label: "Home" }, { value: "away", label: "Away" }]}/><Select label="Trend metric" value={stat} onChange={v => setStat(v as TeamStat)} options={Object.entries(teamStats).map(([value, d]) => ({ value, label: d.label }))}/></section>
    <section className="metric-grid"><Metric label="Record" value={`${wins}–${rows.length - wins}`} detail={rows.length ? `${Math.round(wins / rows.length * 100)}% win rate` : "no games"}/><Metric label="Points for" value={rows.length ? fmt(mean(rows.map(g => g.points))) : "—"} detail="per game"/><Metric label="Points against" value={rows.length ? fmt(mean(rows.map(g => g.opponentPoints))) : "—"} detail="per game"/><Metric label="Average margin" value={rows.length ? `${margin >= 0 ? "+" : ""}${fmt(margin)}` : "—"} detail="points"/></section>
    <section className="dashboard-grid"><Panel title={`${def.label} trend`} subtitle={`${prettySeason(season)} · most recent ${chartRows.length} games`} extra={def.short} wide><TrendChart rows={chartRows.map(g => ({ label: new Date(g.date).toLocaleDateString("en-AU", { day: "numeric", month: "short" }), value: def.get(g) ?? 0, meta: g.opponent }))}/></Panel><Panel title="Roster leaders" subtitle="Per-game production" extra="PTS"><table className="compact-table"><thead><tr><th>Player</th><th>GP</th><th>PTS</th></tr></thead><tbody>{roster.map(r => <tr key={r.player}><td><strong>{r.player}</strong><small>{fmt(r.rebounds)} REB · {fmt(r.assists)} AST</small></td><td>{r.games}</td><td>{fmt(r.points)}</td></tr>)}</tbody></table></Panel></section><TeamDepth rows={rows} data={data}/><Panel title="Team game log" subtitle={`${rows.length} games`} extra="Newest first"><TeamLog rows={rows}/></Panel></>;
}
function TrendChart({ rows, line }: {
    rows: {
        label: string;
        value: number;
        meta: string;
    }[];
    line?: number;
}) { const min = Math.min(0, ...rows.map(r => r.value), line ?? 0), max = Math.max(1, ...rows.map(r => r.value), line ?? 0), range = max - min, zero = -min / range * 100; return <div className="trend-chart" aria-label="Performance trend bar chart">{rows.length ? rows.map((r, i) => <div className="bar-column" key={`${r.label}-${i}`} title={`${r.meta}: ${fmt(r.value)}`}><span className="bar-value">{fmt(r.value)}</span><div className="bar-track"><i className="line-marker" style={{ bottom: `${zero}%` }}/>{line != null && <i className="line-marker" style={{ bottom: `${(line - min) / range * 100}%` }}/>}<b style={{ position: "absolute", bottom: `${r.value < 0 ? (r.value - min) / range * 100 : zero}%`, height: `${Math.abs(r.value) / range * 100}%` }} className={line != null && r.value > line ? "hit" : ""}/></div><small>{r.label}</small></div>) : <Empty />}</div>; }
function TeamTable({ rows }: {
    rows: TeamRow[];
}) { return <div className="table-wrap"><table><thead><tr><th>Team</th><th>GP</th><th>W</th><th>Win %</th><th>PTS</th><th>OPP</th><th>REB</th><th>AST</th><th>PACE</th></tr></thead><tbody>{rows.map((r, i) => <tr key={r.team}><td><span className="rank">{i + 1}</span><strong>{r.team}</strong></td><td>{r.games}</td><td>{r.wins}</td><td>{Math.round(r.wins / r.games * 100)}%</td><td>{fmt(r.points)}</td><td>{fmt(r.opponentPoints)}</td><td>{fmt(r.rebounds)}</td><td>{fmt(r.assists)}</td><td>{fmt(r.pace)}</td></tr>)}</tbody></table></div>; }
function PlayerLog({ rows, stat, line }: {
    rows: PlayerGame[];
    stat: PlayerStat;
    line: number;
}) { const d = playerStats[stat]; return <div className="table-wrap"><table><thead><tr><th>Date</th><th>Opponent</th><th>H/A</th><th>MIN</th><th>PTS</th><th>REB</th><th>AST</th><th>{d.short}</th><th>Line</th></tr></thead><tbody>{rows.map(g => { const value = d.get(g); return <tr key={g.matchId}><td>{new Date(g.date).toLocaleDateString("en-AU")}</td><td><strong>{g.opponent}</strong></td><td>{g.homeAway === "home" ? "H" : "A"}</td><td>{valid(g.minutes) ? fmt(g.minutes) : "—"}</td><td>{g.points ?? "—"}</td><td>{g.rebounds ?? "—"}</td><td>{g.assists ?? "—"}</td><td><strong>{valid(value) ? fmt(value) : "—"}</strong></td><td>{valid(value) ? <span className={`result-tag ${value > line ? "over" : "under"}`}>{value > line ? "OVER" : value < line ? "UNDER" : "EQUAL"}</span> : "—"}</td></tr>; })}</tbody></table>{!rows.length && <Empty />}</div>; }
function TeamLog({ rows }: {
    rows: TeamGame[];
}) { return <div className="table-wrap"><table><thead><tr><th>Date</th><th>Opponent</th><th>Venue</th><th>Result</th><th>Score</th><th>REB</th><th>AST</th><th>TOV</th><th>FG%</th></tr></thead><tbody>{rows.map(g => { const win = (g.points ?? 0) > (g.opponentPoints ?? 0); return <tr key={g.matchId}><td>{new Date(g.date).toLocaleDateString("en-AU")}</td><td><strong>{g.opponent}</strong></td><td>{g.homeAway === "home" ? "Home" : "Away"}</td><td><span className={`result-tag ${win ? "over" : "under"}`}>{win ? "W" : "L"}</span></td><td><strong>{g.points}–{g.opponentPoints}</strong></td><td>{g.rebounds ?? "—"}</td><td>{g.assists ?? "—"}</td><td>{g.turnovers ?? "—"}</td><td>{valid(g.fieldGoalPct) ? `${fmt(g.fieldGoalPct)}%` : "—"}</td></tr>; })}</tbody></table>{!rows.length && <Empty />}</div>; }
function Nav({ active, icon, label, onClick }: {
    active: boolean;
    icon: React.ReactNode;
    label: string;
    onClick: () => void;
}) { return <button className={active ? "is-active" : ""} onClick={onClick}>{icon}<span>{label}</span></button>; }
function PageHead({ eyebrow, title, detail, action }: {
    eyebrow: string;
    title: string;
    detail: string;
    action?: React.ReactNode;
}) { return <div className="page-title-row"><div><p className="eyebrow">{eyebrow}</p><h1>{title}</h1><p className="page-detail">{detail}</p></div>{action}</div>; }
function SeasonSelect({ seasons, value, onChange }: {
    seasons: string[];
    value: string;
    onChange: (v: string) => void;
}) { const available = [...new Set(seasons)]; return <label className="season-control"><span>Season</span><select value={value} onChange={e => onChange(e.target.value)}>{available.map(s => <option key={s} value={s}>{prettySeason(s)}</option>)}</select></label>; }
function Select({ label, value, onChange, options }: {
    label: string;
    value: string;
    onChange: (v: string) => void;
    options: {
        value: string;
        label: string;
    }[];
}) { return <label className="field"><span>{label}</span><select value={value} onChange={e => onChange(e.target.value)}>{options.map(o => <option key={o.value} value={o.value}>{o.label}</option>)}</select></label>; }
function Panel({ title, subtitle, extra, children, wide = false }: {
    title: string;
    subtitle?: string;
    extra?: string;
    children: React.ReactNode;
    wide?: boolean;
}) { return <section className={`panel data-panel ${wide ? "wide" : ""}`}><div className="section-heading"><div><h2>{title}</h2>{subtitle && <p>{subtitle}</p>}</div>{extra && <span>{extra}</span>}</div>{children}</section>; }
function Metric({ label, value, detail }: {
    label: string;
    value: string | number;
    detail: string;
}) { return <article className="panel metric-card"><span>{label}</span><strong>{value}</strong><small>{detail}</small></article>; }
function Profile({ label, value }: {
    label: string;
    value: string;
}) { return <div><span>{label}</span><strong>{value}</strong></div>; }
function Empty() { return <div className="empty-state"><Activity /><strong>No games match these filters</strong><span>Try a different season or filter.</span></div>; }
function Status({ title, detail }: {
    title: string;
    detail: string;
}) { return <main className="status-page"><div className="brand-mark">NBL</div><h1>{title}</h1><p>{detail}</p></main>; }
