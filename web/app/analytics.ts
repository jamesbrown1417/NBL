export type Box = {
    [key: string]: unknown;
    matchId: string;
    season: string;
    date: string;
    team: string;
    opponent: string;
    homeAway: string;
    points: number | null;
    rebounds: number | null;
    assists: number | null;
    turnovers: number | null;
    steals: number | null;
    blocks: number | null;
    fga: number | null;
    fgm: number | null;
    threeAttempts: number | null;
    threes: number | null;
    fta: number | null;
    ftm: number | null;
    offensiveRebounds: number | null;
    defensiveRebounds: number | null;
    fouls: number | null;
    foulsDrawn: number | null;
    paintPoints: number | null;
    fastBreakPoints: number | null;
    secondChancePoints: number | null;
};
export type Player = Box & {
    player: string;
    position: string | null;
    minutes: number | null;
    starter: boolean | null;
    plusMinus: number | null;
};
export type Team = Box & {
    fieldGoalPct: number | null;
    threePct: number | null;
    freeThrowPct: number | null;
    opponentPoints: number | null;
    pace: number | null;
    possessions: number | null;
    benchPoints: number | null;
    q1: number | null;
    q2: number | null;
    q3: number | null;
    q4: number | null;
};
export type Dataset = {
    dvp?: {
        version: string;
        files: Record<string, string>;
    };
    metadata: {
        seasons: string[];
        latestSeasonWithGames: string;
        activeSeason: string;
        generatedAt: string;
    };
    playerGames: Player[];
    teamGames: Team[];
};
export const numeric = (v: unknown): v is number => typeof v === 'number' && Number.isFinite(v);
export const average = (vs: unknown[]) => { const v = vs.filter(numeric); return v.length ? v.reduce((a, b) => a + b, 0) / v.length : null; };
export const sum = (vs: unknown[]) => vs.every(numeric) && vs.length ? vs.reduce((a, b) => a + b, 0) : null;
export const ratio = (a: number | null, b: number | null, m = 100) => a !== null && b !== null && b > 0 ? a / b * m : null;
export const display = (v: number | null, d = 1) => v === null ? '—' : v.toFixed(d);
export const metrics: Record<string, string> = { points: 'Points', rebounds: 'Rebounds', assists: 'Assists', pra: 'PTS + REB + AST', pr: 'PTS + REB', pa: 'PTS + AST', ra: 'REB + AST', threes: '3PM', threeAttempts: '3PA', fga: 'FGA', fta: 'FTA', steals: 'Steals', blocks: 'Blocks', turnovers: 'Turnovers', minutes: 'Minutes', plusMinus: 'Plus / minus', offensiveRebounds: 'Offensive rebounds', defensiveRebounds: 'Defensive rebounds', fouls: 'Fouls', foulsDrawn: 'Fouls drawn', paintPoints: 'Paint points', fastBreakPoints: 'Fast-break points', secondChancePoints: 'Second-chance points' };
export function value(g: Box, key: string): number | null { const keys: Record<string, string[]> = { pra: ['points', 'rebounds', 'assists'], pr: ['points', 'rebounds'], pa: ['points', 'assists'], ra: ['rebounds', 'assists'] }; return keys[key] ? sum(keys[key].map(k => g[k])) : numeric(g[key]) ? g[key] : null; }
export function distribution(rows: Box[], key: string, line: number) { const v = rows.map(g => value(g, key)).filter(numeric).sort((a, b) => a - b); const avg = average(v); return { n: v.length, mean: avg, median: v.length ? (v[Math.floor((v.length - 1) / 2)] + v[Math.ceil((v.length - 1) / 2)]) / 2 : null, sd: avg === null ? null : Math.sqrt(v.reduce((a, b) => a + (b - avg) ** 2, 0) / v.length), over: v.filter(x => x > line).length, under: v.filter(x => x < line).length, equal: v.filter(x => x === line).length }; }
export function rate(rows: Box[], kind: string) {
    const total = (k: string) => sum(rows.map(g => g[k]));
    const pairs = rows.filter(g => numeric(g.fga) && numeric(g.fgm) && numeric(g.threes) && numeric(g.fta) && numeric(g.points));
    if (kind === 'efg')
        return ratio(sum(pairs.map(g => g.fgm! + 0.5 * g.threes!)), sum(pairs.map(g => g.fga)));
    if (kind === 'ts')
        return ratio(sum(pairs.map(g => g.points)), sum(pairs.map(g => 2 * (g.fga! + 0.44 * g.fta!))));
    return kind === '3par' ? ratio(total('threeAttempts'), total('fga')) : kind === 'ftr' ? ratio(total('fta'), total('fga')) : null;
}
