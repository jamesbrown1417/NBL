/** Canonical descriptive DVP estimator. Used by the R pipeline and web export. */
export const DVP_VERSION = '2.0.0';
export const DVP_STATS = ['points', 'rebounds', 'assists', 'threes', 'steals', 'blocks', 'pra'] as const;
export type DvpStat = typeof DVP_STATS[number];
export type DvpBasis = 'per36' | 'per100';
export type DvpRow = {
    matchId: string;
    season: string;
    date: string;
    player: string;
    team: string;
    opponent: string;
    position: string | null;
    minutes: number | null;
    points: number | null;
    rebounds: number | null;
    assists: number | null;
    threes: number | null;
    steals: number | null;
    blocks: number | null;
    pace?: number | null;
    paceSource?: string;
    positionSource?: string;
    playerKey?: string;
};
export type PreparedRow = DvpRow & {
    playerKey: string;
    position: string;
    positionSource: string;
};
const finite = (n: unknown): n is number => typeof n === 'number' && Number.isFinite(n);
export function position(value: string | null): string {
    const p = (value ?? '').trim().toUpperCase();
    if (['C', 'CTR', 'CEN', 'CENTER', 'CENTRE', 'C/F', 'FC'].includes(p))
        return 'C';
    if (['F', 'FWD', 'FORWARD', 'PF', 'SF', 'F/G'].includes(p))
        return 'F';
    if (['G', 'GRD', 'GUARD', 'PG', 'SG', 'PG/SG'].includes(p))
        return 'G';
    return 'Unknown';
}
const identity = (g: DvpRow) => g.playerKey || g.player.normalize('NFKC').trim().replace(/\s+/g, ' ').toLowerCase();
const fields = ['team', 'opponent', 'date', 'minutes', ...DVP_STATS.filter(k => k !== 'pra'), 'pace'] as const;
export function prepareDvpRows(input: DvpRow[], cutoff?: string) {
    const unique = new Map<string, DvpRow>();
    let duplicates = 0;
    for (const g of input) {
        if (cutoff && g.date >= cutoff)
            continue;
        const key = JSON.stringify([g.season, g.matchId, identity(g)]), previous = unique.get(key);
        if (previous) {
            if (fields.some(k => (previous[k] ?? null) !== (g[k] ?? null)) || position(previous.position) !== position(g.position)) {
                throw new Error(`Conflicting DVP records: ${g.season} / ${g.matchId} / ${g.player}`);
            }
            duplicates++;
        }
        else
            unique.set(key, g);
    }
    // Only observations strictly earlier than this date may fill a missing position.
    // Reset at season and team boundaries; never use a future/current fantasy roster.
    const history = new Map<string, {
        date: string;
        positions: Set<string>;
    }>();
    const rows: PreparedRow[] = [];
    const ordered = [...unique.values()].sort((a, b) => a.date.localeCompare(b.date));
    for (let i = 0; i < ordered.length;) {
        let end = i + 1;
        while (end < ordered.length && ordered[end].date === ordered[i].date)
            end++;
        for (const g of ordered.slice(i, end)) {
            const key = JSON.stringify([g.season, identity(g), g.team]);
            const recorded = position(g.position), prior = history.get(key);
            const inherited = recorded === 'Unknown' && prior?.positions.size === 1 ? [...prior.positions][0] : null;
            rows.push({ ...g, playerKey: identity(g), position: inherited ?? recorded,
                positionSource: inherited ? 'earlier same-season team appearance' : recorded === 'Unknown' ? 'unclassified' : 'recorded box score' });
        }
        for (const g of ordered.slice(i, end)) {
            const p = position(g.position);
            if (p === 'Unknown')
                continue;
            const key = JSON.stringify([g.season, identity(g), g.team]), prior = history.get(key);
            if (prior?.date === g.date)
                prior.positions.add(p);
            else
                history.set(key, { date: g.date, positions: new Set([p]) });
        }
        i = end;
    }
    return { rows, duplicates };
}
export const DVP_RULES = {
    minMinutes: 5, minTargetGames: 2, minBaselineGames: 5,
    minTargetMinutesPerPlayer: 20, minBaselineMinutesPerPlayer: 60,
    minDistinctGames: 5, minPlayers: 3, minEffectivePlayers: 3,
    minTotalTargetMinutes: 120, minTotalBaselineMinutes: 360,
};
export function statValue(g: DvpRow, stat: DvpStat): number | null {
    if (stat === 'pra')
        return [g.points, g.rebounds, g.assists].every(finite) ? g.points! + g.rebounds! + g.assists! : null;
    return finite(g[stat]) ? g[stat] : null;
}
type Observation = {
    game: string;
    x: number;
    y: number;
    minutes: number;
};
type Detail = {
    player: string;
    team: string;
    games: number;
    baselineGames: number;
    targetMinutes: number;
    baselineMinutes: number;
    against: number;
    baseline: number;
    difference: number;
    weight: number;
    share: number;
};
export type DvpCell = {
    team: string;
    position: string;
    stat: DvpStat;
    basis: DvpBasis;
    difference: number | null;
    regularized: number | null;
    shrinkage: number | null;
    standardError: number | null;
    interval: [
        number,
        number
    ] | null;
    players: number;
    effectivePlayers: number;
    games: number;
    baselineGames: number;
    appearances: number;
    targetMinutes: number;
    baselineMinutes: number;
    eligibleTargetAppearances: number;
    classifiedTargetAppearances: number;
    inheritedTargetAppearances: number;
    missingStatAppearances: number;
    missingPaceAppearances: number;
    estimatedPaceAppearances: number;
    excludedPlayers: number;
    sufficient: boolean;
    reasons: string[];
    details: Detail[];
};
export function calculateDvp(rows: PreparedRow[], team: string, pos: string, stat: DvpStat, basis: DvpBasis = 'per36'): DvpCell {
    if (!DVP_STATS.includes(stat))
        throw new Error(`Unsupported DVP stat: ${stat}`);
    if (new Set(rows.map(g => g.season)).size > 1)
        throw new Error('DVP requires one season or an explicit historical cutoff within a season');
    const target = rows.filter(g => g.opponent === team && finite(g.minutes) && g.minutes >= DVP_RULES.minMinutes);
    const relevant = rows.filter(g => g.position === pos && finite(g.minutes) && g.minutes >= DVP_RULES.minMinutes);
    const groups = new Map<string, {
        player: string;
        team: string;
        a: Observation[];
        b: Observation[];
    }>();
    for (const g of relevant) {
        const y = statValue(g, stat);
        if (y === null || (basis === 'per100' && (!finite(g.pace) || g.pace <= 0)))
            continue;
        const key = JSON.stringify([g.playerKey, g.team]), group = groups.get(key) ?? { player: g.player, team: g.team, a: [], b: [] };
        const x = basis === 'per36' ? g.minutes! : g.minutes! * g.pace! / 40;
        (g.opponent === team ? group.a : group.b).push({ game: g.matchId, x, y, minutes: g.minutes! });
        groups.set(key, group);
    }
    const total = (rs: Observation[], key: 'x' | 'y' | 'minutes') => rs.reduce((n, r) => n + r[key], 0);
    const count = (rs: Observation[]) => new Set(rs.map(r => r.game)).size;
    const scale = basis === 'per36' ? 36 : 100;
    const comparisons = [...groups.values()].filter(g => count(g.a) >= DVP_RULES.minTargetGames && count(g.b) >= DVP_RULES.minBaselineGames && total(g.a, 'minutes') >= DVP_RULES.minTargetMinutesPerPlayer && total(g.b, 'minutes') >= DVP_RULES.minBaselineMinutesPerPlayer);
    const details = comparisons.map(g => {
        const a = total(g.a, 'x'), b = total(g.b, 'x');
        const against = scale * total(g.a, 'y') / a, baseline = scale * total(g.b, 'y') / b;
        return { player: g.player, team: g.team, games: count(g.a), baselineGames: count(g.b), targetMinutes: total(g.a, 'minutes'), baselineMinutes: total(g.b, 'minutes'), against, baseline, difference: against - baseline, weight: a * b / (a + b), share: 0 };
    });
    const weight = details.reduce((n, d) => n + d.weight, 0);
    const difference = weight ? details.reduce((n, d) => n + d.weight * d.difference, 0) / weight : null;
    details.forEach(d => { d.share = d.weight / weight; });
    // Delta-method influence of each game, including changes to both rate and exposure weights.
    // Aggregate all observations from the same match before estimating uncertainty.
    const influences = new Map<string, number>();
    comparisons.forEach((g, i) => {
        const d = details[i], a = total(g.a, 'x'), b = total(g.b, 'x');
        for (const [observations, isTarget] of [[g.a, true], [g.b, false]] as const) {
            for (const r of observations) {
                const dw = (isTarget ? b * b : a * a) / (a + b) ** 2 * r.x;
                const dr = isTarget ? scale * (r.y - d.against / scale * r.x) / a : -scale * (r.y - d.baseline / scale * r.x) / b;
                const influence = (dw * (d.difference - difference!) + d.weight * dr) / weight;
                influences.set(r.game, (influences.get(r.game) ?? 0) + influence);
            }
        }
    });
    const n = influences.size;
    const standardError = n > 1 ? Math.sqrt(n / (n - 1) * [...influences.values()].reduce((v, x) => v + x * x, 0)) : null;
    const games = new Set(comparisons.flatMap(g => g.a.map(r => r.game))).size;
    const baselineGames = new Set(comparisons.flatMap(g => g.b.map(r => r.game))).size;
    // Team-stints of the same player remain separate baselines but do not inflate player counts.
    const playerShares = new Map<string, number>();
    details.forEach(d => playerShares.set(d.player, (playerShares.get(d.player) ?? 0) + d.share));
    const effectivePlayers = weight ? 1 / [...playerShares.values()].reduce((n, s) => n + s * s, 0) : 0;
    const targetMinutes = details.reduce((n, d) => n + d.targetMinutes, 0), baselineMinutes = details.reduce((n, d) => n + d.baselineMinutes, 0);
    const reasons = [];
    if (games < DVP_RULES.minDistinctGames)
        reasons.push('Fewer than 5 distinct target games');
    if (playerShares.size < DVP_RULES.minPlayers || effectivePlayers < DVP_RULES.minEffectivePlayers)
        reasons.push('Fewer than 3 effective players');
    if (targetMinutes < DVP_RULES.minTotalTargetMinutes || baselineMinutes < DVP_RULES.minTotalBaselineMinutes)
        reasons.push('Insufficient target or baseline minutes');
    if (standardError === null)
        reasons.push('Uncertainty unavailable');
    return { team, position: pos, stat, basis, difference, regularized: null, shrinkage: null, standardError,
        interval: difference !== null && standardError !== null ? [difference - 1.96 * standardError, difference + 1.96 * standardError] : null,
        players: playerShares.size, effectivePlayers, games, baselineGames, appearances: comparisons.reduce((n, g) => n + g.a.length, 0), targetMinutes, baselineMinutes,
        eligibleTargetAppearances: target.length, classifiedTargetAppearances: target.filter(g => g.position !== 'Unknown').length,
        inheritedTargetAppearances: target.filter(g => g.positionSource === 'earlier same-season team appearance').length,
        missingStatAppearances: relevant.filter(g => g.opponent === team && statValue(g, stat) === null).length,
        missingPaceAppearances: relevant.filter(g => g.opponent === team && (!finite(g.pace) || g.pace <= 0)).length,
        estimatedPaceAppearances: relevant.filter(g => g.opponent === team && finite(g.pace) && g.paceSource === 'estimated from both team box scores').length,
        excludedPlayers: [...groups.values()].filter(g => g.a.length).length - comparisons.length,
        sufficient: reasons.length === 0, reasons, details };
}
/** Empirical-Bayes shrinkage toward neutral, fit separately within season/stat/position/basis. */
export function regularizeDvp(cells: DvpCell[]) {
    if (new Set(cells.map(c => `${c.position}|${c.stat}|${c.basis}`)).size > 1)
        throw new Error('Regularization requires a single position, stat and basis');
    const fitted = cells.filter(c => c.sufficient && c.difference !== null && c.standardError !== null);
    const meanSquare = fitted.length ? fitted.reduce((n, c) => n + c.difference! ** 2 - c.standardError! ** 2, 0) / fitted.length : 0;
    const tauSquared = Math.max(0, meanSquare);
    return cells.map(c => {
        if (!c.sufficient || fitted.length < 3 || c.difference === null || c.standardError === null)
            return c;
        const shrinkage = tauSquared === 0 ? 0 : tauSquared / (tauSquared + c.standardError ** 2);
        return { ...c, regularized: shrinkage === 0 ? 0 : c.difference * shrinkage, shrinkage };
    });
}
export type DvpSnapshot = {
    version: string;
    season: string;
    cutoff: string | null;
    rules: typeof DVP_RULES;
    duplicates: number;
    eligible: number;
    unknown: number;
    inherited: number;
    cells: DvpCell[];
};
export function buildDvpSnapshot(input: DvpRow[], season: string, cutoff?: string): DvpSnapshot {
    const { rows, duplicates } = prepareDvpRows(input.filter(g => g.season === season), cutoff);
    const teams = [...new Set(rows.flatMap(g => [g.team, g.opponent]))].sort();
    const cells = (['per36', 'per100'] as const).flatMap(basis => DVP_STATS.flatMap(stat => ['C', 'F', 'G'].flatMap(p => regularizeDvp(teams.map(t => calculateDvp(rows, t, p, stat, basis))))));
    const eligible = rows.filter(g => finite(g.minutes) && g.minutes >= DVP_RULES.minMinutes);
    return { version: DVP_VERSION, season, cutoff: cutoff ?? null, rules: DVP_RULES, duplicates, eligible: eligible.length, unknown: eligible.filter(g => g.position === 'Unknown').length, inherited: eligible.filter(g => g.positionSource === 'earlier same-season team appearance').length, cells };
}
