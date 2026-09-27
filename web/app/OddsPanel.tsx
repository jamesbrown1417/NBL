"use client";

import { useEffect, useMemo, useState, type Dispatch, type SetStateAction } from "react";
import { Activity } from "lucide-react";
import { sortRows, type SortDirection } from "./odds-sort";
import { edge, edgeBand, fairPrice, sideProbability, type EdgeBand, type Side } from "./model-edge";

type Price = number | null;
type Prop = {
  match: string; homeTeam: string; awayTeam: string; market: string;
  player: string; team: string; line: number; agency: string;
  overPrice: Price; underPrice: Price; gamesCurrent: number;
  hitCurrent: Price; hitLast10: Price;
  modelMean?: Price; modelProbOver?: Price; modelSource?: string | null;
  betSignal?: boolean | null; betSide?: Side | null; altLegTier?: string | null; altLegBacktestRoi?: Price;
};
type HeadToHead = {
  match: string; homeTeam: string; awayTeam: string; agency: string;
  homePrice: Price; awayPrice: Price;
};
type Total = {
  match: string; homeTeam: string; awayTeam: string; agency: string;
  line: number; overPrice: Price; underPrice: Price;
};
type OddsPayload = {
  metadata: {
    generatedAt: string; season: string; agencies: string[];
    modelGeneratedAt?: string | null; modelSeasonWeek?: number | null; modelSignalsFromWeek?: number | null;
  };
  headToHead: HeadToHead[]; totals: Total[]; props: Prop[];
};
type PropRow = Prop & {
  side: Side; price: number; hitRate: Price; recentHitRate: Price; best: boolean;
  modelProb: Price; modelPrice: Price; modelEdge: Price; oneSided: boolean; displayEdge: Price;
  band: EdgeBand | null; signal: boolean; topLeg: boolean;
};
type PropSort = "player" | "market" | "selection" | "agency" | "price" | "model" | "edge" | "season" | "last10";
type H2hSort = "agency" | "home" | "away";
type TotalSort = "agency" | "line" | "over" | "under";
type SortState<Key extends string> = { key: Key; direction: SortDirection };

const priceText = (price: Price) => price !== null && Number.isFinite(price) ? price.toFixed(2) : "—";
const percentText = (rate: Price) => rate !== null && Number.isFinite(rate) ? `${Math.round(rate * 100)}%` : "—";
const edgeText = (value: Price) => value !== null && Number.isFinite(value) ? `${value >= 0 ? "+" : ""}${(value * 100).toFixed(1)}%` : "—";
const bandLabels: Record<EdgeBand, string> = {
  "strong-positive": "+10% or more", positive: "+3% to +10%", neutral: "−3% to +3%", negative: "−15% to −3%", "strong-negative": "below −15%",
};
const selectOptions = (values: string[]) => values.map(value => <option key={value} value={value}>{value}</option>);

function SortHeader({ label, active, direction, onClick }: { label: string; active: boolean; direction: SortDirection; onClick: () => void }) {
  return <th aria-sort={active ? direction === "asc" ? "ascending" : "descending" : "none"}><button type="button" className="odds-sort-button" onClick={onClick}>{label}<span aria-hidden="true">{active ? direction === "asc" ? "↑" : "↓" : "↕"}</span></button></th>;
}

function allPropRows(props: Prop[]): PropRow[] {
  const rows = props.flatMap(prop => ([
    ...(prop.overPrice !== null && prop.overPrice > 1 ? [{ ...prop, side: "Over" as const, price: prop.overPrice, hitRate: prop.hitCurrent, recentHitRate: prop.hitLast10 }] : []),
    ...(prop.underPrice !== null && prop.underPrice > 1 ? [{ ...prop, side: "Under" as const, price: prop.underPrice, hitRate: prop.hitCurrent === null ? null : 1 - prop.hitCurrent, recentHitRate: prop.hitLast10 === null ? null : 1 - prop.hitLast10 }] : [])
  ]));
  const best = new Map<string, number>();
  for (const row of rows) {
    const key = [row.match, row.player, row.market, row.line, row.side].join("|");
    best.set(key, Math.max(best.get(key) ?? 0, row.price));
  }
  return rows.map(row => {
    const modelProb = sideProbability(row.modelProbOver, row.side);
    const modelEdge = edge(modelProb, row.price);
    // One-sided X+ lines: raw model edges did not hold up in the backtest, so
    // rank and colour them by the backtested return of their model tier.
    const oneSided = row.side === "Over" && row.underPrice === null;
    const displayEdge = oneSided ? row.altLegBacktestRoi ?? null : modelEdge;
    return {
      ...row,
      best: row.price === best.get([row.match, row.player, row.market, row.line, row.side].join("|")),
      modelProb, modelPrice: fairPrice(modelProb), modelEdge, oneSided, displayEdge, band: edgeBand(displayEdge),
      signal: row.betSignal === true && row.betSide === row.side,
      topLeg: oneSided && row.altLegTier === "top 5%",
    };
  });
}

export function OddsPanel() {
  const [data, setData] = useState<OddsPayload | null>(null);
  const [error, setError] = useState("");
  const [tab, setTab] = useState<"props" | "matches">("props");
  const [match, setMatch] = useState("all");
  const [market, setMarket] = useState("all");
  const [agency, setAgency] = useState("all");
  const [side, setSide] = useState("all");
  const [search, setSearch] = useState("");
  const [bestOnly, setBestOnly] = useState(false);
  const [edgeOnly, setEdgeOnly] = useState(false);
  const [page, setPage] = useState(0);
  const [propSort, setPropSort] = useState<SortState<PropSort>>({ key: "edge", direction: "desc" });
  const [h2hSort, setH2hSort] = useState<SortState<H2hSort>>({ key: "agency", direction: "asc" });
  const [totalSort, setTotalSort] = useState<SortState<TotalSort>>({ key: "agency", direction: "asc" });

  useEffect(() => {
    fetch("/data/nbl-odds.json")
      .then(response => { if (!response.ok) throw new Error("Odds export is unavailable"); return response.json(); })
      .then((payload: OddsPayload) => {
        if (!payload.metadata || !Array.isArray(payload.props) || !Array.isArray(payload.headToHead) || !Array.isArray(payload.totals)) throw new Error("Odds export is invalid");
        setData(payload);
      })
      .catch(cause => setError(cause instanceof Error ? cause.message : "Odds export is unavailable"));
  }, []);

  const rows = useMemo(() => allPropRows(data?.props ?? []), [data]);
  const matches = useMemo(() => [...new Set([...(data?.headToHead ?? []).map(row => row.match), ...rows.map(row => row.match)])].sort(), [data, rows]);
  const markets = useMemo(() => [...new Set(rows.map(row => row.market))].sort(), [rows]);
  const agencies = data?.metadata.agencies ?? [];
  const filteredRows = rows.filter(row =>
    (match === "all" || row.match === match) &&
    (market === "all" || row.market === market) &&
    (agency === "all" || row.agency === agency) &&
    (side === "all" || row.side === side) &&
    (!bestOnly || row.best) &&
    (!edgeOnly || (row.displayEdge !== null && row.displayEdge > 0)) &&
    (!search || row.player.toLowerCase().includes(search.trim().toLowerCase()))
  );
  const propValue = (row: PropRow): string | number | null => {
    switch (propSort.key) {
      case "player": return row.player;
      case "market": return row.market;
      case "selection": return row.line;
      case "agency": return row.agency;
      case "price": return row.price;
      case "model": return row.modelPrice;
      case "edge": return row.displayEdge;
      case "season": return row.hitRate;
      case "last10": return row.recentHitRate;
    }
  };
  const filtered = sortRows(filteredRows, propValue, propSort.direction);
  const pageSize = 50;
  const pageCount = Math.max(1, Math.ceil(filtered.length / pageSize));
  const visible = filtered.slice(Math.min(page, pageCount - 1) * pageSize, (Math.min(page, pageCount - 1) + 1) * pageSize);
  const changeFilter = (setter: (value: string) => void, value: string) => { setter(value); setPage(0); };
  const changeSort = <Key extends string>(setter: Dispatch<SetStateAction<SortState<Key>>>, key: NoInfer<Key>, initial: SortDirection) => {
    setter(previous => ({ key, direction: previous.key === key ? previous.direction === "asc" ? "desc" : "asc" : initial }));
    setPage(0);
  };
  const selectedMatches = matches.filter(value => match === "all" || match === value);
  const hasModel = rows.some(row => row.modelProb !== null);
  const signalsFrom = data?.metadata.modelSignalsFromWeek;
  const earlySeason = data?.metadata.modelSeasonWeek != null && signalsFrom != null && data.metadata.modelSeasonWeek < signalsFrom;

  if (error) return <div className="odds-status panel"><Activity /><h1>Odds are unavailable</h1><p>{error}. Run <code>Rscript Scripts/13-export-web-odds.R</code> after processing the scrapers.</p></div>;
  if (!data) return <div className="odds-status panel"><Activity /><h1>Loading odds</h1></div>;

  return <>
    <div className="page-title-row"><div><p className="eyebrow">{data.metadata.season.replace("-", "–")} season</p><h1>Odds workspace</h1><p className="page-detail">Compare NBL prices across the four confirmed agencies. Prices are a snapshot and may change.</p></div><div className="odds-snapshot">Exported {new Date(data.metadata.generatedAt).toLocaleString("en-AU", { dateStyle: "medium", timeStyle: "short" })}<small>{data.metadata.agencies.join(" · ")}</small>{data.metadata.modelGeneratedAt && <small>Model priced {new Date(data.metadata.modelGeneratedAt).toLocaleString("en-AU", { dateStyle: "medium", timeStyle: "short" })}</small>}</div></div>
    <div className="odds-tabs" role="tablist" aria-label="Odds views"><button role="tab" aria-selected={tab === "props"} className={tab === "props" ? "active" : ""} onClick={() => setTab("props")}>Player props</button><button role="tab" aria-selected={tab === "matches"} className={tab === "matches" ? "active" : ""} onClick={() => setTab("matches")}>Match odds</button></div>
    <div className="control-panel panel odds-controls"><label className="field"><span>Match</span><select value={match} onChange={event => changeFilter(setMatch, event.target.value)}><option value="all">All matches</option>{selectOptions(matches)}</select></label>{tab === "props" && <><label className="field"><span>Market</span><select value={market} onChange={event => changeFilter(setMarket, event.target.value)}><option value="all">All markets</option>{selectOptions(markets)}</select></label><label className="field"><span>Side</span><select value={side} onChange={event => changeFilter(setSide, event.target.value)}><option value="all">Over and under</option><option>Over</option><option>Under</option></select></label><label className="field"><span>Player</span><input value={search} onChange={event => changeFilter(setSearch, event.target.value)} placeholder="Search player"/></label></>}<label className="field"><span>Agency</span><select value={agency} onChange={event => changeFilter(setAgency, event.target.value)}><option value="all">All agencies</option>{selectOptions(agencies)}</select></label>{tab === "props" && <label className="odds-check"><input type="checkbox" checked={bestOnly} onChange={event => { setBestOnly(event.target.checked); setPage(0); }}/> Best price only</label>}{tab === "props" && hasModel && <label className="odds-check"><input type="checkbox" checked={edgeOnly} onChange={event => { setEdgeOnly(event.target.checked); setPage(0); }}/> Positive model edge only</label>}</div>
    {tab === "props" ? <section className="panel data-panel"><div className="section-heading"><div><h2>Player markets</h2><p>{filtered.length} prices match these filters · click a column heading to sort</p></div><span>Hit rates: current season / last 10 games</span></div>{hasModel && <div className="edge-legend" aria-label="Model edge colour key"><span className="edge-legend-title">Model edge</span>{(Object.keys(bandLabels) as EdgeBand[]).map(band => <span key={band} className={`edge-chip edge-${band}`}>{bandLabels[band]}</span>)}{earlySeason && <p className="edge-warning" role="note">Season week {data.metadata.modelSeasonWeek}: early in a season the model runs about one unit low, which inflates under edges. Bet signals are paused until week {data.metadata.modelSignalsFromWeek}.</p>}<span className="edge-legend-note">Two-way lines: edge = model probability × price − 1. One-sided X+ lines: backtested return of the line&apos;s model tier (raw model edge shown below it).</span></div>}<div className="table-wrap"><table><thead><tr><SortHeader label="Player / match" active={propSort.key === "player"} direction={propSort.direction} onClick={() => changeSort(setPropSort, "player", "asc")}/><SortHeader label="Market" active={propSort.key === "market"} direction={propSort.direction} onClick={() => changeSort(setPropSort, "market", "asc")}/><SortHeader label="Line" active={propSort.key === "selection"} direction={propSort.direction} onClick={() => changeSort(setPropSort, "selection", "asc")}/><SortHeader label="Agency" active={propSort.key === "agency"} direction={propSort.direction} onClick={() => changeSort(setPropSort, "agency", "asc")}/><SortHeader label="Price" active={propSort.key === "price"} direction={propSort.direction} onClick={() => changeSort(setPropSort, "price", "desc")}/>{hasModel && <><SortHeader label="Model" active={propSort.key === "model"} direction={propSort.direction} onClick={() => changeSort(setPropSort, "model", "asc")}/><SortHeader label="Edge" active={propSort.key === "edge"} direction={propSort.direction} onClick={() => changeSort(setPropSort, "edge", "desc")}/></>}<SortHeader label="Season" active={propSort.key === "season"} direction={propSort.direction} onClick={() => changeSort(setPropSort, "season", "desc")}/><SortHeader label="Last 10" active={propSort.key === "last10"} direction={propSort.direction} onClick={() => changeSort(setPropSort, "last10", "desc")}/></tr></thead><tbody>{visible.map((row, index) => <tr key={`${row.match}-${row.player}-${row.market}-${row.line}-${row.side}-${row.agency}-${index}`}><td><strong>{row.player}</strong><small>{row.team} · {row.match}</small></td><td>{row.market}</td><td>{row.side} {row.line}</td><td>{row.agency}</td><td><strong>{priceText(row.price)}</strong>{row.best && <small>Best available</small>}</td>{hasModel && <><td className={row.band ? `edge-cell edge-${row.band}` : ""}><strong>{priceText(row.modelPrice)}</strong>{row.modelProb !== null && <small>{percentText(row.modelProb)} · proj {row.modelMean !== null && row.modelMean !== undefined ? row.modelMean.toFixed(1) : "—"}</small>}</td><td className={row.band ? `edge-cell edge-${row.band}` : ""}><strong>{edgeText(row.displayEdge)}</strong>{row.signal ? <small className="edge-flag">Bet signal</small> : row.topLeg ? <small className="edge-flag">Top 5% SGM leg</small> : null}{row.oneSided && row.modelEdge !== null && <small>X+ tier · model {edgeText(row.modelEdge)}</small>}</td></>}<td>{percentText(row.hitRate)}<small>{row.gamesCurrent} games</small></td><td>{percentText(row.recentHitRate)}</td></tr>)}</tbody></table>{!filtered.length && <div className="empty-state"><Activity/><strong>No prices match these filters</strong><span>Try another market, match or agency.</span></div>}</div>{filtered.length > pageSize && <div className="odds-pagination"><button disabled={page === 0} onClick={() => setPage(value => value - 1)}>Previous</button><span>Page {Math.min(page, pageCount - 1) + 1} of {pageCount}</span><button disabled={page >= pageCount - 1} onClick={() => setPage(value => value + 1)}>Next</button></div>}</section> : <div className="odds-match-list">{selectedMatches.map(selectedMatch => { const h2h = sortRows(data.headToHead.filter(row => row.match === selectedMatch && (agency === "all" || row.agency === agency)), row => h2hSort.key === "agency" ? row.agency : h2hSort.key === "home" ? row.homePrice : row.awayPrice, h2hSort.direction); const totals = sortRows(data.totals.filter(row => row.match === selectedMatch && (agency === "all" || row.agency === agency)), row => totalSort.key === "agency" ? row.agency : totalSort.key === "line" ? row.line : totalSort.key === "over" ? row.overPrice : row.underPrice, totalSort.direction); return <section key={selectedMatch} className="panel data-panel"><div className="section-heading"><div><h2>{selectedMatch}</h2><p>Head-to-head and match total prices</p></div></div><div className="table-wrap"><table><thead><tr><SortHeader label="Agency" active={h2hSort.key === "agency"} direction={h2hSort.direction} onClick={() => changeSort(setH2hSort, "agency", "asc")}/><SortHeader label={h2h[0]?.homeTeam ?? "Home"} active={h2hSort.key === "home"} direction={h2hSort.direction} onClick={() => changeSort(setH2hSort, "home", "desc")}/><SortHeader label={h2h[0]?.awayTeam ?? "Away"} active={h2hSort.key === "away"} direction={h2hSort.direction} onClick={() => changeSort(setH2hSort, "away", "desc")}/></tr></thead><tbody>{h2h.map(row => <tr key={`${selectedMatch}-${row.agency}`}><td>{row.agency}</td><td>{priceText(row.homePrice)}</td><td>{priceText(row.awayPrice)}</td></tr>)}</tbody></table>{!h2h.length && <p className="odds-empty">No head-to-head price from this agency.</p>}</div>{totals.length > 0 && <div className="table-wrap odds-totals"><table><thead><tr><SortHeader label="Agency" active={totalSort.key === "agency"} direction={totalSort.direction} onClick={() => changeSort(setTotalSort, "agency", "asc")}/><SortHeader label="Total line" active={totalSort.key === "line"} direction={totalSort.direction} onClick={() => changeSort(setTotalSort, "line", "asc")}/><SortHeader label="Over" active={totalSort.key === "over"} direction={totalSort.direction} onClick={() => changeSort(setTotalSort, "over", "desc")}/><SortHeader label="Under" active={totalSort.key === "under"} direction={totalSort.direction} onClick={() => changeSort(setTotalSort, "under", "desc")}/></tr></thead><tbody>{totals.map(row => <tr key={`${selectedMatch}-${row.agency}-${row.line}`}><td>{row.agency}</td><td>{row.line}</td><td>{priceText(row.overPrice)}</td><td>{priceText(row.underPrice)}</td></tr>)}</tbody></table></div>}</section>; })}</div>}
  </>;
}
