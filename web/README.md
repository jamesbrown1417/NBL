# NBL Analytics web app

An NBL player and team performance workspace backed by the repository's canonical combined stats table. The Odds workspace displays processed prices from BetRight, Pointsbet, Sportsbet and TAB for the active season.

## Refresh the data

From the NBL repository root:

```bash
Rscript Scripts/12-export-web-stats.R
Rscript Scripts/13-export-web-odds.R
```

These rebuild `web/public/data/nbl-stats.json` from the combined stats table and `web/public/data/nbl-odds.json` from the processed odds files. Run the scrapers and master processing script before exporting odds. The regular shell and PowerShell update scripts refresh both web exports after processing.

## Model prices and edges

The export joins model prices from `Models/props/output/current_prices.rds` (written by `Rscript Models/props/03-price-current-markets.R`, which the update scripts run before the odds export). The Player props table adds:

- **Model**: the fair price, with the model's probability and projected stat.
- **Edge**: for two-way lines, model probability × price − 1. For one-sided X+ lines, the backtested return of the line's model tier, with the raw model edge beneath it. Raw model edges on X+ lines did not hold up in the backtest.

Edge cells are coloured in bands (+10%, +3%, −3%, −15%). "Bet signal" marks two-way lines the pricing script flags, and "Top 5% SGM leg" marks the best-ranked X+ legs. Early in a season a banner notes that bet signals are paused. If model pricing fails, the export still runs without model columns.

## Run locally

```bash
cd web
npm install
npm run dev
```

Use `npm run lint` and `npm test` to verify the production build and server-rendered shell.

## Research workspace

Player and Team Labs include opportunity, efficiency, scoring sources and opposing box scores. Shooting matchups describe three-point volume allowed, not shot openness. The matchup workspace includes recent, venue and role filters, distributions and head-to-head context. Full-game stats include overtime; quarter and half scores exclude it.

## Canonical DVP v2

DVP is calculated once by `app/dvp.ts`, invoked by the R export through `Scripts/export-dvp.mjs`. Both RDS consumers and the web use these exact snapshots; the browser has no independent estimator. The statistics payload contains a manifest of content-addressed per-season DVP files, loaded on demand.

The model deduplicates player/game keys, rejects conflicts, preserves minute precision, separates team stints and uses only earlier same-season/team records for position fallbacks. It combines other-opponent baseline differences using exposure weights on both sides, reports game-cluster approximate raw intervals and shrinks uncertain effects toward neutral. Explicit sample gates withhold unsupported regularized estimates. Unknown positions, missing inputs, exposure and effective player counts remain visible.

Users can compare per-36 production with production per 100 estimated possessions. Neither view isolates a causal defensive effect. Pace fallback uses both teams' box scores and overtime duration. Availability, role changes and true on-court tracking are not modelled. DVP never adjusts projections or odds automatically.

See `../Reports/audits/dvp-v2-validation.md` for formulas, provisional thresholds, three-season validation and limitations. To regenerate R compatibility bundles, run `Rscript Scripts/06-defence-vs-position.R` from the repository root. This reads local source data only; it does not manage upstream data updates. Node >=22.13 is required.

Run `npm test` for the build, estimator tests and real-data rendering checks, and `Rscript tests/testthat.R` from the repository root for R/web contract parity.
