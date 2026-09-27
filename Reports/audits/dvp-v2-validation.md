# DVP v2 validation and implementation

The web display and R compatibility exports now consume the same versioned estimator in `web/app/dvp.ts`. No DVP correction is applied to projections, historical hit rates or fair odds.

## Changes

- Identical match/player records are removed; conflicting records fail the export before the previous published manifest is replaced.
- Minute precision is retained; eligibility uses five actual minutes, not a rounded display value.
- Position names are normalised in one place. Missing values may use an earlier recorded position in the same season and team, never a future or same-date appearance. Unknowns remain visible. No centre fallback.
- Baselines remain separate for each player/team stint and exclude the target opponent. No missing stat is treated as zero.
- The pooled stat/exposure rate is calculated on each side. Player differences are combined with harmonic exposure weights: target exposure × baseline exposure / (target + baseline exposure). Both target and baseline evidence determine the weight.
- Raw estimates, exposure counts, contributor weights, effective player counts and approximate raw 95% intervals are retained.
- Uncertainty uses a game-cluster delta approximation, combining each match’s target and baseline influences before estimating variance. It is not a calibrated prediction interval and does not fully model repeated-player dependence between games.
- Empirical-Bayes regularisation estimates a zero-centred prior second moment separately for each season/stat/position/basis, subtracting estimated sampling variance and flooring at zero. A zero prior variance can legitimately shrink an entire group to neutral. Prior estimation uses only evidence-qualified teams and requires at least three.
- Evidence gates are explicit provisional operational safeguards: two target and five baseline games plus 20/60 minutes per player/team comparison; five target games, three effective players and 120/360 minutes for the aggregate. Inadequate aggregates have no regularised value.
- The per-100 view uses estimated player possessions (minutes × team pace / 40). Missing source pace is estimated from both team box scores using FGA − ORB + TOV + 0.44 × FTA and overtime duration. It is not measured player on-court exposure.
- Unsupported metrics such as minutes no longer receive a DVP value.
- Season-specific content-addressed snapshots are fetched on demand rather than bundling all historical DVP detail into the main statistics download.

## Chronological diagnostic

2023–24, 2024–25 and 2025–26, seven stats, 4,870 matched eligible appearances per stat. Each test date uses strictly earlier dates for positions, baselines, DVP and shrinkage. Training starts after 60 distinct matches. The optional calibrated coefficient uses only previously scored dates, is constrained to [0,1], and is diagnostic only.

The baseline is the player’s pooled rate against other opponents in the same team stint and position, requiring at least five prior appearances. All four methods are scored on identical observations with an available evidence-qualified DVP estimate. Rates are conditioned on observed target minutes of at least five; this is not a pregame prop forecast.

| Stat | Baseline RMSE | + Raw DVP | + Regularized DVP | + Chronologically calibrated DVP |
|---|---:|---:|---:|---:|
| points | 8.5629 | 8.5834 | 8.5509 | 8.5526 |
| rebounds | 3.9042 | 3.9299 | 3.8970 | 3.8946 |
| assists | 2.5706 | 2.5782 | 2.5669 | 2.5681 |
| threes | 1.8870 | 1.8939 | 1.8843 | 1.8850 |
| steals | 1.5641 | 1.5705 | 1.5622 | 1.5622 |
| blocks | 1.2402 | 1.2512 | 1.2399 | 1.2402 |
| pra | 9.9648 | 9.9847 | 9.9406 | 9.9443 |

Raw DVP is generally noisier than the regularised version. Improvements over the baseline are small and not uniform across seasons, metrics or error measures. There has been no significance testing, interval calibration, odds comparison or profitability evaluation. These results do not justify automatic projection adjustments.

Current historical source revisions are used because point-in-time source archives are unavailable. Role, venue and teammate availability are not controlled. Position coverage and minimum exposure requirements select a subset of games. Evidence thresholds were not tuned on these results. A future probabilistic model should model minutes/role and validate both count distributions and line-probability calibration using additional held-out seasons.

## Current coverage

For 2025–26, 123 identical duplicate player/game records are removed. There are 3,229 appearances of at least five precise minutes. Causal position carry-forward resolves 19 previously missing assignments; 563 remain unknown (17.4%). The app reports this remaining limitation instead of manufacturing historical assignments from the current fantasy roster.

## Running the workflow

- `Rscript Scripts/12-export-web-stats.R` produces ordinary stats and canonical DVP snapshots without scheduling or fetching upstream data.
- `Rscript Scripts/06-defence-vs-position.R` also produces the legacy RDS bundles from the same snapshot. `games` now means distinct target matches. `avg_*` contains the regularised result and `raw_value` preserves the descriptive result; insufficient estimates are NA. The source season is explicit; `NBL_DVP_SEASON` selects a requested historical season.
- `node --experimental-strip-types Scripts/validate-dvp.mjs` reproduces the validation; optional season arguments override the three defaults.
- `npm test` from `web` checks calculations, cutoffs, duplicates, missingness, uncertainty, grouping, snapshot parity and rendered views.
- `Rscript tests/testthat.R` additionally verifies R/web value and sample-count parity.

Node 22.13 or newer is required for the shared estimator. Stable source player IDs are not currently available: normalised names remain provisional identities, with team-stint separation and conflict detection. Future source IDs should replace these keys without silently merging names.
