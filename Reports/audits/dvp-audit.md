# DVP audit — 7 September 2026

## Verdict

The opponent-excluded player baseline is a meaningful descriptive statistic. It partly controls for the calibre of the players a defence faced and separates playing time from production rate. Neither implementation is yet a validated predictive defensive adjustment. Reliability problems include duplicate game records in the R input, misleading sample counts, incomplete/different position coverage, small-sample leverage, and unmodelled pace/role differences.

This audit examined the R source, the current web implementation, the saved 2025–2026 data, weighting and inclusion sensitivity, and a limited chronological points-per-36 diagnostic. Application and model code were not changed by the audit.

## What each implementation measures

The R implementation first converts each eligible appearance to a per-36 rate, takes a simple mean against a selected opponent and a simple mean against all other opponents, subtracts the two, then averages the differences across player/team/position groups. Despite the `career_avgs` name, this is selected-season data, not career data.

The web implementation uses 36 × total stat / total minutes within each player's opponent and other-opponent samples, then equally averages player differences. It groups by player name and recorded broad position, merging team stints. Both require at least one appearance on each side; neither applies precision weighting to the final player differences.

These implementations are intentionally described separately: the web implementation added in the preceding work did not reproduce the original estimator exactly. Differences extend beyond display: position source, weighting, cohort, duplicate handling, and team-stint grouping all differ.

## Findings

### 1. Duplicate appearances bias the original calculation

In the raw canonical 2025–2026 season there are 4,353 rows but only 4,230 unique match/player-name keys: 123 duplicate keys. Points and minutes agree within all these duplicate keys. Among appearances of at least five minutes, deduplication reduces the count from 3,325 to 3,229.

The R DVP script does not deduplicate before aggregation. Repeated games therefore receive additional weight and inflate appearance counts. The web exporter does deduplicate; its rounded minutes admit two additional appearances at the five-minute boundary, giving 3,231 eligible web rows.

Recommendation: establish one record per player ID and match ID before calculation; detect conflicts explicitly rather than silently selecting an arbitrary record. Preserve seconds/minutes precision for eligibility and calculations, rounding for display only.

### 2. The R minimum-games control is not counting games

The final `games = n()` counts player/team/position comparison rows. It does not count distinct matches or appearances. `min_games = 2` consequently requires two comparison rows, not two games against each player. In the reconstructed original points calculation, 207 of 1,047 player-team-opponent comparisons had only one appearance against the opponent.

The web displays distinct matches and players separately, but has no minimum-evidence threshold: one qualifying player can produce a coloured score. Its corresponding single-appearance count was 218 of 941 comparisons.

Recommendation: retain separate numbers of distinct matches, appearances, players, target minutes and baseline minutes. Define minimum evidence separately for the target and baseline. Suppress or flag insufficient evidence and display uncertainty; calibrate thresholds against chronological validation.

### 3. Position coverage materially changes the sample

The original Supercoach name/team inner join retains 3,095 of 3,325 eligible raw rows, dropping 230 (6.9%) and reducing distinct player names from 149 to 137. Omitted players include John Brown, Karim Lopez, Quentin Peterson and Rob Baker. There were no duplicate Supercoach join keys in this snapshot.

The web uses recorded box-score positions instead. Of 3,231 eligible exported appearances, 582 (18.0%) have null positions and do not contribute to C/F/G heatmaps. Forty-two players have more than one mapped position category across their eligible season appearances; this count includes changes between a known position and Unknown. One player, Hunter Maldonado, has multiple team stints.

Recommendation: use a single season-aware position mapping and stable player identity, record fallback provenance and coverage, and avoid interpreting fantasy eligibility as who actually defended a player. Treat hybrid positions consistently. Do not silently fall back from an unknown position to centre in the matchup selector, as the current web selector does on player change.

### 4. Equal averaging makes sparse player comparisons influential

Original R per-appearance averaging gives a five-minute appearance as much weight as a 30-minute appearance. For example, 4 points in 5 minutes and 12 in 30 produce a mean game rate of 21.6 per 36, versus a pooled rate of 16.46 per 36. Both describe different quantities; the latter is usually the clearer pooled scoring-rate estimate.

On the same original cohort, changing only within-player weighting changed Melbourne United guards' points DVP from −1.20 to −1.74 and Brisbane forwards from +2.48 to +1.96. None of the 30 points cells changed sign in that particular comparison. This is not the largest source of instability found.

The web already pools minutes within a player, but still gives each player's difference equal weight regardless of target and baseline exposure. Removing Harry Froling's comparison changes South East Melbourne centres' points DVP from −0.44 to −2.33 per 36. This is an influence diagnostic, not justification for excluding him.

Raising the appearance threshold from 5 to 15 minutes changes the sign of 7 of 30 web points cells. Examples:

| Team / group | Five-minute threshold | Fifteen-minute threshold |
|---|---:|---:|
| Perth / forwards | −3.12 | −0.46 |
| Illawarra / forwards | −0.30 | +2.09 |
| Brisbane / forwards | +1.84 | −0.21 |
| Cairns / centres | +5.97 | +3.60 |

This changes the population being measured, so it does not prove that 15 minutes is correct. Simply raising the threshold can exclude meaningful foul-trouble or rotation outcomes.

Recommendation: use exposure/precision-aware estimates and partial pooling toward a neutral effect, with the amount of shrinkage learned from validation or a hierarchical model. Account for both target and baseline uncertainty. Report the raw estimate alongside the regularised estimate. Games and repeated players are correlated; do not treat every comparison as an independent observation when constructing intervals.

### 5. Production allowed is not isolated defensive ability

Per-36 normalisation does not remove pace, venue, teammate availability, role changes, opponent schedule mix, or strategic matchups. A fast team may permit more production per minute without being worse per possession. A player can have a different role in the games against one team than in their other games. Current position labels cannot establish the identity or position of the actual defender.

Recommendation: distinguish a per-minute matchup environment from defensive efficiency per possession. For a predictive model, consider stat-appropriate count models with minutes/exposure offsets and player, team, position and opponent effects; add pace, venue and role covariates supported by the data. Use actual on-court possessions only when available; team pace is an approximation. Estimate minutes/role separately when projecting a full-game prop.

### 6. Historical comparisons are not prediction validation

Both calculations use all selected-season observations for player baselines and opponent effects. That is valid for a season summary, but using these same values to explain or backtest games earlier in that season would use future information. Removing the last N games separately for each player does not establish a common historical information cutoff.

A limited chronological diagnostic was run on 2025–2026 exported web data:

- Train only on dates strictly before the target date.
- Start after 60 distinct training matches.
- Require at least five eligible prior appearances for the target player, a known position and at least three DVP comparison players.
- Compare the player's historical pooled points-per-36 baseline with that baseline plus the raw web DVP difference.
- Score the same 1,693 eligible future appearances under both methods.

| Error in points per 36 | Player baseline | Baseline + raw DVP |
|---|---:|---:|
| Mean absolute error | 6.698 | 6.717 |
| Root mean squared error | 8.656 | 8.706 |

The raw additive adjustment did not improve this diagnostic. This does not establish that DVP has no predictive value: it is one season, one stat and a particular uncalibrated additive application. No significance test, interval, line-level probability calibration or betting-return test was performed. Actual target minutes are used to define the observed per-36 outcome; this is not a pregame points-prop forecast. The original R estimator was not tested predictively here.

Recommendation: run date-cutoff walk-forward tests across seasons and each stat against strong minutes/role-aware baselines. Learn the DVP contribution on training data only, and compare baseline, raw adjustment and regularised adjustment on held-out data. For prop probabilities, assess calibration and probability scoring as well as count prediction error. Use only data and position/availability mappings that were available before each test game.

### 7. Two implementation edge cases need correction

R sums ignore missing values while `total_games` and `games_vs` count all rows. A missing other-opponent observation can therefore reduce the denominator-adjusted baseline as though it were zero. The audited joined season had no missing values in the six primary component stats, so this is a latent rather than observed error in this snapshot. Use stat-specific valid observations for sums and denominators.

The web matchup page offers DVP for every selectable metric, including minutes. Minutes per 36 are identically 36, so minutes DVP is always zero for qualifying comparisons and has no meaning. Positive turnover or foul differences also do not have the same favourable interpretation as positive points or rebounds. Restrict supported DVP metrics and define stat-specific interpretation.

## Suggested order of work

1. Create one canonical, versioned estimator shared by the R export and web display. Deduplicate, use precise minutes, unify positions and identity, and fix valid-count denominators and unsupported metrics.
2. Surface coverage and actual exposure, and add uncertainty/insufficient-evidence treatment to the heatmap.
3. Add shrinkage and separate pace/context from the defensive effect; preserve the descriptive raw calculation for comparison.
4. Validate chronologically across seasons and stats before using DVP to alter projections or fair odds.

## Supporting methodological references

- Stan's [partial-pooling case study](https://mc-stan.org/learn-stan/case-studies/pool-binary-trials.html) explains why sparse group estimates benefit from pooling. The example uses binary outcomes; the DVP model would require a likelihood appropriate to the selected stat.
- Scikit-learn's [TimeSeriesSplit documentation](https://scikit-learn.org/stable/modules/generated/sklearn.model_selection.TimeSeriesSplit.html) explains why validation must avoid training on future data and testing on the past.

Numerical sensitivity results are saved in `dvp-audit-results.json`. `dvp_audit.py` contains the diagnostic calculation; its original-R cohort input is prepared by `prepare-dvp-audit.R`. Run both from the repository root, in that order. These scripts read source data and write audit artifacts only; they do not refresh or overwrite the production datasets.
