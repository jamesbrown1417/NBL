# Player prop pricing model

Prices NBL player props (points, rebounds, assists, threes, PRA, steals, blocks)
from box-score history and compares fair prices with scraped bookmaker odds.

Run from the project root, in order:

1. `Rscript Models/props/01-backtest.R`: walk-forward backtest. Refits weekly on
   games before each week and prices every archived line in
   `Historical Performance/datasets`, and scores the full predicted
   distribution of every player in those matches (about 4 minutes).
2. `Rscript Models/props/02-evaluate-backtest.R`: accuracy and betting results
   compared with the market, plus the model/market blend weights
   (`output/probability_maps.rds`) used for live pricing.
3. `Rscript Models/props/03-price-current-markets.R`: prices the markets in
   `Data/processed/odds` and writes `output/current_prices.{rds,csv}`.

4. `Rscript Models/props/04-compare-backtests.R <reference> <variant> ...`:
   paired, match-clustered comparison of backtest variants, for testing
   features. Produce variants with `PROP_FEATURES=a,b` (or `none`) and
   `PROP_BACKTEST_TAG=_name` on step 1; `default` names the untagged run.

Team news: list ruled-out players in `Data/reference/player_outs.csv` (column
`player_name`) before step 3. They are dropped from pricing and count as
absent rotation minutes for their teammates.

Steps 1-2 only need rerunning when the odds history grows (e.g. weekly); step 3
runs whenever odds are scraped.

## Model

- **Minutes**: regression on each player's prior minutes (season to date, last
  1 and 3 games, starter share, previous season), with empirical residuals by
  projected-minutes band. Conditional on playing: DNPs void props.
- **Per-minute rates**: hierarchical negative binomial per stat (Poisson where
  no overdispersion is found) with partially pooled player, player-season,
  team-season and opponent-season effects over the last four seasons, fitted by
  Laplace approximation (`glmmTMB`). Predictions draw from the approximate
  posterior, so players with little data carry wider uncertainty and debutants
  fall back to the population prior.
- **Features** (`settings$features`, switchable for ablation):
  - `teammate_absence` (on): season-average minutes of absent rotation
    teammates, in the minutes and rate models.
  - `line_minutes` (on): debut players' minutes implied by their consensus
    points line, fitted on earlier players with lines.
  - `opp_position`, `rest_travel` (off): opponent-by-position effects, and
    rest days plus Perth/NZ travel. No gain in the backtest.
- **Simulation**: 4,000 joint draws of minutes then counts per player. PRA sums
  the same draws, so it shares the minutes correlation.
- **Market conditioning**: final probabilities are a per-stat logistic blend
  of the model with the de-vigged consensus (two-way lines) or the book's own
  implied probability (one-sided X+ lines), with weights fitted on the backtest.

## Backtest findings (2024-25 and 2025-26 odds history)

- The model beats the de-vigged market consensus on its own for points,
  rebounds, assists and threes in 2024-25, and for points and threes in
  2025-26. It is well calibrated across whole distributions (checked on 150k
  alternate lines). Its blend weights are significant for every stat except PRA.
- Books shade props toward the over in both seasons: overs hit 44% and 43%
  against a market-implied 49% (blind unders +1.0% and +4.3%). Judge ROI
  against blind baselines.
- Two-way lines, out of sample in 2025-26, EV >= 10%: +10.7% ROI (90% CI
  +7.0% to +14.7%). About 90% of these bets are unders: +11.8% vs +4.3% blind.
  Overs 0% vs -19.7% blind.
- One-sided X+ lines: the model improves on blind betting but does not
  overcome their margins, so they are never flagged as single bets. For SGM
  legs, `alt_leg_tier` ranks them by raw-model EV (price <= 6). The top-5%
  tier averaged -2.0% (2024-25) and -4.2% (2025-26) as singles, against
  -38% and -30% for the rest. Anchoring alt ladders to the main-line market
  consensus was tested and rejected: it imports the books' over-shading.
- Early season the model runs about one unit low. Accuracy against the market
  was mixed (better in 2024-25, worse in 2025-26), so `bet_signal` is
  suppressed until week 7 as a conservative default
  (`min_season_week_for_bets`).

## Joint model for same game multis (phase 1)

Scripts (run after 01-02):

5. `05-joint-residuals.R`: monthly walk-forward from 2021-22. Records where each
   player-game-stat landed within its predictive distribution (randomised PIT),
   about 4 minutes.
6. `06-estimate-correlations.R`: pooled residual correlations with
   match-clustered SEs, corrected for count discreteness. Writes
   `output/joint/correlation_params.rds` (`pre_test` excludes 2025-26; `all`
   is for live use).
7. `07-backtest-sgm.R`: out-of-sample SGM backtest on archived legs
   (`PROP_SGM_TEST_SEASON`, default 2025-26), about 5 minutes.

Method (`R/joint_functions.R`): each player's single-leg distribution is kept
exactly; a t-copula (8 df) reorders the simulated draws so legs become jointly
dependent. Correlations are pooled by relationship: same player, teammates by
usual role (starter/bench), and opponents.

Findings:

- Same-player dependence is strong (latent correlations: points-threes +0.73,
  points-rebounds +0.35, points-assists +0.23). Cross-player correlations are
  small but sensibly signed: teammates' points -0.04 to -0.06, a starter's
  assists with teammates' threes +0.08, opponents about 0.
- Pricing legs as independent is badly wrong for same-player combinations.
  Same-direction pairs hit 1.38x as often as independence predicts, and
  under + alt-over pairs only 0.79x. The copula brings both to within about
  1.00-1.14x. It improves log loss significantly on 2-leg combinations.
- Cross-player correlations add no measurable accuracy over same-player-only
  dependence. They are kept because they are small and correctly signed, but
  they are unproven.
- A Gaussian copula under-priced longshot combinations (< 1% joint
  probability). The t-copula fixes most of this.
- Combinations still hit about 5-10% more often than predicted, including
  under independence. That points to single-leg tail calibration on
  alternate lines, not dependence: the model under-predicts short-priced
  overs.

## Known gaps

- Team news comes only from `player_outs.csv`. In the backtest, absences are
  known from the box score, so live results depend on keeping that file current.
- Minutes remain the largest error source.
- Team and opponent pace come only from season-level random effects. Match
  spread and total from the H2H and total markets are not used yet.
- Role changes within a season (e.g. a player's usage jumping) are slow to
  register.
- Only one out-of-sample season, and settings were chosen on the same two
  seasons. Treat the ROI figures as indicative; 2026-27 is the true forward test.
