# Evaluate the walk-forward backtest ---------------------------------------
#
# Two-way lines: model vs de-vigged market consensus (accuracy, calibration).
# One-sided X+ lines: model vs the book's implied probability.
# Market-conditioned blends are fitted on 2024-25 and tested out of sample on
# 2025-26, then refitted on every season to produce the live probability maps.
#
# Betting results are compared with blind baselines (every over / every under
# in the same season): the market's over/under bias swung between seasons, so
# raw ROI alone mostly measures which side a strategy leans to.

source("Scripts/00-config.R")
source("Models/props/R/prop_model_functions.R")

output_dir <- file.path(project_root, "Models", "props", "output")
backtest_dir <- file.path(output_dir, "backtest")
train_season <- "2024-2025"
test_season <- "2025-2026"
min_blend_rows <- 300L

log_loss <- function(p, y) {
  p <- pmin(pmax(p, 0.001), 0.999)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}
round_all <- function(d, digits = 3) mutate(d, across(where(is.double), ~ round(.x, digits)))

lines <- readRDS(file.path(backtest_dir, "backtest_lines.rds")) |>
  filter(outcome != line, !is.na(model_p_over)) |>
  mutate(
    over_hit = as.integer(outcome > line),
    two_way = !is.na(under_price),
    book_p_over = if_else(two_way, devig_two_way(over_price, under_price), NA_real_),
    book_implied = 1 / over_price,
    over_profit = if_else(over_hit == 1, over_price - 1, -1),
    under_profit = if_else(over_hit == 0, under_price - 1, -1)
  ) |>
  group_by(match_id, player_name, stat, line) |>
  mutate(market_p_over = if (any(two_way)) mean(book_p_over, na.rm = TRUE) else NA_real_) |>
  group_by(season) |>
  mutate(week = season_week(match_date, min(match_date))) |>
  ungroup()

two_way <- lines |> filter(two_way)
one_sided <- lines |> filter(!two_way)
two_way_props <- two_way |> distinct(match_id, player_name, stat, line, .keep_all = TRUE)

# Accuracy -------------------------------------------------------------------

cat("\n== Two-way lines: model vs de-vigged market consensus (log loss, lower is better) ==\n")
two_way_props |>
  group_by(season, stat) |>
  summarise(
    n = n(),
    model = log_loss(model_p_over, over_hit),
    market = log_loss(market_p_over, over_hit),
    model_mae = mean(abs(model_mean - outcome)),
    line_mae = mean(abs(line - outcome)),
    over_rate = mean(over_hit),
    market_over_p = mean(market_p_over),
    .groups = "drop"
  ) |>
  round_all() |>
  print(n = Inf)

cat("\n== One-sided lines: calibration of the raw model by book price band ==\n")
one_sided |>
  mutate(price_band = cut(over_price, c(1, 1.5, 2, 3, 6, 10, Inf))) |>
  group_by(price_band) |>
  summarise(n = n(), book_implied = mean(book_implied), model = mean(model_p_over),
            actual = mean(over_hit), blind_roi = mean(over_profit)) |>
  round_all() |>
  print()

# Blends -----------------------------------------------------------------------

fit_blend <- function(d, market_col) {
  d |>
    group_by(stat) |>
    filter(n() >= min_blend_rows) |>
    group_modify(function(x, key) {
      x$market_ref <- x[[market_col]]
      g <- glm(
        over_hit ~ blend_logit(model_p_over) + blend_logit(market_ref),
        family = binomial(), data = x
      )
      co <- summary(g)$coefficients
      tibble(
        intercept = co[1, 1], model_coef = co[2, 1], market_coef = co[3, 1],
        model_z = co[2, 3], n = nrow(x)
      )
    }) |>
    ungroup()
}

oos_maps <- list(
  two_way = fit_blend(two_way_props |> filter(season == train_season), "market_p_over"),
  one_sided = fit_blend(one_sided |> filter(season == train_season), "book_implied")
)
cat("\n== Blend weights fitted on ", train_season, " ==\n", sep = "")
print(map(oos_maps, round_all))

test <- lines |>
  filter(season == test_season) |>
  apply_probability_maps(oos_maps)

cat("\n== Out-of-sample log loss, ", test_season, " ==\n", sep = "")
test |>
  mutate(market_ref = if_else(two_way, market_p_over, book_implied)) |>
  group_by(two_way, stat) |>
  summarise(
    n = n(),
    model = log_loss(model_p_over, over_hit),
    market = log_loss(market_ref, over_hit),
    blend = log_loss(final_p_over, over_hit),
    .groups = "drop"
  ) |>
  round_all(4) |>
  print(n = Inf)

# Betting ----------------------------------------------------------------------

place_bets <- function(d, threshold, max_price = 6) {
  d |>
    mutate(
      ev_over = final_p_over * over_price - 1,
      ev_under = (1 - final_p_over) * under_price - 1,
      side = if_else(coalesce(ev_under, -Inf) > ev_over, "under", "over"),
      ev = pmax(ev_over, coalesce(ev_under, -Inf)),
      price = if_else(side == "over", over_price, under_price),
      profit = if_else(side == "over", over_profit, under_profit)
    ) |>
    filter(ev >= threshold, price <= max_price)
}

roi_ci <- function(profit, match_id) {
  if (length(profit) == 0) {
    return(tibble(bets = 0L, roi = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  }
  by_match <- split(profit, match_id)
  boot <- replicate(2000, mean(unlist(sample(by_match, length(by_match), replace = TRUE))))
  tibble(bets = length(profit), roi = mean(profit),
         ci_low = quantile(boot, 0.05), ci_high = quantile(boot, 0.95))
}

set.seed(1)
cat("\n== Out-of-sample betting, ", test_season, " (flat stakes, prices <= 6, 90% CI by match) ==\n", sep = "")
cat("Baselines - blind overs: two-way ", round(mean(test$over_profit[test$two_way]), 3),
    ", one-sided ", round(mean(test$over_profit[!test$two_way & test$over_price <= 6]), 3),
    "; blind unders (two-way) ", round(mean(test$under_profit[test$two_way]), 3), "\n", sep = "")
betting <- expand_grid(market = c("two-way", "one-sided"), threshold = c(0.02, 0.05, 0.10)) |>
  mutate(result = map2(market, threshold, function(m, thr) {
    bets <- test |> filter(two_way == (m == "two-way")) |> place_bets(thr)
    bind_cols(
      roi_ci(bets$profit, bets$match_id),
      bets |> summarise(over_share = mean(side == "over"),
                        over_roi = mean(profit[side == "over"]),
                        under_roi = mean(profit[side == "under"]))
    )
  })) |>
  unnest(result) |>
  round_all()
print(betting, width = Inf)

cat("\n== Two-way bets (EV >= 5%) by season phase, ", test_season, " ==\n", sep = "")
test |>
  filter(two_way) |>
  place_bets(0.05) |>
  mutate(phase = if_else(week < min_season_week_for_bets, "early (weeks 1-6)", "week 7+")) |>
  group_by(phase) |>
  group_modify(~ roi_ci(.x$profit, .x$match_id)) |>
  ungroup() |>
  round_all() |>
  print()

# Live probability maps ------------------------------------------------------

# Alt-leg tiers for SGM building. One-sided lines are overs-only with heavy
# margins, so the aim is ranking legs from least to most overpriced. Ranking
# on the raw model's EV (out of sample in both seasons) beat the per-line
# blend and main-line anchoring for stability across seasons.
alt_legs <- one_sided |>
  filter(over_price <= max_alt_leg_price) |>
  mutate(model_ev = model_p_over * over_price - 1)
tier_cutoffs <- quantile(alt_legs$model_ev, c(0.80, 0.90, 0.95))
alt_leg_tiers <- alt_legs |>
  mutate(tier = assign_alt_leg_tier(model_ev, tier_cutoffs)) |>
  bind_rows(one_sided |> filter(over_price > max_alt_leg_price) |> mutate(tier = long_shot_tier)) |>
  group_by(season, tier) |>
  summarise(legs = n(), roi = mean(over_profit), .groups = "drop") |>
  pivot_wider(names_from = season, values_from = c(legs, roi))
cat("\n== Alt-leg tiers (one-sided, price <= ", max_alt_leg_price, "): backtest ROI by raw-model EV tier ==\n", sep = "")
cat("EV cut-offs:", paste(names(tier_cutoffs), round(tier_cutoffs, 3), collapse = ", "), "\n")
print(round_all(alt_leg_tiers))

probability_maps <- list(
  two_way = fit_blend(two_way_props, "market_p_over"),
  one_sided = fit_blend(one_sided, "book_implied"),
  alt_leg_cutoffs = tier_cutoffs,
  alt_leg_tiers = alt_leg_tiers,
  fitted_on = sort(unique(lines$season))
)
cat("\n== Probability maps for live pricing (all seasons) ==\n")
print(map(probability_maps[c("two_way", "one_sided")], round_all))
saveRDS(probability_maps, file.path(output_dir, "probability_maps.rds"))
saveRDS(list(test = test, betting = betting, oos_maps = oos_maps),
        file.path(backtest_dir, "backtest_evaluation.rds"))
