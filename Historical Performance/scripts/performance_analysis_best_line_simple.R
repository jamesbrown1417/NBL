#!/usr/bin/env Rscript

# Minimal best-line analysis focusing on per-leg profit only.
# For each (snapshot_date, match, player_name, market_name):
# - Under: choose highest line then highest under_price; get complementary best over_price at that line;
#          compute dutching stakes and return UNDER LEG profit only.
# - Over:  choose lowest line then highest over_price; get complementary best under_price at that line;
#          compute dutching stakes and return OVER LEG profit only.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

input_path <- "Historical Performance/all_processed_odds_lines.rds"
stopifnot(file.exists(input_path))

df <- readr::read_rds(input_path)

# Keep required fields
df <- df %>% select(snapshot_date, match, player_name, market_name, line, over_price, under_price, stat)

# Complementary best prices at each exact line within group
line_best_prices <- df %>%
  group_by(snapshot_date, match, player_name, market_name, line) %>%
  summarise(
    comp_over_price = max(over_price, na.rm = TRUE),
    comp_under_price = max(under_price, na.rm = TRUE),
    .groups = "drop"
  )

# Under leg: highest line then highest under_price
under_leg <- df %>%
  filter(!is.na(under_price)) %>%
  arrange(snapshot_date, match, player_name, market_name, desc(line), desc(under_price)) %>%
  group_by(snapshot_date, match, player_name, market_name) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(snapshot_date, match, player_name, market_name, line, under_price, stat) %>%
  left_join(line_best_prices, by = c("snapshot_date", "match", "player_name", "market_name", "line")) %>%
  mutate(
    inv_over = 1 / comp_over_price,
    inv_under = 1 / under_price,
    denom = inv_over + inv_under,
    stake_under = 100 * (inv_under / denom),
    outcome = case_when(
      is.na(stat) ~ NA_character_,
      stat < line ~ "win",
      stat > line ~ "loss",
      TRUE ~ "push"
    ),
    profit = case_when(
      outcome == "win" ~ stake_under * (under_price - 1),
      outcome == "loss" ~ -stake_under,
      outcome == "push" ~ 0,
      TRUE ~ NA_real_
    ),
    roi = profit / 100
  ) %>%
  transmute(snapshot_date, match, player_name, market_name,
            strategy = "Under", line, price_selected = under_price,
            comp_price = comp_over_price, stake = stake_under,
            stat, profit, roi)

# Over leg: lowest line then highest over_price
over_leg <- df %>%
  filter(!is.na(over_price)) %>%
  arrange(snapshot_date, match, player_name, market_name, line, desc(over_price)) %>%
  group_by(snapshot_date, match, player_name, market_name) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(snapshot_date, match, player_name, market_name, line, over_price, stat) %>%
  left_join(line_best_prices, by = c("snapshot_date", "match", "player_name", "market_name", "line")) %>%
  mutate(
    inv_over = 1 / over_price,
    inv_under = 1 / comp_under_price,
    denom = inv_over + inv_under,
    stake_over = 100 * (inv_over / denom),
    outcome = case_when(
      is.na(stat) ~ NA_character_,
      stat > line ~ "win",
      stat < line ~ "loss",
      TRUE ~ "push"
    ),
    profit = case_when(
      outcome == "win" ~ stake_over * (over_price - 1),
      outcome == "loss" ~ -stake_over,
      outcome == "push" ~ 0,
      TRUE ~ NA_real_
    ),
    roi = profit / 100
  ) %>%
  transmute(snapshot_date, match, player_name, market_name,
            strategy = "Over", line, price_selected = over_price,
            comp_price = comp_under_price, stake = stake_over,
            stat, profit, roi)

# Combine per-leg results
best_line_leg_performance <- bind_rows(under_leg, over_leg)

# Overall summary by strategy
summary_tbl <- best_line_leg_performance %>%
  group_by(strategy) %>%
  summarise(
    bets = n(),
    total_profit = round(sum(profit, na.rm = TRUE), 2),
    roi_total = round(total_profit / (bets * 100), 4),
    .groups = "drop"
  )

print(summary_tbl)

