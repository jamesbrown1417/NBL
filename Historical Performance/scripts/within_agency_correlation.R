#!/usr/bin/env Rscript

# Performance analysis for over/under positions using dutching stakes that sum to 100.
# Loads the pre-saved odds+stats dataset and computes per-row profits for
# over and under, plus total profit.

suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(tibble)
    library(tidyr)
    library(ggplot2)
    library(scales)
    library(forcats)
})

input_path <- "Historical Performance/all_processed_odds_lines.rds"
stopifnot(file.exists(input_path))

df <- readr::read_rds(input_path)

# Ensure prices present
df <- df %>% filter(!is.na(over_price), !is.na(under_price))

# Optional filter date
# df <- df %>% filter(snapshot_date >= as.Date("2025-12-01"))

# Compute dutching stakes that sum to 100
arb_df <- df %>%
    mutate(
        inv_over = 1 / over_price,
        inv_under = 1 / under_price,
        denom = inv_over + inv_under,
        stake_over = 100 * (inv_over / denom),
        stake_under = 100 * (inv_under / denom)
    ) %>%
    # Determine outcome
    mutate(
        outcome = case_when(
            is.na(stat) ~ NA_character_,
            stat > line ~ "over",
            stat < line ~ "under",
            TRUE ~ "push"
        ),
        # Per-side profit per outcome
        over_profit = case_when(
            outcome == "over" ~ stake_over * over_price - stake_over,
            outcome == "under" ~ -stake_over,
            outcome == "push" ~ 0,
            TRUE ~ NA_real_
        ),
        under_profit = case_when(
            outcome == "under" ~ stake_under * under_price - stake_under,
            outcome == "over" ~ -stake_under,
            outcome == "push" ~ 0,
            TRUE ~ NA_real_
        ),
        total_profit = over_profit + under_profit,
        roi = total_profit / 100
    ) %>%
    select(match, snapshot_date, player_name, player_team, market_name, line, agency,
           over_price, under_price, stat, outcome,
           stake_over, stake_under, over_profit, under_profit, total_profit)

# Expose result
arb_performance <- arb_df

# Create a function to summarize performance within a team
sportsbet_summary <-
arb_performance |> 
    filter(agency == "Sportsbet") |> 
    group_by(match, snapshot_date, player_team) |> 
    summarise(
        total_bets = n(),
        over_percentage = mean(outcome == "over", na.rm = TRUE) * 100
    ) |> 
    filter(total_bets >= 10)
