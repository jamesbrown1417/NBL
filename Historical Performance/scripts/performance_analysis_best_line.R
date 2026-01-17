#!/usr/bin/env Rscript

# Best-line performance analysis across agencies:
# - For each match/player/market/day, pick:
#   - Under Only: the highest line, then highest under_price
#   - Over Only:  the lowest line, then highest over_price
# - Compute per-bet profit with stake = 100 and ROI = profit/100
# - Produce overall and by-market summaries and plots (with seasonal comparison)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(tibble)
  library(forcats)
  library(ggplot2)
  library(scales)
})

input_path <- "Historical Performance/all_processed_odds_lines.rds"
stopifnot(file.exists(input_path))

df <- readr::read_rds(input_path)

# Optional: filter to specific date range
df <- df %>% filter(snapshot_date >= as.Date("2025-09-01"))

# Optional: filter to specific agencies
# df <- df |> filter(str_detect(agency, "Dabble", negate = TRUE))

# We only need columns relevant for selection and evaluation
df <- df %>%
  select(snapshot_date, match, player_name, market_name, line, over_price, under_price, stat)

# Unders: select highest line then highest under price
under_sel <- df %>%
  filter(!is.na(under_price)) %>%
  arrange(snapshot_date, match, player_name, market_name, desc(line), desc(under_price)) %>%
  group_by(snapshot_date, match, player_name, market_name) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(snapshot_date, match, player_name, market_name, line, over_price, under_price, stat) %>%
  mutate(
    inv_over = 1 / over_price,
    inv_under = 1 / under_price,
    denom = inv_over + inv_under,
    stake_over = 100 * (inv_over / denom),
    stake_under = 100 * (inv_under / denom),
    outcome = case_when(
      is.na(stat) ~ NA_character_,
      stat > line ~ "over",
      stat < line ~ "under",
      TRUE ~ "push"
    ),
    under_profit = case_when(
      outcome == "under" ~ stake_under * (under_price - 1),
      outcome == "over" ~ -stake_under,
      outcome == "push" ~ 0,
      TRUE ~ NA_real_
    )
  )


# Overs: select lowest line then highest over price
over_sel <- df %>%
  filter(!is.na(over_price)) %>%
  arrange(snapshot_date, match, player_name, market_name, line, desc(over_price)) %>%
  group_by(snapshot_date, match, player_name, market_name) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(snapshot_date, match, player_name, market_name, line, over_price, under_price, stat) %>%
  mutate(
    inv_over = 1 / over_price,
    inv_under = 1 / under_price,
    denom = inv_over + inv_under,
    stake_over = 100 * (inv_over / denom),
    stake_under = 100 * (inv_under / denom),
    outcome = case_when(
      is.na(stat) ~ NA_character_,
      stat > line ~ "over",
      stat < line ~ "under",
      TRUE ~ "push"
    ),
    over_profit = case_when(
      outcome == "over" ~ stake_over * (over_price - 1),
      outcome == "under" ~ -stake_over,
      outcome == "push" ~ 0,
      TRUE ~ NA_real_
    )
  )

# ------------------------
# Summaries and plots
# ------------------------

# UNDER: overall and by-market summaries using under_profit
under_overall <- under_sel %>%
  summarise(
    bets = n(),
    total_profit = sum(under_profit, na.rm = TRUE),
    sum_stake = sum(stake_under, na.rm = TRUE),
    roi_total = total_profit / sum_stake,
    .groups = "drop"
  ) %>%
  mutate(level = "overall", key = "all")

under_market <- under_sel %>%
  group_by(market_name) %>%
  summarise(
    bets = n(),
    total_profit = sum(under_profit, na.rm = TRUE),
    sum_stake = sum(stake_under, na.rm = TRUE),
    roi_total = total_profit / sum_stake,
    .groups = "drop"
  ) %>%
  mutate(level = "market", key = market_name) %>% select(-market_name)

under_summary <- bind_rows(under_overall, under_market) %>% mutate(level = factor(level, levels = c("overall", "market")))

print(under_summary, n = nrow(under_summary))

# Plot UNDER ROI
u_df <- under_summary %>% select(level, key, roi_total)
u_order <- under_summary %>% group_by(level) %>% arrange(roi_total) %>% mutate(order = row_number()) %>% ungroup() %>% select(level, key, order)
u_df <- u_df %>% left_join(u_order, by = c("level", "key"))

dir.create("Historical Performance/figures", recursive = TRUE, showWarnings = FALSE)

p_u <- ggplot(u_df, aes(x = forcats::fct_reorder(key, order, .desc = TRUE), y = roi_total)) +
  geom_col(fill = "#2E86C1", width = 0.7) +
  geom_text(aes(label = scales::percent(roi_total, accuracy = 0.1)), hjust = -0.1, size = 3) +
  coord_flip() +
  facet_wrap(~ level, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), breaks = scales::pretty_breaks(n = 10), expand = expansion(mult = c(0.02, 0.2))) +
  labs(title = "ROI from Betting Unders (Best Under Line)", x = NULL, y = "ROI (%)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"), plot.background = element_rect(fill = "white", color = NA), panel.background = element_rect(fill = "white", color = NA), axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Historical Performance/figures/performance_roi_best_unders.png", p_u, width = 12, height = 8, dpi = 300, bg = "white")

# OVER: overall and by-market summaries using over_profit
over_overall <- over_sel %>%
  summarise(
    bets = n(),
    total_profit = sum(over_profit, na.rm = TRUE),
    sum_stake = sum(stake_over, na.rm = TRUE),
    roi_total = total_profit / sum_stake,
    .groups = "drop"
  ) %>%
  mutate(level = "overall", key = "all")

over_market <- over_sel %>%
  group_by(market_name) %>%
  summarise(
    bets = n(),
    total_profit = sum(over_profit, na.rm = TRUE),
    sum_stake = sum(stake_over, na.rm = TRUE),
    roi_total = total_profit / sum_stake,
    .groups = "drop"
  ) %>%
  mutate(level = "market", key = market_name) %>% select(-market_name)

over_summary <- bind_rows(over_overall, over_market) %>% mutate(level = factor(level, levels = c("overall", "market")))

print(over_summary, n = nrow(over_summary))

# Plot OVER ROI
o_df <- over_summary %>% select(level, key, roi_total)
o_order <- over_summary %>% group_by(level) %>% arrange(roi_total) %>% mutate(order = row_number()) %>% ungroup() %>% select(level, key, order)
o_df <- o_df %>% left_join(o_order, by = c("level", "key"))

p_o <- ggplot(o_df, aes(x = forcats::fct_reorder(key, order, .desc = TRUE), y = roi_total)) +
  geom_col(fill = "#E67E22", width = 0.7) +
  geom_text(aes(label = scales::percent(roi_total, accuracy = 0.1)), hjust = -0.1, size = 3) +
  coord_flip() +
  facet_wrap(~ level, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), breaks = scales::pretty_breaks(n = 10), expand = expansion(mult = c(0.02, 0.2))) +
  labs(title = "ROI from Betting Overs (Best Over Line)", x = NULL, y = "ROI (%)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"), plot.background = element_rect(fill = "white", color = NA), panel.background = element_rect(fill = "white", color = NA), axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Historical Performance/figures/performance_roi_best_overs.png", p_o, width = 12, height = 8, dpi = 300, bg = "white")
