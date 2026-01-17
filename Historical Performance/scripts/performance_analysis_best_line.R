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

# We only need columns relevant for selection and evaluation
df <- df %>%
  select(snapshot_date, match, player_name, market_name, line, over_price, under_price, stat)

# Precompute complementary best prices at each exact line within a group
line_best_prices <- df %>%
  group_by(snapshot_date, match, player_name, market_name, line) %>%
  summarise(
    over_price_at_line = max(over_price, na.rm = TRUE),
    under_price_at_line = max(under_price, na.rm = TRUE),
    .groups = "drop"
  )

# Unders: select highest line then highest under price
under_sel <- df %>%
  filter(!is.na(under_price)) %>%
  arrange(snapshot_date, match, player_name, market_name, desc(line), desc(under_price)) %>%
  group_by(snapshot_date, match, player_name, market_name) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(snapshot_date, match, player_name, market_name, under_line = line, under_price = under_price, stat)

under_with_comp <- under_sel %>%
  left_join(line_best_prices, by = c("snapshot_date", "match", "player_name", "market_name", "under_line" = "line")) %>%
  mutate(
    comp_over_price = over_price_at_line,
    inv_over = 1 / comp_over_price,
    inv_under = 1 / under_price,
    denom = inv_over + inv_under,
    stake_over = 100 * (inv_over / denom),
    stake_under = 100 * (inv_under / denom),
    outcome_under = case_when(
      is.na(stat) ~ NA_character_,
      stat < under_line ~ "win",
      stat > under_line ~ "loss",
      TRUE ~ "push"
    ),
    outcome_over_at_under = case_when(
      is.na(stat) ~ NA_character_,
      stat > under_line ~ "win",
      stat < under_line ~ "loss",
      TRUE ~ "push"
    ),
    under_only_profit = case_when(
      outcome_under == "win" ~ 100 * (under_price - 1),
      outcome_under == "loss" ~ -100,
      outcome_under == "push" ~ 0,
      TRUE ~ NA_real_
    ),
    dutch_under_profit = case_when(
      outcome_under == "win" ~ stake_under * (under_price - 1),
      outcome_under == "loss" ~ -stake_under,
      outcome_under == "push" ~ 0,
      TRUE ~ NA_real_
    ),
    dutch_over_profit = case_when(
      outcome_over_at_under == "win" ~ stake_over * (comp_over_price - 1),
      outcome_over_at_under == "loss" ~ -stake_over,
      outcome_over_at_under == "push" ~ 0,
      TRUE ~ NA_real_
    ),
    dutch_total_underline = dutch_under_profit + dutch_over_profit,
    dutch_roi_underline = dutch_total_underline / 100
  )

# Overs: select lowest line then highest over price
over_sel <- df %>%
  filter(!is.na(over_price)) %>%
  arrange(snapshot_date, match, player_name, market_name, line, desc(over_price)) %>%
  group_by(snapshot_date, match, player_name, market_name) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(snapshot_date, match, player_name, market_name, over_line = line, over_price = over_price, stat)

over_with_comp <- over_sel %>%
  left_join(line_best_prices, by = c("snapshot_date", "match", "player_name", "market_name", "over_line" = "line")) %>%
  mutate(
    comp_under_price = under_price_at_line,
    inv_over = 1 / over_price,
    inv_under = 1 / comp_under_price,
    denom = inv_over + inv_under,
    stake_over = 100 * (inv_over / denom),
    stake_under = 100 * (inv_under / denom),
    outcome_over = case_when(
      is.na(stat) ~ NA_character_,
      stat > over_line ~ "win",
      stat < over_line ~ "loss",
      TRUE ~ "push"
    ),
    outcome_under_at_over = case_when(
      is.na(stat) ~ NA_character_,
      stat < over_line ~ "win",
      stat > over_line ~ "loss",
      TRUE ~ "push"
    ),
    over_only_profit = case_when(
      outcome_over == "win" ~ 100 * (over_price - 1),
      outcome_over == "loss" ~ -100,
      outcome_over == "push" ~ 0,
      TRUE ~ NA_real_
    ),
    dutch_over_profit = case_when(
      outcome_over == "win" ~ stake_over * (over_price - 1),
      outcome_over == "loss" ~ -stake_over,
      outcome_over == "push" ~ 0,
      TRUE ~ NA_real_
    ),
    dutch_under_profit = case_when(
      outcome_under_at_over == "win" ~ stake_under * (comp_under_price - 1),
      outcome_under_at_over == "loss" ~ -stake_under,
      outcome_under_at_over == "push" ~ 0,
      TRUE ~ NA_real_
    ),
    dutch_total_overline = dutch_over_profit + dutch_under_profit,
    dutch_roi_overline = dutch_total_overline / 100
  )

# Long-format per-bet results for summarisation
best_line_performance <- bind_rows(
  over_with_comp %>% transmute(snapshot_date, match, player_name, market_name,
                               strategy = "Over Only", profit = over_only_profit, roi = over_only_profit / 100),
  under_with_comp %>% transmute(snapshot_date, match, player_name, market_name,
                                strategy = "Dutching (Under line)", profit = dutch_total_underline, roi = dutch_roi_underline),
  over_with_comp %>% transmute(snapshot_date, match, player_name, market_name,
                               strategy = "Dutching (Over line)", profit = dutch_total_overline, roi = dutch_roi_overline)
)

# Summary helpers (overall + by market)
summarise_best <- function(data) {
  group_cols <- dplyr::group_vars(data)
  out <- data %>%
    summarise(
      bets = n(),
      stake_total = n() * 100,
      total_profit = sum(profit, na.rm = TRUE),
      roi_avg = mean(roi, na.rm = TRUE),
      roi_total = total_profit / stake_total,
      .groups = "drop"
    ) %>%
    mutate(
      total_profit = round(total_profit, 2),
      roi_avg = round(roi_avg, 4),
      roi_total = round(roi_total, 4)
    )
  out %>% select(any_of(c(group_cols, "bets", "total_profit", "roi_avg", "roi_total")))
}

overall_summary <- best_line_performance %>%
  summarise_best() %>%
  mutate(level = "overall", key = "all")

market_summary <- best_line_performance %>%
  group_by(market_name, strategy) %>%
  summarise_best() %>%
  mutate(level = "market", key = market_name) %>%
  select(-market_name)

performance_summary_best <- bind_rows(
  overall_summary %>% mutate(strategy = "All"),
  market_summary
) %>%
  mutate(level = factor(level, levels = c("overall", "market"))) %>%
  arrange(level, key, strategy)

print(performance_summary_best, n = nrow(performance_summary_best))

# Plot: ROI by strategy across overall and markets
plot_df <- performance_summary_best %>%
  filter(level %in% c("overall", "market")) %>%
  filter(strategy != "All") %>%
  select(level, key, strategy, roi_total)

# Order keys within each level by average of strategies
order_map <- performance_summary_best %>%
  filter(level %in% c("overall", "market")) %>%
  filter(strategy != "All") %>%
  group_by(level, key) %>%
  summarise(order_roi = mean(roi_total, na.rm = TRUE), .groups = "drop") %>%
  group_by(level) %>% arrange(order_roi) %>% mutate(order = row_number()) %>% ungroup() %>%
  select(level, key, order)

plot_df <- plot_df %>%
  left_join(order_map, by = c("level", "key")) %>%
  mutate(key = forcats::fct_reorder(key, order, .desc = TRUE)) %>%
  mutate(hjust_val = if_else(roi_total >= 0, -0.1, 1.1))

overall_n <- performance_summary_best %>% filter(level == "overall", key == "all", strategy == "All") %>% pull(bets)

p <- ggplot(plot_df, aes(x = key, y = roi_total, fill = strategy)) +
  geom_col(width = 0.7, position = position_dodge(width = 0.75)) +
  geom_text(aes(label = scales::percent(roi_total, accuracy = 0.1), hjust = hjust_val),
            position = position_dodge(width = 0.75), size = 3) +
  coord_flip() +
  facet_wrap(~ level, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     breaks = scales::pretty_breaks(n = 10),
                     expand = expansion(mult = c(0.02, 0.2))) +
  scale_fill_manual(
    name = "Strategy",
    breaks = c("Over Only", "Dutching (Under line)", "Dutching (Over line)"),
    values = c(
      "Over Only" = "#E67E22",
      "Dutching (Under line)" = "#7F8C8D",
      "Dutching (Over line)" = "#7F8C8D"
    )
  ) +
  labs(
    title = "ROI by Strategy (Best Line & Price across Agencies)",
    subtitle = paste0(
      "Over: lowest line + best over price; Under: highest line + best under price\n",
      "Stake = 100 per bet; Overall n = ", overall_n
    ),
    x = NULL,
    y = "ROI (%)",
    caption = "Pushes counted as 0 profit; ROI = total profit / total stake"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 0)
  )

dir.create("Historical Performance/figures", recursive = TRUE, showWarnings = FALSE)
ggsave(filename = "Historical Performance/figures/performance_roi_best_line.png", plot = p,
       width = 12, height = 8, dpi = 300, bg = "white")

# Seasonal comparison (2024–25 and 2025–26)
make_summary <- function(data) {
  overall <- data %>% summarise_best() %>% mutate(level = "overall", key = "all") %>% mutate(strategy = "All")
  market  <- data %>% group_by(market_name, strategy) %>% summarise_best() %>% mutate(level = "market", key = market_name) %>% select(-market_name)
  bind_rows(overall, market) %>% mutate(level = factor(level, levels = c("overall", "market")))
}

season1 <- best_line_performance %>% filter(snapshot_date >= as.Date("2024-10-01"), snapshot_date < as.Date("2025-06-01"))
season2 <- best_line_performance %>% filter(snapshot_date >= as.Date("2025-10-01"), snapshot_date < as.Date("2026-06-01"))

season_sum1 <- make_summary(season1) %>% mutate(season = "2024–25")
season_sum2 <- make_summary(season2) %>% mutate(season = "2025–26")
season_summary_best <- bind_rows(season_sum1, season_sum2)

season_plot_df <- season_summary_best %>%
  filter(strategy != "All") %>%
  select(season, level, key, strategy, roi_total)

order_map_season <- season_summary_best %>%
  filter(strategy != "All") %>%
  group_by(season, level, key) %>%
  summarise(order_roi = mean(roi_total, na.rm = TRUE), .groups = "drop") %>%
  group_by(season, level) %>% arrange(order_roi) %>% mutate(order = row_number()) %>% ungroup() %>%
  select(season, level, key, order)

season_plot_df <- season_plot_df %>%
  left_join(order_map_season, by = c("season", "level", "key")) %>%
  mutate(key = forcats::fct_reorder(key, order, .desc = TRUE)) %>%
  mutate(hjust_val = if_else(roi_total >= 0, -0.1, 1.1))

season_ns <- season_summary_best %>% filter(level == "overall", key == "all", strategy == "All") %>% select(season, bets)
season_n_label <- paste(season_ns$season, paste0("n = ", season_ns$bets), collapse = " | ")

p_season <- ggplot(season_plot_df, aes(x = key, y = roi_total, fill = strategy)) +
  geom_col(width = 0.7, position = position_dodge(width = 0.75)) +
  geom_text(aes(label = scales::percent(roi_total, accuracy = 0.1), hjust = hjust_val),
            position = position_dodge(width = 0.75), size = 3) +
  coord_flip() +
  facet_grid(level ~ season, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     breaks = scales::pretty_breaks(n = 10),
                     expand = expansion(mult = c(0.02, 0.2))) +
  scale_fill_manual(
    name = "Strategy",
    breaks = c("Over Only", "Dutching (Under line)", "Dutching (Over line)"),
    values = c(
      "Over Only" = "#E67E22",
      "Dutching (Under line)" = "#7F8C8D",
      "Dutching (Over line)" = "#7F8C8D"
    )
  ) +
  labs(
    title = "ROI by Strategy (Best Line & Price) across Seasons",
    subtitle = paste0(
      "Over-only = lowest line + best price; Under-only = highest line + best price\n",
      season_n_label
    ),
    x = NULL,
    y = "ROI (%)",
    caption = "Seasons: 2024–25 (Oct 1–Jun 1) and 2025–26 (Oct 1–Jun 1). Pushes counted as 0 profit."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 0)
  )

ggsave(filename = "Historical Performance/figures/performance_roi_best_line_by_season.png", plot = p_season,
       width = 14, height = 10, dpi = 300, bg = "white")
