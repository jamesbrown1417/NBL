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
  select(match, snapshot_date, player_name, market_name, line, agency,
         over_price, under_price, stat, outcome,
         stake_over, stake_under, over_profit, under_profit, total_profit)

# Expose result
arb_performance <- arb_df

# Build summary tables: overall, by agency, by market
summarise_block <- function(data) {
  group_cols <- dplyr::group_vars(data)
  out <- data %>%
    summarise(
      bets = n(),
      pushes = sum(outcome == "push", na.rm = TRUE),
      win_over = sum(outcome == "over", na.rm = TRUE),
      win_under = sum(outcome == "under", na.rm = TRUE),
      stake_total = n() * 100,
      profit_over = sum(over_profit, na.rm = TRUE),
      profit_under = sum(under_profit, na.rm = TRUE),
      total_profit = sum(total_profit, na.rm = TRUE),
      roi_total = total_profit / stake_total,
      roi_over_only_total = profit_over / sum(stake_over, na.rm = TRUE),
      roi_under_only_total = profit_under / sum(stake_under, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      profit_over = round(profit_over, 2),
      profit_under = round(profit_under, 2),
      total_profit = round(total_profit, 2),
      roi_total = round(roi_total, 4),
      roi_over_only_total = round(roi_over_only_total, 4),
      roi_under_only_total = round(roi_under_only_total, 4)
    )
  # Keep grouping columns if present
  out %>% select(any_of(c(group_cols, "bets", "pushes", "win_over", "win_under",
                          "profit_over", "profit_under", "total_profit",
                          "profit_over_only", "profit_under_only",
                          "roi_avg", "roi_total", "roi_over_only_total", "roi_under_only_total")))
}

overall_summary <- arb_performance %>%
  summarise_block() %>%
  mutate(level = "overall", key = "all")

agency_summary <- arb_performance %>%
  group_by(agency) %>%
  summarise_block() %>%
  mutate(level = "agency", key = agency) %>%
  select(-agency)

market_summary <- arb_performance %>%
  group_by(market_name) %>%
  summarise_block() %>%
  mutate(level = "market", key = market_name) %>%
  select(-market_name)

performance_summary <- bind_rows(overall_summary, agency_summary, market_summary) %>%
  mutate(level = factor(level, levels = c("overall", "agency", "market"))) %>%
  arrange(level, key)

print(performance_summary, n = nrow(performance_summary))

# Build publication-ready plot comparing ROI by strategy across overall, agencies and markets
# Prepare long-format data for plotting
plot_df <- performance_summary %>%
  select(level, key, roi_total, roi_over_only_total, roi_under_only_total) %>%
  pivot_longer(cols = starts_with("roi_"), names_to = "strategy", values_to = "roi") %>%
  mutate(
    strategy = recode(strategy,
                      roi_total = "Dutching (Over+Under)",
                      roi_over_only_total = "Over Only",
                      roi_under_only_total = "Under Only"),
    hjust_val = if_else(roi >= 0, -0.1, 1.1)
  )

# Order keys within each level by Dutching ROI
order_map <- performance_summary %>%
  select(level, key, roi_total) %>%
  group_by(level) %>%
  arrange(roi_total) %>%
  mutate(order = row_number()) %>%
  ungroup()

plot_df <- plot_df %>%
  left_join(order_map, by = c("level", "key")) %>%
  mutate(key = forcats::fct_reorder(key, order, .desc = TRUE))

# Overall sample size for subtitle annotation
overall_n <- performance_summary %>%
  dplyr::filter(level == "overall", key == "all") %>%
  dplyr::pull(bets)

p <- ggplot(plot_df, aes(x = key, y = roi, fill = strategy)) +
  geom_col(width = 0.7, position = position_dodge(width = 0.75)) +
  geom_text(aes(label = scales::percent(roi, accuracy = 0.1), hjust = hjust_val),
            position = position_dodge(width = 0.75), size = 3) +
  coord_flip() +
  facet_wrap(~ level, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     breaks = scales::pretty_breaks(n = 10),
                     expand = expansion(mult = c(0.02, 0.2))) +
  scale_fill_manual(
    name = "Strategy",
    breaks = c("Dutching (Over+Under)", "Over Only", "Under Only"),
    values = c(
      "Dutching (Over+Under)" = "#7F8C8D",  # grey
      "Over Only" = "#E67E22",              # orange
      "Under Only" = "#2E86C1"              # blue
    )
  ) +
  labs(
    title = "ROI by Strategy across Agencies and Markets",
    subtitle = paste0(
      "Dutching vs. Over-only vs. Under-only (stake = 100 total)",
      "\nOverall n = ", overall_n, " bets"
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

# Save plot
dir.create("Historical Performance/figures", recursive = TRUE, showWarnings = FALSE)
ggsave(filename = "Historical Performance/figures/performance_roi.png", plot = p,
       width = 12, height = 8, dpi = 300, bg = "white")

# Seasonal comparison: 2024-25 (2024-10-01 to 2025-06-01) and 2025-26 (2025-10-01 to 2026-06-01)
make_performance_summary <- function(data) {
  overall_summary <- data %>%
    summarise_block() %>%
    mutate(level = "overall", key = "all")
  agency_summary <- data %>%
    group_by(agency) %>%
    summarise_block() %>%
    mutate(level = "agency", key = agency) %>%
    select(-agency)
  market_summary <- data %>%
    group_by(market_name) %>%
    summarise_block() %>%
    mutate(level = "market", key = market_name) %>%
    select(-market_name)
  bind_rows(overall_summary, agency_summary, market_summary) %>%
    mutate(level = factor(level, levels = c("overall", "agency", "market")))
}

season1 <- arb_performance %>%
  filter(snapshot_date >= as.Date("2024-10-01"), snapshot_date < as.Date("2025-06-01"))
season2 <- arb_performance %>%
  filter(snapshot_date >= as.Date("2025-10-01"), snapshot_date < as.Date("2026-06-01"))

season_summary1 <- make_performance_summary(season1) %>% mutate(season = "2024–25")
season_summary2 <- make_performance_summary(season2) %>% mutate(season = "2025–26")

season_performance_summary <- bind_rows(season_summary1, season_summary2)

# Plot seasonal comparison
season_plot_df <- season_performance_summary %>%
  select(season, level, key, roi_total, roi_over_only_total, roi_under_only_total) %>%
  pivot_longer(cols = starts_with("roi_"), names_to = "strategy", values_to = "roi") %>%
  mutate(
    strategy = recode(strategy,
                      roi_total = "Dutching (Over+Under)",
                      roi_over_only_total = "Over Only",
                      roi_under_only_total = "Under Only"),
    hjust_val = if_else(roi >= 0, -0.1, 1.1)
  )

order_map_season <- season_performance_summary %>%
  select(season, level, key, roi_total) %>%
  group_by(season, level) %>%
  arrange(roi_total) %>%
  mutate(order = row_number()) %>%
  ungroup()

season_plot_df <- season_plot_df %>%
  left_join(order_map_season, by = c("season", "level", "key")) %>%
  mutate(key = forcats::fct_reorder(key, order, .desc = TRUE))

# Overall sample sizes per season for subtitle
season_ns <- season_performance_summary %>%
  dplyr::filter(level == "overall", key == "all") %>%
  dplyr::select(season, bets)
season_n_label <- paste(season_ns$season, paste0("n = ", season_ns$bets), collapse = " | ")

p_season <- ggplot(season_plot_df, aes(x = key, y = roi, fill = strategy)) +
  geom_col(width = 0.7, position = position_dodge(width = 0.75)) +
  geom_text(aes(label = scales::percent(roi, accuracy = 0.1), hjust = hjust_val),
            position = position_dodge(width = 0.75), size = 3) +
  coord_flip() +
  facet_grid(level ~ season, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     breaks = scales::pretty_breaks(n = 10),
                     expand = expansion(mult = c(0.02, 0.2))) +
  scale_fill_manual(
    name = "Strategy",
    breaks = c("Dutching (Over+Under)", "Over Only", "Under Only"),
    values = c(
      "Dutching (Over+Under)" = "#7F8C8D",
      "Over Only" = "#E67E22",
      "Under Only" = "#2E86C1"
    )
  ) +
  labs(
    title = "ROI by Strategy across Seasons, Agencies, and Markets",
    subtitle = paste0(
      "Dutching vs. Over-only vs. Under-only (stake = 100 total)",
      "\n", season_n_label
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

ggsave(filename = "Historical Performance/figures/performance_roi_by_season.png", plot = p_season,
       width = 14, height = 10, dpi = 300, bg = "white")
