#!/usr/bin/env Rscript

# Minimal loader: reads all RDS snapshots under
# "Historical Performance/datasets" (from project root),
# adds snapshot date/time from filename (Australia/Adelaide tz),
# and combines into one tibble `all_processed_odds`.

suppressPackageStartupMessages({
  library(purrr)
  library(dplyr)
  library(stringr)
  library(readr)
  library(tibble)
  library(lubridate)
})

datasets_dir <- "Historical Performance/datasets"
rds_files <- list.files(datasets_dir, pattern = "\\.rds$", full.names = TRUE)

parse_snapshot_ts <- function(path) {
  fname <- basename(path)
  m <- stringr::str_match(fname, "__([0-9]{8}_[0-9]{6})_")
  ts_str <- if (nrow(m) > 0) m[1, 2] else NA_character_
  if (is.na(ts_str)) return(as.POSIXct(NA))
  readr::parse_datetime(ts_str, format = "%Y%m%d_%H%M%S", locale = readr::locale(tz = "Australia/Adelaide"))
}

read_with_ts <- function(path) {
  df <- readRDS(path)
  if (!is.data.frame(df)) df <- tibble(value = list(df)) else df <- tibble::as_tibble(df)
  ts <- parse_snapshot_ts(path)
  df %>% mutate(
    snapshot_datetime = ts,
    snapshot_date = as.Date(ts)
  )
}

all_processed_odds <- purrr::map_dfr(rds_files, read_with_ts)

# Keep only the latest snapshot per day for the same
# match, player_name, line, agency and market_name.
# In case of ties on timestamp, keep the first occurrence.
all_processed_odds_filtered <- all_processed_odds %>%
  dplyr::group_by(snapshot_date, .data$match, .data$player_name, .data$line, .data$market_name, .data$agency) %>%
  dplyr::slice_max(order_by = snapshot_datetime, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

# Keep only lines
all_processed_odds_lines <-
    all_processed_odds_filtered %>%
    filter(!is.na(under_price)) |> 
    select(match, snapshot_date, player_name, line, market_name, agency, over_price, under_price)

# Read in box score data
all_player_stats <-
    read_rds("Data/combined_stats_table.rds") |>
    mutate(PLAYER_NAME = paste(first_name, family_name)) |>
    mutate(minutes_played = ifelse(str_detect(player_minutes, "\\:"), period_to_seconds(ms(player_minutes)) / 60, player_minutes)) |>
    mutate(minutes_played = as.numeric(minutes_played)) |>
    rename(
        PTS = player_points,
        REB = player_rebounds_total,
        AST = player_assists,
        STL = player_steals,
        BLK = player_blocks,
        THREES = player_three_pointers_made,
        MIN = minutes_played,
        SEASON_YEAR = season
    ) |>
    mutate(MIN = round(MIN, 2)) |>
    mutate(PRA = PTS + REB + AST) |>
    mutate(HOME_TEAM = ifelse(home_away == "home", name, opp_name)) |>
    mutate(AWAY_TEAM = ifelse(home_away == "away", name, opp_name)) |> 
    mutate(match = paste0(HOME_TEAM, " v ", AWAY_TEAM)) |>
    select(match, player_name = PLAYER_NAME, match_time_utc, PTS, REB, AST, STL, BLK, PRA, THREES) |>
    mutate(
        match_datetime_adelaide = with_tz(match_time_utc, tzone = "Australia/Adelaide"),
        match_date = as.Date(match_datetime_adelaide)
    )

# Join the relevant stat onto each odds row by match, player, and match date (Adelaide)
stats_for_join <- all_player_stats %>%
  select(match, player_name, match_date, PTS, REB, AST, STL, BLK, PRA, THREES)

all_processed_odds_lines <- all_processed_odds_lines %>%
  left_join(stats_for_join, by = c("match", "player_name", "snapshot_date" = "match_date")) %>%
  mutate(
    stat = case_when(
      market_name == "Player Points" ~ PTS,
      market_name == "Player Rebounds" ~ REB,
      market_name == "Player Assists" ~ AST,
      market_name %in% c("Player Threes", "Player Threes Made") ~ THREES,
      market_name == "Player PRAs" ~ PRA,
      market_name == "Player Steals" ~ STL,
      market_name == "Player Blocks" ~ BLK,
      TRUE ~ NA_real_
    )
  ) %>%
  select(match, snapshot_date, player_name, line, market_name, agency, over_price, under_price, stat) %>%
  filter(!is.na(stat))

# Persist filtered odds+stats lines for analysis
readr::write_rds(all_processed_odds_lines, "Historical Performance/all_processed_odds_lines.rds")
