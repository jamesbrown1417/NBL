suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
x <- readRDS("Data/processed/stats/combined_stats_table.rds") |>
  filter(season == "2025-2026") |>
  mutate(player_name = paste(first_name, family_name), player_team = name,
         minutes = as.numeric(ms(player_minutes)) / 60) |>
  filter(minutes >= 5)
p <- read_csv("Data/raw/stats/supercoach-data.csv", show_col_types = FALSE) |>
  select(player_name, player_team, position = supercoach_position_1) |>
  filter(!is.na(position))
x |> inner_join(p, by = c("player_name", "player_team")) |>
  transmute(player = player_name, team = player_team, position,
            opponent = opp_name, matchId = match_id,
            date = as.character(as.Date(match_time_utc)), minutes,
            points = player_points, rebounds = player_rebounds_total,
            assists = player_assists, threes = player_three_pointers_made) |>
  write.csv("/tmp/nbl-dvp-r-audit.csv", row.names = FALSE)
