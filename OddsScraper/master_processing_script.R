# Libraries
library(tidyverse)

##%######################################################%##
#                                                          #
####                    Head to Head                    ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_odds_files <-
    list.files("Data/scraped_odds", full.names = TRUE, pattern = "h2h") |>
    map(read_csv) |>
    reduce(bind_rows)

# For each match, get the highest home_win
best_home <-
all_odds_files |>
    arrange(start_time, match, desc(home_win)) |>
    group_by(match) |>
    slice_head(n = 1) |>
    select(match, start_time, market_name, home_team, home_win, home_agency = agency) |>
    ungroup()

# For each match, get the highest away_win
best_away <-
all_odds_files |>
    arrange(start_time, match, desc(away_win)) |>
    group_by(match) |>
    slice_head(n = 1) |>
    select(match, start_time, market_name, away_team, away_win, away_agency = agency) |>
    ungroup()

# Combine
best_odds <-
    best_home |>
    left_join(best_away) |>
    mutate(margin = round((1/home_win + 1/away_win), digits = 3))
