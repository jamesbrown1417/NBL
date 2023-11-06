# Libraries and functions-------------------------------------------------------
library(tidyverse)
library(googlesheets4)
library(googledrive)

# Google sheets authentification -----------------------------------------------
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "cuzzy.punting@gmail.com")
gs4_auth(token = drive_token())

# Empirical Probabilities Script
source("Scripts/08-get-empirical-probabilities.R")

# Run all odds scraping scripts-------------------------------------------------
source("OddsScraper/scrape_betr.R")
source("OddsScraper/scrape_BetRight.R")
source("OddsScraper/scrape_Palmerbet.R")
source("OddsScraper/scrape_pointsbet.R")
source("OddsScraper/scrape_sportsbet.R")
source("OddsScraper/scrape_TAB.R")
source("OddsScraper/scrape_neds.R")
source("OddsScraper/scrape_TopSport.R")
source("OddsScraper/scrape_bet365.R")
source("OddsScraper/scrape_bluebet.R")

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

# For each match, get all home wins
all_home <-
all_odds_files |>
    arrange(match, start_time, desc(home_win)) |>
    select(match, start_time, market_name, home_team, home_win, home_agency = agency) |> 
    mutate(start_time = date(start_time)) |> 
    select(-start_time)

# For each match, get all away wins
all_away <-
all_odds_files |>
    arrange(match, start_time, desc(away_win)) |>
    select(match, start_time, market_name, away_team, away_win, away_agency = agency) |> 
    mutate(start_time = date(start_time)) |> 
    select(-start_time)

# Combine
all_odds_h2h <-
    all_home |>
    full_join(all_away, relationship = "many-to-many", by = c("match", "market_name")) |>
    mutate(margin = (1/home_win + 1/away_win)) |> 
    mutate(margin = round(100*(margin - 1), digits = 3)) |> 
    arrange(margin)

# Google Sheets-----------------------------------------------------
sheet <- gs4_find("NBL Data")
sheet_write(sheet, data = all_odds_h2h, sheet = "H2H")

##%######################################################%##
#                                                          #
####                    Total Points                    ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_totals_files <-
    list.files("Data/scraped_odds", full.names = TRUE, pattern = "total") |>
    map(read_csv) |>
    reduce(bind_rows) |> 
    mutate(market_name = "Total Points")

# For each match, get all unders
all_unders <-
    all_totals_files |>
    arrange(start_time, match, total_points_line, desc(under_price)) |>
    select(match, start_time, market_name, home_team, away_team, total_points_line, under_price, under_agency = agency)

# For each match, get all overs
all_overs <-
    all_totals_files |>
    arrange(start_time, match, total_points_line, desc(over_price)) |>
    select(match, start_time, market_name, home_team, away_team, total_points_line, over_price, over_agency = agency)

# Combine
all_odds_totals <-
    all_unders |>
    full_join(all_overs, relationship = "many-to-many") |>
    mutate(margin = (1/under_price + 1/over_price)) |> 
    mutate(margin = round(100*(margin - 1), digits = 3)) |> 
    arrange(margin)

# Google Sheets-----------------------------------------------------
sheet_write(sheet, data = all_odds_totals, sheet = "Total Points")

##%######################################################%##
#                                                          #
####                   Player Points                    ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_points <-
    list.files("Data/scraped_odds", full.names = TRUE, pattern = "player_points") |>
    map(read_csv) |>
    # Ignore null elements
    keep(~nrow(.x) > 0) |>
    reduce(bind_rows) |> 
    arrange(player_name, line, desc(over_price))


# Add empirical probabilities---------------------------------------------------

# Points
distinct_point_combos <-
    all_player_points |> 
    distinct(player_name, line)

player_emp_probs_2022_23 <-
    pmap(distinct_point_combos, get_empirical_prob, "PTS", "2022_2023", .progress = TRUE) |> 
    bind_rows() |> 
    select(player_name, line, games_played_2022_2023 = games_played, empirical_prob_2022_2023)

player_emp_probs_2023_24 <- 
    pmap(distinct_point_combos, get_empirical_prob, "PTS", "2023_2024", .progress = TRUE) |> 
    bind_rows() |> 
    select(player_name, line, games_played_2023_2024 = games_played, empirical_prob_2023_2024)

all_player_points <-
    all_player_points |>
    mutate(
        implied_prob_over = 1 / over_price,
        implied_prob_under = 1 / under_price
    ) |>
    left_join(player_emp_probs_2022_23, by = c("player_name", "line")) |>
    left_join(player_emp_probs_2023_24, by = c("player_name", "line")) |>
    rename(empirical_prob_over_2022_23 = empirical_prob_2022_2023,
           empirical_prob_over_2023_24 = empirical_prob_2023_2024) |>
    mutate(empirical_prob_under_2022_23 = 1 - empirical_prob_over_2022_23,
           empirical_prob_under_2023_24 = 1 - empirical_prob_over_2023_24) |>
    mutate(
        diff_over_2022_23 = empirical_prob_over_2022_23 - implied_prob_over,
        diff_under_2022_23 = empirical_prob_under_2022_23 - implied_prob_under,
        diff_over_2023_24 = empirical_prob_over_2023_24 - implied_prob_over,
        diff_under_2023_24 = empirical_prob_under_2023_24 - implied_prob_under
    ) |>
    relocate(agency, .after = diff_under_2023_24) |>
    mutate_if(is.double, round, 2) |>
    filter(!is.na(opposition_team)) |>
    group_by(player_name, line) |>
    mutate(
        min_implied_prob = min(implied_prob_over, na.rm = TRUE),
        max_implied_prob = max(implied_prob_over, na.rm = TRUE)
    ) |>
    mutate(variation = max_implied_prob - min_implied_prob) |>
    ungroup() |>
    select(-min_implied_prob,-max_implied_prob) |>
    arrange(desc(variation), player_name, desc(over_price), line)

# Add to google sheets
sheet_write(sheet, data = all_player_points, sheet = "Player Points")

# Write as RDS
all_player_points |> write_rds("Data/processed_odds/all_player_points.rds")

##%######################################################%##
#                                                          #
####                   Player Assists                   ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_assists <-
    list.files("Data/scraped_odds", full.names = TRUE, pattern = "player_assists") |>
    map(read_csv) |>
    # Ignore null elements
    keep(~nrow(.x) > 0) |>
    reduce(bind_rows)

# Add empirical probabilities---------------------------------------------------

# Assists
distinct_assist_combos <-
    all_player_assists |> 
    distinct(player_name, line)

player_emp_probs_2022_23 <-
    pmap(distinct_assist_combos, get_empirical_prob, "AST", "2022_2023", .progress = TRUE) |> 
    bind_rows() |> 
    select(player_name, line, games_played_2022_2023 = games_played, empirical_prob_2022_2023)

player_emp_probs_2023_24 <- 
    pmap(distinct_assist_combos, get_empirical_prob, "AST", "2023_2024", .progress = TRUE) |> 
    bind_rows() |> 
    select(player_name, line, games_played_2023_2024 = games_played, empirical_prob_2023_2024)

all_player_assists <-
    all_player_assists |>
    mutate(
        implied_prob_over = 1 / over_price,
        implied_prob_under = 1 / under_price
    ) |>
    left_join(player_emp_probs_2022_23, by = c("player_name", "line")) |>
    left_join(player_emp_probs_2023_24, by = c("player_name", "line")) |>
    rename(empirical_prob_over_2022_23 = empirical_prob_2022_2023,
           empirical_prob_over_2023_24 = empirical_prob_2023_2024) |>
    mutate(empirical_prob_under_2022_23 = 1 - empirical_prob_over_2022_23,
           empirical_prob_under_2023_24 = 1 - empirical_prob_over_2023_24) |>
    mutate(
        diff_over_2022_23 = empirical_prob_over_2022_23 - implied_prob_over,
        diff_under_2022_23 = empirical_prob_under_2022_23 - implied_prob_under,
        diff_over_2023_24 = empirical_prob_over_2023_24 - implied_prob_over,
        diff_under_2023_24 = empirical_prob_under_2023_24 - implied_prob_under
    ) |>
    relocate(agency, .after = diff_under_2023_24) |>
    mutate_if(is.double, round, 2) |>
    filter(!is.na(opposition_team)) |>
    group_by(player_name, line) |>
    mutate(
        min_implied_prob = min(implied_prob_over, na.rm = TRUE),
        max_implied_prob = max(implied_prob_over, na.rm = TRUE)
    ) |>
    mutate(variation = max_implied_prob - min_implied_prob) |>
    ungroup() |>
    select(-min_implied_prob,-max_implied_prob) |>
    arrange(desc(variation), player_name, desc(over_price), line)

# Add to google sheets
sheet_write(sheet, data = all_player_assists, sheet = "Player Assists")

# Write as RDS
all_player_assists |> write_rds("Data/processed_odds/all_player_assists.rds")

##%######################################################%##
#                                                          #
####                  Player Rebounds                   ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_rebounds <-
    list.files("Data/scraped_odds", full.names = TRUE, pattern = "player_rebounds") |>
    map(read_csv) |>
    # Ignore null elements
    keep(~nrow(.x) > 0) |>
    reduce(bind_rows)


# Add empirical probabilities---------------------------------------------------

# Rebounds
distinct_rebound_combos <-
    all_player_rebounds |> 
    distinct(player_name, line)

player_emp_probs_2022_23 <-
    pmap(distinct_rebound_combos, get_empirical_prob, "REB", "2022_2023", .progress = TRUE) |> 
    bind_rows() |> 
    select(player_name, line, games_played_2022_2023 = games_played, empirical_prob_2022_2023)

player_emp_probs_2023_24 <- 
    pmap(distinct_rebound_combos, get_empirical_prob, "REB", "2023_2024", .progress = TRUE) |> 
    bind_rows() |> 
    select(player_name, line, games_played_2023_2024 = games_played, empirical_prob_2023_2024)

all_player_rebounds <-
    all_player_rebounds |>
    mutate(
        implied_prob_over = 1 / over_price,
        implied_prob_under = 1 / under_price
    ) |>
    left_join(player_emp_probs_2022_23, by = c("player_name", "line")) |>
    left_join(player_emp_probs_2023_24, by = c("player_name", "line")) |>
    rename(empirical_prob_over_2022_23 = empirical_prob_2022_2023,
           empirical_prob_over_2023_24 = empirical_prob_2023_2024) |>
    mutate(empirical_prob_under_2022_23 = 1 - empirical_prob_over_2022_23,
           empirical_prob_under_2023_24 = 1 - empirical_prob_over_2023_24) |>
    mutate(
        diff_over_2022_23 = empirical_prob_over_2022_23 - implied_prob_over,
        diff_under_2022_23 = empirical_prob_under_2022_23 - implied_prob_under,
        diff_over_2023_24 = empirical_prob_over_2023_24 - implied_prob_over,
        diff_under_2023_24 = empirical_prob_under_2023_24 - implied_prob_under
    ) |>
    relocate(agency, .after = diff_under_2023_24) |>
    mutate_if(is.double, round, 2) |>
    filter(!is.na(opposition_team)) |>
    group_by(player_name, line) |>
    mutate(
        min_implied_prob = min(implied_prob_over, na.rm = TRUE),
        max_implied_prob = max(implied_prob_over, na.rm = TRUE)
    ) |>
    mutate(variation = max_implied_prob - min_implied_prob) |>
    ungroup() |>
    select(-min_implied_prob,-max_implied_prob) |>
    arrange(desc(variation), player_name, desc(over_price), line)

# Add to google sheets
sheet_write(sheet, data = all_player_rebounds, sheet = "Player Rebounds")

# Write as RDS
all_player_rebounds |> write_rds("Data/processed_odds/all_player_rebounds.rds")

##%######################################################%##
#                                                          #
####                   Player Threes                    ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_threes <-
    list.files("Data/scraped_odds", full.names = TRUE, pattern = "player_threes") |>
    map(read_csv) |>
    # Ignore null elements
    keep(~nrow(.x) > 0) |>
    reduce(bind_rows)

# Add empirical probabilities---------------------------------------------------

# Player Threes
distinct_rebound_combos <-
    all_player_threes |> 
    distinct(player_name, line)

player_emp_probs_2022_23 <-
    pmap(distinct_rebound_combos, get_empirical_prob, "Threes", "2022_2023", .progress = TRUE) |> 
    bind_rows() |> 
    select(player_name, line, games_played_2022_2023 = games_played, empirical_prob_2022_2023)

player_emp_probs_2023_24 <- 
    pmap(distinct_rebound_combos, get_empirical_prob, "Threes", "2023_2024", .progress = TRUE) |> 
    bind_rows() |> 
    select(player_name, line, games_played_2023_2024 = games_played, empirical_prob_2023_2024)

all_player_threes <-
    all_player_threes |>
    mutate(
        implied_prob_over = 1 / over_price,
        implied_prob_under = 1 / under_price
    ) |>
    left_join(player_emp_probs_2022_23, by = c("player_name", "line")) |>
    left_join(player_emp_probs_2023_24, by = c("player_name", "line")) |>
    rename(empirical_prob_over_2022_23 = empirical_prob_2022_2023,
           empirical_prob_over_2023_24 = empirical_prob_2023_2024) |>
    mutate(empirical_prob_under_2022_23 = 1 - empirical_prob_over_2022_23,
           empirical_prob_under_2023_24 = 1 - empirical_prob_over_2023_24) |>
    mutate(
        diff_over_2022_23 = empirical_prob_over_2022_23 - implied_prob_over,
        diff_under_2022_23 = empirical_prob_under_2022_23 - implied_prob_under,
        diff_over_2023_24 = empirical_prob_over_2023_24 - implied_prob_over,
        diff_under_2023_24 = empirical_prob_under_2023_24 - implied_prob_under
    ) |>
    relocate(agency, .after = diff_under_2023_24) |>
    mutate_if(is.double, round, 2) |>
    filter(!is.na(opposition_team)) |>
    group_by(player_name, line) |>
    mutate(
        min_implied_prob = min(implied_prob_over, na.rm = TRUE),
        max_implied_prob = max(implied_prob_over, na.rm = TRUE)
    ) |>
    mutate(variation = max_implied_prob - min_implied_prob) |>
    ungroup() |>
    select(-min_implied_prob,-max_implied_prob) |>
    arrange(desc(variation), player_name, desc(over_price), line)

# Add to google sheets
sheet_write(sheet, data = all_player_threes, sheet = "Player Threes")

# Write as RDS
all_player_threes |> write_rds("Data/processed_odds/all_player_threes.rds")
