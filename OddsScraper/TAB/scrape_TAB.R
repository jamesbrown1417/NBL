# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(httr)
library(jsonlite)

# Load user functions
source("Scripts/04-helper-functions.R")

# Get player name and team data
player_names_teams <-
    read_csv("Data/supercoach-data.csv") |> 
    mutate(first_initial = str_sub(player_first_name, 1, 1)) |>
    select(player_first_name, first_initial, player_last_name, player_team) |> 
    mutate(player_name_initials = paste(first_initial, player_last_name, sep = " "))

main_tab <- function() {

# Get response body
tab_response <- fromJSON("OddsScraper/TAB/tab_response.json")

# Function to extract market info from response---------------------------------
get_market_info <- function(markets) {
    
    # Market info
    markets_name = markets$betOption
    market_propositions = markets$propositions
    
    # Output Tibble
    tibble(market = markets_name,
           propositions = market_propositions)
}

# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
    # Match info
    match_name = matches$name
    match_round = matches$round
    match_start_time = matches$startTime
    
    # Market info
    market_info = map(matches$markets, get_market_info) |> bind_rows()
    
    # Output Tibble
    tibble(
        match = match_name,
        round = match_round,
        start_time = match_start_time,
        market_name = market_info$market,
        propositions = market_info$propositions
    )
}

# List of matches
matches <- map(1:nrow(tab_response$matches), ~ tab_response$matches[., ])
    
# Map functions to data
all_tab_markets <-
    map(matches, get_match_info) |> bind_rows()

# Expand list col into multiple cols
all_tab_markets <-
  all_tab_markets |>
  unnest(cols = c(propositions)) |> 
  select(any_of(c("match",
                  "round",
                  "start_time",
                  "market_name")),
         prop_id = id,
         prop_name = name,
         price = returnWin)
    
#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
    all_tab_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Head To Head") |> 
    group_by(match) |> 
    filter(row_number() == 1) |> 
    rename(home_win = price) |> 
    select(-prop_name) |> 
    rename(home_prop_id = prop_id)

# Away teams
away_teams <-
    all_tab_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Head To Head") |> 
    group_by(match) |> 
    filter(row_number() == 2) |> 
    rename(away_win = price) |> 
    select(-prop_name) |> 
    rename(away_prop_id = prop_id)

# Combine
tab_head_to_head_markets <-
    home_teams |>
    left_join(away_teams, by = c("match", "home_team", "away_team", "start_time", "market_name")) |>
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "TAB")

# Fix team names
tab_head_to_head_markets <-
    tab_head_to_head_markets |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(tab_head_to_head_markets, "Data/scraped_odds/tab_h2h.csv")

#===============================================================================
# Total line markets
#===============================================================================

# Under lines
under_lines <-
    all_tab_markets |>
    filter(market_name == "Total Points Over/Under") |> 
    filter(str_detect(prop_name, "Under")) |> 
    mutate(line = as.numeric(str_extract(prop_name, "\\d+\\.\\d+"))) |>
    select(match, start_time, market_name, line, under_price = price, under_prop_id = prop_id)

# Over lines
over_lines <-
    all_tab_markets |>
    filter(market_name == "Total Points Over/Under") |> 
    filter(str_detect(prop_name, "Over")) |> 
    mutate(line = as.numeric(str_extract(prop_name, "\\d+\\.\\d+"))) |>
    select(match, start_time, market_name, line, over_price = price, prop_id)

# Combine
tab_total_line_markets <-
    under_lines |>
    left_join(over_lines) |> 
    select(match, start_time, market_name, line, under_price, over_price) |> 
    mutate(margin = round((1/under_price + 1/over_price), digits = 3)) |> 
    mutate(agency = "TAB")

# Fix team names
tab_total_line_markets <-
    tab_total_line_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |> 
    mutate(market = "Total Match Points")

# Write to csv
write_csv(tab_total_line_markets, "Data/scraped_odds/tab_total_points.csv")

#===============================================================================
# Player Points
#===============================================================================

# Filter to player points markets
player_points_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "(Player Points$)|(\\d+\\+ Points)"))

# Extract player names
player_points_markets <-
    player_points_markets |>
    mutate(prop_name = str_remove_all(prop_name, " \\(.*\\)")) |>
    mutate(prop_name = if_else(str_detect(market_name, "\\d+\\+ Points"),paste(prop_name, market_name) , prop_name)) |> 
    mutate(prop_name = if_else(str_detect(market_name, "\\d+\\+ Points"), str_replace(prop_name, "Points", "Pts") , prop_name)) |> 
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under")) |> 
    mutate(line = if_else(market_name == "Alternate Player Points", line - 0.5, line)) |> 
    mutate(line = if_else(str_detect(market_name, "\\d+\\+ Points"), line - 0.5, line)) |> 
    arrange(prop_name, market_name, line) |> 
    group_by(prop_name) |> 
    slice_head(n = 1) |> 
    ungroup()

# Over lines
over_lines <-
    player_points_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Points") |>
    select(match, market_name, player_name, line, over_price = price, prop_id)

# Under lines
under_lines <-
    player_points_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Points") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_points_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_points_markets <-
    tab_player_points_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

# Add first initial for players who were not given one
tab_player_points_markets <-
    tab_player_points_markets |> 
    mutate(player_name = str_replace_all(player_name, "^Dellavedova", "M Dellavedova")) |> 
    mutate(player_name = str_replace_all(player_name, "Le Afa", "Le'Afa")) |> 
    mutate(player_name = str_replace_all(player_name, "Lual-Acuil", "J Lual-Acuil"))

# Separate player name into first and last name
tab_player_points_markets <-
    tab_player_points_markets |> 
    separate(player_name, into = c("first_name", "last_name"), sep = " ") |>
    mutate(first_initial = substr(first_name, 1, 1)) |> 
    mutate(player_name = paste(first_initial, last_name)) |> 
    mutate(player_name = str_replace_all(player_name, "TeRangi", "Te Rangi")) |> 
    mutate(player_name = str_replace_all(player_name, "Delaney", "Delany")) |>
    mutate(player_name = str_replace_all(player_name, "P J-Crtwght", "P Jackson-Cartwright")) |>
    mutate(player_name = str_replace_all(player_name, "W McD-White", "W McDowell-White")) |>
    mutate(player_name = str_replace_all(player_name, "S Wardnburg", "S Waardenburg")) |>
    mutate(player_name = str_replace_all(player_name, "J Lual-Acuil", "J Lual-Acuil Jr")) |>
    left_join(player_names_teams[,c("player_name_initials", "player_first_name", "player_last_name", "player_team")], by = c("player_name" = "player_name_initials")) |> 
    mutate(player_name = paste(player_first_name, player_last_name)) |>
    select(-first_initial, -first_name, -last_name, -player_first_name, -player_last_name) |>
    relocate(player_name, player_team, .after = market_name)

# Create opposition team variable
tab_player_points_markets <-
    tab_player_points_markets |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team))

#===============================================================================
# Player Assists
#===============================================================================

# Filter to player assists markets
player_assists_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "(Player Assists$)|(\\d+\\+ Assists)"))

# Extract player names
player_assists_markets <-
    player_assists_markets |>
    mutate(prop_name = str_remove_all(prop_name, " \\(.*\\)")) |>
    mutate(prop_name = if_else(str_detect(market_name, "\\d+\\+ Assists"),paste(prop_name, market_name) , prop_name)) |> 
    mutate(prop_name = if_else(str_detect(market_name, "\\d+\\+ Assists"), str_replace(prop_name, "Assists", "Ast") , prop_name)) |> 
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under")) |> 
    mutate(line = if_else(market_name == "Alternate Player Assists", line - 0.5, line)) |> 
    mutate(line = if_else(str_detect(market_name, "\\d+\\+ Assists"), line - 0.5, line)) |> 
    arrange(prop_name, market_name, line) |> 
    group_by(prop_name) |> 
    slice_head(n = 1) |> 
    ungroup()

# Over lines
over_lines <-
    player_assists_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Assists") |>
    select(match, market_name, player_name, line, over_price = price, prop_id)

# Under lines
under_lines <-
    player_assists_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Assists") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_assists_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_assists_markets <-
    tab_player_assists_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

# Add first initial for players who were not given one
tab_player_assists_markets <-
    tab_player_assists_markets |> 
    mutate(player_name = str_replace_all(player_name, "^Dellavedova", "M Dellavedova")) |> 
    mutate(player_name = str_replace_all(player_name, "Le Afa", "Le'Afa")) |> 
    mutate(player_name = str_replace_all(player_name, "Lual-Acuil", "J Lual-Acuil"))

# Separate player name into first and last name
tab_player_assists_markets <-
    tab_player_assists_markets |> 
    separate(player_name, into = c("first_name", "last_name"), sep = " ") |>
    mutate(first_initial = substr(first_name, 1, 1)) |> 
    mutate(player_name = paste(first_initial, last_name)) |> 
    mutate(player_name = str_replace_all(player_name, "TeRangi", "Te Rangi")) |> 
    mutate(player_name = str_replace_all(player_name, "Delaney", "Delany")) |>
    mutate(player_name = str_replace_all(player_name, "P J-Crtwght", "P Jackson-Cartwright")) |>
    mutate(player_name = str_replace_all(player_name, "W McD-White", "W McDowell-White")) |>
    mutate(player_name = str_replace_all(player_name, "S Wardnburg", "S Waardenburg")) |>
    mutate(player_name = str_replace_all(player_name, "J Lual-Acuil", "J Lual-Acuil Jr")) |>
    left_join(player_names_teams[,c("player_name_initials", "player_first_name", "player_last_name", "player_team")], by = c("player_name" = "player_name_initials")) |> 
    mutate(player_name = paste(player_first_name, player_last_name)) |>
    select(-first_initial, -first_name, -last_name, -player_first_name, -player_last_name) |>
    relocate(player_name, player_team, .after = market_name)

# Create opposition team variable
tab_player_assists_markets <-
    tab_player_assists_markets |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team))

#===============================================================================
# Player Rebounds
#===============================================================================

# Filter to player rebounds markets
player_rebounds_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "(Player Rebounds$)|(\\d+\\+ Rebounds)"))

# Extract player names
player_rebounds_markets <-
    player_rebounds_markets |>
    mutate(prop_name = str_remove_all(prop_name, " \\(.*\\)")) |>
    mutate(prop_name = if_else(str_detect(market_name, "\\d+\\+ Rebounds"),paste(prop_name, market_name) , prop_name)) |> 
    mutate(prop_name = if_else(str_detect(market_name, "\\d+\\+ Rebounds"), str_replace(prop_name, "Rebounds", "Reb") , prop_name)) |> 
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under")) |> 
    mutate(line = if_else(market_name == "Alternate Player Rebounds", line - 0.5, line)) |> 
    mutate(line = if_else(str_detect(market_name, "\\d+\\+ Rebounds"), line - 0.5, line)) |> 
    arrange(prop_name, market_name, line) |> 
    group_by(prop_name) |> 
    slice_head(n = 1) |> 
    ungroup()

# Over lines
over_lines <-
    player_rebounds_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Rebounds") |>
    select(match, market_name, player_name, line, over_price = price, prop_id)

# Under lines
under_lines <-
    player_rebounds_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Rebounds") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_rebounds_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_rebounds_markets <-
    tab_player_rebounds_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

# Add first initial for players who were not given one
tab_player_rebounds_markets <-
    tab_player_rebounds_markets |> 
    mutate(player_name = str_replace_all(player_name, "^Dellavedova", "M Dellavedova")) |> 
    mutate(player_name = str_replace_all(player_name, "Le Afa", "Le'Afa")) |> 
    mutate(player_name = str_replace_all(player_name, "Lual-Acuil", "J Lual-Acuil"))

# Separate player name into first and last name
tab_player_rebounds_markets <-
    tab_player_rebounds_markets |> 
    separate(player_name, into = c("first_name", "last_name"), sep = " ") |>
    mutate(first_initial = substr(first_name, 1, 1)) |> 
    mutate(player_name = paste(first_initial, last_name)) |> 
    mutate(player_name = str_replace_all(player_name, "TeRangi", "Te Rangi")) |> 
    mutate(player_name = str_replace_all(player_name, "Delaney", "Delany")) |>
    mutate(player_name = str_replace_all(player_name, "P J-Crtwght", "P Jackson-Cartwright")) |>
    mutate(player_name = str_replace_all(player_name, "W McD-White", "W McDowell-White")) |>
    mutate(player_name = str_replace_all(player_name, "S Wardnburg", "S Waardenburg")) |>
    mutate(player_name = str_replace_all(player_name, "J Lual-Acuil", "J Lual-Acuil Jr")) |>
    left_join(player_names_teams[,c("player_name_initials", "player_first_name", "player_last_name", "player_team")], by = c("player_name" = "player_name_initials")) |> 
    mutate(player_name = paste(player_first_name, player_last_name)) |>
    select(-first_initial, -first_name, -last_name, -player_first_name, -player_last_name) |>
    relocate(player_name, player_team, .after = market_name)

# Create opposition team variable
tab_player_rebounds_markets <-
    tab_player_rebounds_markets |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team))

#===============================================================================
# Player Threes
#=============================================================================== 

# Filter to player threes markets
player_threes_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "(Player Threes$)|(\\d+\\+ Threes)"))

# Extract player names
player_threes_markets <-
    player_threes_markets |>
    mutate(prop_name = str_remove_all(prop_name, " \\(.*\\)")) |>
    mutate(prop_name = if_else(str_detect(market_name, "\\d+\\+ Threes"),paste(prop_name, market_name) , prop_name)) |> 
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under")) |> 
    mutate(line = if_else(market_name == "Alternate Player Threes", line - 0.5, line)) |> 
    mutate(line = if_else(str_detect(market_name, "\\d+\\+ Threes"), line - 0.5, line)) |> 
    arrange(prop_name, market_name, line) |> 
    group_by(prop_name) |> 
    slice_head(n = 1) |> 
    ungroup()

# Over lines
over_lines <-
    player_threes_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Threes") |>
    select(match, market_name, player_name, line, over_price = price, prop_id)

# Under lines
under_lines <-
    player_threes_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Threes") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_threes_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_threes_markets <-
    tab_player_threes_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

# Add first initial for players who were not given one
tab_player_threes_markets <-
    tab_player_threes_markets |> 
    mutate(player_name = str_replace_all(player_name, "^Dellavedova", "M Dellavedova")) |> 
    mutate(player_name = str_replace_all(player_name, "Le Afa", "Le'Afa")) |> 
    mutate(player_name = str_replace_all(player_name, "Lual-Acuil", "J Lual-Acuil"))

# Separate player name into first and last name
tab_player_threes_markets <-
    tab_player_threes_markets |> 
    separate(player_name, into = c("first_name", "last_name"), sep = " ") |>
    mutate(first_initial = substr(first_name, 1, 1)) |> 
    mutate(player_name = paste(first_initial, last_name)) |> 
    mutate(player_name = str_replace_all(player_name, "TeRangi", "Te Rangi")) |> 
    mutate(player_name = str_replace_all(player_name, "Delaney", "Delany")) |>
    mutate(player_name = str_replace_all(player_name, "P J-Crtwght", "P Jackson-Cartwright")) |>
    mutate(player_name = str_replace_all(player_name, "W McD-White", "W McDowell-White")) |>
    mutate(player_name = str_replace_all(player_name, "S Wardnburg", "S Waardenburg")) |>
    mutate(player_name = str_replace_all(player_name, "J Lual-Acuil", "J Lual-Acuil Jr")) |>
    left_join(player_names_teams[,c("player_name_initials", "player_first_name", "player_last_name", "player_team")], by = c("player_name" = "player_name_initials")) |> 
    mutate(player_name = paste(player_first_name, player_last_name)) |>
    select(-first_initial, -first_name, -last_name, -player_first_name, -player_last_name) |>
    relocate(player_name, player_team, .after = market_name)

# Create opposition team variable
tab_player_threes_markets <-
    tab_player_threes_markets |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |> 
    distinct(player_name, line, over_price, .keep_all = TRUE)

#===============================================================================
# Write to CSV------------------------------------------------------------------
#===============================================================================

tab_player_points_markets |> write_csv("Data/scraped_odds/tab_player_points.csv")
tab_player_assists_markets |> write_csv("Data/scraped_odds/tab_player_assists.csv")
tab_player_rebounds_markets |> write_csv("Data/scraped_odds/tab_player_rebounds.csv")
tab_player_threes_markets |> write_csv("Data/scraped_odds/tab_player_threes.csv")
}

#===============================================================================
# Run safe function
#===============================================================================

safe_main_tab <- safely(main_tab)
safe_main_tab()