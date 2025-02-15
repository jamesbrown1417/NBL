# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(tidyjson)

# Load user functions
source("Scripts/04-helper-functions.R")

# Get player name and team data
player_names_teams <-
    read_csv("Data/supercoach-data.csv") |> 
    mutate(first_initial = str_sub(player_first_name, 1, 1)) |>
    select(player_first_name, first_initial, player_last_name, player_team) |> 
    mutate(player_name_initials = paste(first_initial, player_last_name, sep = " ")) |> 
    mutate(player_full_name = paste(player_first_name, player_last_name, sep = " "))

pointsbet_h2h_main <- function() {

# URL of website
pointsbet_url = "https://api.au.pointsbet.com/api/v2/competitions/7172/events/featured?includeLive=false"

# Make request and get response
pointsbet_response <-
    request(pointsbet_url) |>
    req_perform() |> 
    resp_body_json()

# List of matches and data
events <- pointsbet_response$events

# Loop through to get all data--------------------------------------------------

# Create empty vectors
match_names <- c()
match_starts_at <- c()
home_teams <- c()
away_teams <- c()
event_names <- c()
outcome_names <- c()
outcome_prices <- c()
keys <- c()

# Loop through events
for (match in events) {
    for (market in match$specialFixedOddsMarkets) {
        for (outcome in market$outcomes) {
            # Append data to vectors
            match_names <- c(match_names, match$name)
            match_starts_at <- c(match_starts_at, match$startsAt)
            home_teams <- c(home_teams, match$homeTeam)
            away_teams <- c(away_teams, match$awayTeam)
            event_names <- c(event_names, market$eventName)
            outcome_names <- c(outcome_names, outcome$name)
            outcome_prices <- c(outcome_prices, outcome$price)
            keys <- c(keys, match$key)
        }
    }
}

# Output tibble
pointsbet_data <-
    tibble(
        match = match_names,
        start_time = match_starts_at,
        home_team = home_teams,
        away_team = away_teams,
        event = event_names,
        outcome = outcome_names,
        price = outcome_prices
    ) |> 
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    relocate(match, .before = start_time)


#===============================================================================
# Head to head markets
#===============================================================================

# Filter to head to head markets
pointsbet_data_h2h <-
    pointsbet_data |> 
    filter(event == "Head to Head") |> 
    mutate(outcome = fix_team_names(outcome))
               
# Home Teams
pointsbet_data_h2h_home <-
    pointsbet_data_h2h |> 
    filter(home_team == outcome) |>
    select(match, start_time, market = event, home_team, home_win = price)

# Away Teams
pointsbet_data_h2h_away <-
    pointsbet_data_h2h |> 
    filter(away_team == outcome) |>
    select(match, start_time, market = event, away_team, away_win = price)

# Combine
pointsbet_h2h <-
    full_join(pointsbet_data_h2h_home, pointsbet_data_h2h_away, by = c("match", "start_time", "market")) |> 
    mutate(market = "Head To Head") |>
    select(match, start_time, market_name = market, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Pointsbet")

# Write to csv
write_csv(pointsbet_h2h, "Data/scraped_odds/pointsbet_h2h.csv")

#===============================================================================
# Player Props
#===============================================================================

# Get unique keys
keys <- unique(keys)

# Get each match's api page
match_urls <- paste0("https://api.au.pointsbet.com/api/mes/v3/events/", keys)

# Create a function that gets the player props from each URL
get_player_props <- function(url) {
    # Make request and get response
    pointsbet_response <-
        request(url) |>
        req_perform() |>
        resp_body_json()
    
    # Match info
    home_team <- fix_team_names(pointsbet_response$homeTeam)
    away_team <- fix_team_names(pointsbet_response$awayTeam)
    match <- paste(home_team, "v", away_team)
    
    
    # Loop through to get prop data---------------------------------------------
    
    # Create empty vectors

    market_names <- c()
    outcome_names <- c()
    outcome_types <- c()
    outcome_prices <- c()
    event_key <- c()
    market_key <- c()
    outcome_key <- c()
    
    # Loop through events
    for (market in pointsbet_response$fixedOddsMarkets) {
        for (outcome in market$outcomes) {
            # Append data to vectors
            
            if (!is.null(market$name)) {
                market_names <- c(market_names, market$name)
            } else {
                market_names <- c(market_names, NA)
            }
            
            if (!is.null(outcome$name)) {
                outcome_names <- c(outcome_names, outcome$name)
            } else {
                outcome_names <- c(outcome_names, NA)
            }

            if (!is.null(outcome$outcomeType)) {
                outcome_types <- c(outcome_types, outcome$outcomeType)
            } else {
                outcome_types <- c(outcome_types, NA)
            }
            
            if (!is.null(outcome$price)) {
                outcome_prices <- c(outcome_prices, outcome$price)
            } else {
                outcome_prices <- c(outcome_prices, NA)
            }
            
            event_key <- c(event_key, pointsbet_response$key)
            
            if (!is.null(market$key)) {
                market_key <- c(market_key, market$key)
            } else {
                market_key <- c(market_key, NA)
            }
            
            if (!is.null(outcome$key)) {
                outcome_key <- c(outcome_key, outcome$key)
            } else {
                outcome_key <- c(outcome_key, NA)
            }
        }
    }
    
    # Output tibble
        tibble(
            match = match,
            home_team = home_team,
            away_team = away_team,
            market = market_names,
            outcome = outcome_names,
            outcome_type = outcome_types,
            price = outcome_prices,
            EventKey = event_key,
            MarketKey = market_key,
            OutcomeKey = outcome_key
        )
}

# Map function to each URL
pointsbet_data_player_props <- map_df(match_urls, get_player_props)

# Fix player names
pointsbet_data_player_props <-
    pointsbet_data_player_props |>
    mutate(outcome = str_replace_all(outcome, "Byrce", "Bryce")) |>
    mutate(outcome = str_replace_all(outcome, "Jordon", "Jordan")) |> 
    mutate(outcome = str_replace_all(outcome, "Gary Brown$", "Gary Browne")) |>
    mutate(outcome = str_replace_all(outcome, "Delaney", "Delany")) |>
    mutate(outcome = str_replace_all(outcome, "Le Afa", "Le'Afa"))

#===============================================================================
# Player Points
#===============================================================================

# Player points alternative totals----------------------------------------------

# Filter list to player points
pointsbet_player_points_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "To Get [0-9]{1,2}\\+ Points")) |>
    mutate(line = str_extract(market, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(outcome = str_replace_all(outcome, "^Mitch", "Mitchell")) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("outcome" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Points",
        player_name = outcome,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )

# Player points over / under----------------------------------------------------

# Filter list to player points over under
pointsbet_player_points_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Points Over/Under")) |>
    mutate(outcome = str_replace_all(outcome, "^Mitch", "Mitchell"))

# Get Overs
pointsbet_player_points_over <-
    pointsbet_player_points_over_under |> 
    filter(outcome_type == "Over") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Over ") |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Points",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )
    
# Get Unders
pointsbet_player_points_under <-
    pointsbet_player_points_over_under |> 
    filter(outcome_type == "Under") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Under ") |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Points",
        player_name,
        player_team,
        opposition_team,
        line,
        under_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey_unders = OutcomeKey
    )

# Combine overs and unders
pointsbet_player_points_over_under <-
    pointsbet_player_points_over |>
    left_join(pointsbet_player_points_under) |>
    select(
        match,
        home_team,
        away_team,
        market_name,
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        under_price,
        agency,
        contains("Key")
    )

#===============================================================================
# Player Assists
#===============================================================================

# Player assists alternative totals----------------------------------------------

# Filter list to player assists
pointsbet_player_assists_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "To Get [0-9]{1,2}\\+ Assists")) |>
    mutate(line = str_extract(market, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(outcome = str_replace_all(outcome, "^Mitch", "Mitchell")) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("outcome" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Assists",
        player_name = outcome,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )

# Player assists over / under----------------------------------------------------

# Filter list to player assists over under
pointsbet_player_assists_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Assists Over/Under")) |>
    mutate(outcome = str_replace_all(outcome, "^Mitch", "Mitchell"))

# Get Overs
pointsbet_player_assists_over <-
    pointsbet_player_assists_over_under |> 
    filter(outcome_type == "Over") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Over ") |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Assists",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )

# Get Unders
pointsbet_player_assists_under <-
    pointsbet_player_assists_over_under |> 
    filter(outcome_type == "Under") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Under ") |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Assists",
        player_name,
        player_team,
        opposition_team,
        line,
        under_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey_unders = OutcomeKey
    )

# Combine overs and unders
pointsbet_player_assists_over_under <-
    pointsbet_player_assists_over |>
    left_join(pointsbet_player_assists_under) |>
    select(
        match,
        home_team,
        away_team,
        market_name,
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        under_price,
        agency,
        contains("Key")
    )


#===============================================================================
# Player Rebounds
#===============================================================================

# Player rebounds alternative totals----------------------------------------------

# Filter list to player rebounds
pointsbet_player_rebounds_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "To Get [0-9]{1,2}\\+ Rebounds")) |>
    mutate(line = str_extract(market, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(outcome = str_replace_all(outcome, "^Mitch", "Mitchell")) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("outcome" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Rebounds",
        player_name = outcome,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )

# Player rebounds over / under----------------------------------------------------

# Filter list to player rebounds over under
pointsbet_player_rebounds_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Rebounds Over/Under")) |>
    mutate(outcome = str_replace_all(outcome, "^Mitch", "Mitchell"))

# Get Overs
pointsbet_player_rebounds_over <-
    pointsbet_player_rebounds_over_under |> 
    filter(outcome_type == "Over") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Over ") |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Rebounds",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )

# Get Unders
pointsbet_player_rebounds_under <-
    pointsbet_player_rebounds_over_under |> 
    filter(outcome_type == "Under") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Under ") |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Rebounds",
        player_name,
        player_team,
        opposition_team,
        line,
        under_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey_unders = OutcomeKey
    )

# Combine overs and unders
pointsbet_player_rebounds_over_under <-
    pointsbet_player_rebounds_over |>
    left_join(pointsbet_player_rebounds_under) |>
    select(
        match,
        home_team,
        away_team,
        market_name,
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        under_price,
        agency,
        contains("Key")
    )

#===============================================================================
# Player Threes
#===============================================================================

# Player threes alternative totals----------------------------------------------

# Filter list to player threes
pointsbet_player_threes_lines <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "To Get [0-9]{1,2}\\+ Threes")) |>
    mutate(line = str_extract(market, "[0-9]{1,2}")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    mutate(outcome = str_replace_all(outcome, "^Mitch", "Mitchell")) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("outcome" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Threes",
        player_name = outcome,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )

# Player threes over / under----------------------------------------------------

# Filter list to player threes over under
pointsbet_player_threes_over_under <-
    pointsbet_data_player_props |>
    filter(str_detect(market, "Player Threes Over/Under")) |>
    mutate(outcome = str_replace_all(outcome, "^Mitch", "Mitchell"))

# Get Overs
pointsbet_player_threes_over <-
    pointsbet_player_threes_over_under |> 
    filter(outcome_type == "Over") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Over ") |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Threes",
        player_name,
        player_team,
        opposition_team,
        line,
        over_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey
    )

# Get Unders
pointsbet_player_threes_under <-
    pointsbet_player_threes_over_under |> 
    filter(outcome_type == "Under") |>
    mutate(player_name = outcome) |>
    separate(outcome, into = c("player_name", "line"), sep = " Under ") |>
    mutate(line = as.numeric(line)) |> 
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    transmute(
        match,
        home_team,
        away_team,
        market_name = "Player Threes",
        player_name,
        player_team,
        opposition_team,
        line,
        under_price = price,
        agency = "Pointsbet",
        EventKey,
        MarketKey,
        OutcomeKey_unders = OutcomeKey
    )

# Combine overs and unders
pointsbet_player_threes_over_under <-
    pointsbet_player_threes_over |>
    left_join(pointsbet_player_threes_under) |>
    select(
        match,
        home_team,
        away_team,
        market_name,
        player_name,
        player_team,
        opposition_team,
        line,
        over_price,
        under_price,
        agency,
        contains("Key")
    )

#===============================================================================
# Write to CSV
#===============================================================================

# Points
pointsbet_player_points_lines |>
    bind_rows(pointsbet_player_points_over_under) |>
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price",
        "under_price",
        "agency",
        "opposition_team",
        "EventKey",
        "MarketKey",
        "OutcomeKey",
        "OutcomeKey_unders"
    ) |>
    mutate(market_name = "Player Points") |>
    mutate(agency = "Pointsbet") |> 
    write_csv("Data/scraped_odds/pointsbet_player_points.csv")

# Rebounds
pointsbet_player_rebounds_lines |>
    bind_rows(pointsbet_player_rebounds_over_under) |>
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price",
        "under_price",
        "agency",
        "opposition_team",
        "EventKey",
        "MarketKey",
        "OutcomeKey",
        "OutcomeKey_unders"
    ) |>
    mutate(market_name = "Player Rebounds") |>
    mutate(agency = "Pointsbet") |> 
    write_csv("Data/scraped_odds/pointsbet_player_rebounds.csv")

# Assists
pointsbet_player_assists_lines |>
    bind_rows(pointsbet_player_assists_over_under) |>
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price",
        "under_price",
        "agency",
        "opposition_team",
        "EventKey",
        "MarketKey",
        "OutcomeKey",
        "OutcomeKey_unders"
    ) |>
    mutate(market_name = "Player Assists") |>
    mutate(agency = "Pointsbet") |> 
    write_csv("Data/scraped_odds/pointsbet_player_assists.csv")
}

##%######################################################%##
#                                                          #
####                   Run functions                    ####
#                                                          #
##%######################################################%##

# This runs both the props and head to head as they use same info
h2h_safe_pointsbet <- safely(pointsbet_h2h_main)

# Run functions
h2h_safe_pointsbet()
