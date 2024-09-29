# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(glue)

# Load user functions
source("Scripts/04-helper-functions.R")

# Get player name and team data
player_names_teams <-
    read_csv("Data/supercoach-data.csv") |> 
    mutate(first_initial = str_sub(player_first_name, 1, 1)) |>
    select(player_first_name, first_initial, player_last_name, player_team) |> 
    mutate(player_name_initials = paste(first_initial, player_last_name, sep = " ")) |> 
    mutate(player_full_name = paste(player_first_name, player_last_name, sep = " "))

# URL to get responses
betright_url = "https://next-api.betright.com.au/Sports/Category?categoryId=110"

# Make request and get response
betright_response <-
    request(betright_url) |>
    req_perform() |> 
    resp_body_json()

# Get events list
events_list <- betright_response$masterCategories[[1]]$categories[[1]]$masterEvents

# Get only elements of list with masterEventClassName = "Matches"
events_list <- map(events_list, function(x) if (x$masterEventClassName == "Matches") x else NULL) |> compact()

# Function to extract market info from response---------------------------------
get_market_info <- function(markets) {
    
    # Market info
    markets_name = markets$eventName
    market_propositions = markets$outcomeName
    market_prices = markets$price
    
    # Output Tibble
    tibble(market = markets_name,
           propositions = market_propositions,
           prices = market_prices)
}


# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
    # Match info
    match_name = matches$masterEventName
    match_start_time = matches$minAdvertisedStartTimeUtc
    match_id = matches$masterEventId
    
    # Market info
    market_info = map(matches$markets, get_market_info) |> bind_rows()

    # Output Tibble
    tibble(
        match = match_name,
        match_id = match_id,
        start_time = match_start_time,
        market_name = market_info$market,
        propositions = market_info$propositions,
        prices = market_info$prices
    )
}

# Map functions to data
all_betright_markets <-
    map(events_list, get_match_info) |> bind_rows()

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
    all_betright_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Money Line") |> 
    mutate(market_name = "Head To Head") |> 
    group_by(match) |> 
    filter(row_number() == 1) |> 
    rename(home_win = prices) |> 
    select(-propositions)

# Away teams
away_teams <-
    all_betright_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Money Line") |> 
    mutate(market_name = "Head To Head") |>
    group_by(match) |> 
    filter(row_number() == 2) |> 
    rename(away_win = prices) |> 
    select(-propositions)

# Combine
betright_head_to_head_markets <-
    home_teams |>
    left_join(away_teams) |> 
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "BetRight")

# Fix team names
betright_head_to_head_markets <-
    betright_head_to_head_markets |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(betright_head_to_head_markets, "Data/scraped_odds/betright_h2h.csv")

#===============================================================================
# Player Props
#===============================================================================

# Get API URL for each market type----------------------------------------------

# Player Stats
player_points_links <-
    glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G670")

player_assist_links <-
    glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G672")

player_rebound_links <-
    glue("https://next-api.betright.com.au/Sports/MasterEvent?masterEventId={unique(all_betright_markets$match_id)}&groupTypeCode=G673")

# Function to extract prop data from links--------------------------------------

get_prop_data <- function(link) {
    
    # Get response
    response <-
        request(link) |>
        req_perform() |> 
        resp_body_json()
    
    # Empty vectors to append to
    event_name <- c()
    event_id <- c()
    outcome_title <- c()
    outcome_name <- c()
    outcome_id <- c()
    group_by_header <- c()
    fixed_market_id <- c()
    price <- c()
    
    for (event in response$events) {
        for (outcome in event$outcomes) {
            event_name <- c(event_name, event$eventName)
            event_id <- c(event_id, event$eventId)
            outcome_title <- c(outcome_title, outcome$eventName)
            outcome_name <- c(outcome_name, outcome$outcomeName)
            outcome_id <- c(outcome_id, outcome$outcomeId)
            group_by_header <- c(group_by_header, outcome$groupByHeader)
            fixed_market_id <- c(fixed_market_id, outcome$fixedMarketId)
            price <- c(price, outcome$price)
        }
    }
    
    # Output Tibble
    tibble(
        event_name = event_name,
        event_id = event_id,
        outcome_title = outcome_title,
        outcome_name = outcome_name,
        outcome_id = outcome_id,
        group_by_header = group_by_header,
        fixed_market_id = fixed_market_id,
        price = price,
        link
    )
}

# Safe version of function
safe_get_prop_data <- safely(get_prop_data)

#===============================================================================
# Player Points
#===============================================================================

# Match names to join
match_names <-
    all_betright_markets |>
    distinct(match, match_id)

# Get all player points
betright_player_points <-
    map(player_points_links, safe_get_prop_data) |> 
    map("result") |>
    bind_rows() |>
    rename(match_id = link) |> 
    mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
    left_join(match_names) |> 
    filter(!is.na(outcome_name)) |> 
    separate(outcome_title, into = c("market_name", "player_name"), sep = " - ") |>
    mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |>
    mutate(player_name = str_replace_all(player_name, "  ", " ")) |>  
    mutate(player_name = str_replace(player_name, "^Mitch", "Mitchell")) |>
    mutate(player_name = str_replace(player_name, "^William", "Will")) |>
    mutate(player_name = str_replace(player_name, "Lee Jr.", "Lee")) |>
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    mutate(agency = "BetRight") |>
    mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price" = "price",
        "agency",
        "group_by_header",
        "event_id",
        "outcome_name",
        "outcome_id",
        "fixed_market_id",
        "opposition_team"
    ) |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(player_team = fix_team_names(player_team)) |>
    mutate(opposition_team = fix_team_names(opposition_team)) |> 
    mutate(match = paste(home_team, away_team, sep = " v "))

#===============================================================================
# Player Rebounds
#===============================================================================

# Get all player rebounds
betright_player_rebounds <-
    map(player_rebound_links, safe_get_prop_data) |> 
    map("result") |>
    bind_rows() |>
    rename(match_id = link) |> 
    mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
    left_join(match_names) |> 
    filter(!is.na(outcome_name)) |> 
    separate(outcome_title, into = c("market_name", "player_name"), sep = " - ") |>
    mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |>
    mutate(player_name = str_replace_all(player_name, "  ", " ")) |>  
    mutate(player_name = str_replace(player_name, "^Mitch", "Mitchell")) |>
    mutate(player_name = str_replace(player_name, "^William", "Will")) |>
    mutate(player_name = str_replace(player_name, "Lee Jr.", "Lee")) |>
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    mutate(agency = "BetRight") |>
    mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price" = "price",
        "agency",
        "group_by_header",
        "event_id",
        "outcome_name",
        "outcome_id",
        "fixed_market_id",
        "opposition_team"
    ) |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(player_team = fix_team_names(player_team)) |>
    mutate(opposition_team = fix_team_names(opposition_team)) |> 
    mutate(match = paste(home_team, away_team, sep = " v "))

#===============================================================================
# Player Assists
#===============================================================================

# Get all player assists
betright_player_assists <-
    map(player_assist_links, safe_get_prop_data) |> 
    map("result") |>
    bind_rows() |>
    rename(match_id = link) |> 
    mutate(match_id = as.integer(str_extract(match_id, "[0-9]{4,7}"))) |> 
    left_join(match_names) |> 
    filter(!is.na(outcome_name)) |> 
    separate(outcome_title, into = c("market_name", "player_name"), sep = " - ") |>
    mutate(player_name = str_remove_all(player_name, " \\(.*\\)")) |>
    mutate(player_name = str_replace_all(player_name, "  ", " ")) |>  
    mutate(player_name = str_replace(player_name, "^Mitch", "Mitchell")) |>
    mutate(player_name = str_replace(player_name, "^William", "Will")) |>
    mutate(player_name = str_replace(player_name, "Lee Jr.", "Lee")) |>
    left_join(player_names_teams[, c("player_full_name", "player_team")], by = c("player_name" = "player_full_name")) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |>
    mutate(agency = "BetRight") |>
    mutate(line = str_extract(outcome_name, "\\d+\\.?\\d*")) |>
    mutate(line = as.numeric(line) - 0.5) |>
    select(
        "match",
        "home_team",
        "away_team",
        "market_name",
        "player_name",
        "player_team",
        "line",
        "over_price" = "price",
        "agency",
        "group_by_header",
        "event_id",
        "outcome_name",
        "outcome_id",
        "fixed_market_id",
        "opposition_team"
    ) |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(player_team = fix_team_names(player_team)) |>
    mutate(opposition_team = fix_team_names(opposition_team)) |> 
    mutate(match = paste(home_team, away_team, sep = " v "))

# Get player points data--------------------------------------------------------
betright_player_points |> 
    write_csv("Data/scraped_odds/betright_player_points.csv")

# Get player rebounds data------------------------------------------------------
betright_player_rebounds |>
    write_csv("Data/scraped_odds/betright_player_rebounds.csv")

# Get player assists data-------------------------------------------------------
betright_player_assists |>
    write_csv("Data/scraped_odds/betright_player_assists.csv")
