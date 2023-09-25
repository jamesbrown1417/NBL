# Libraries
library(tidyverse)
library(rvest)
library(httr2)

# Load user functions
source("Scripts/04-helper-functions.R")

# URL to get responses
betright_url = "https://next-api.betright.com.au/Sports/Category?categoryId=110"

# Make request and get response
betright_response <-
    request(betright_url) |>
    req_perform() |> 
    resp_body_json()

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
    
    # Market info
    market_info = map(matches$markets, get_market_info) |> bind_rows()

    # Output Tibble
    tibble(
        match = match_name,
        start_time = match_start_time,
        market_name = market_info$market,
        propositions = market_info$propositions,
        prices = market_info$prices
    )
}

# Map functions to data
all_betright_markets <-
    map(betright_response$masterCategories[[1]]$categories[[1]]$masterEvents, get_match_info) |> bind_rows()

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

