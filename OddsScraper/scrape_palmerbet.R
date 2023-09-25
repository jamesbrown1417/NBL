# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# Load user functions
source("Scripts/04-helper-functions.R")

# URL of website
palmerbet_url = "https://fixture.palmerbet.online/fixtures/sports/ef7872bb-242e-4a8d-a001-b2b5c83eca05/matches?sportType=basketball&pageSize=25&channel=website"

# Make request and get response
palmerbet_response <-
    request(palmerbet_url) |>
    req_perform() |> 
    resp_body_json()

# Function to extract h2h info from response------------------------------------
get_match_info <- function(matches) {
    # Match info
    away_team_name = matches$awayTeam$title
    home_team_name = matches$homeTeam$title
    match_name = paste(home_team_name, "v", away_team_name)
    away_win = matches$awayTeam$win$price
    home_win = matches$homeTeam$win$price
    match_start_time = matches$startTime
    
    # Output Tibble
    tibble(
        match = match_name,
        start_time = match_start_time,
        home_team = home_team_name,
        away_team = away_team_name,
        home_win = home_win,
        away_win = away_win
    )
}

# Function to extract additional market info from response----------------------
get_market_info <- function(matches) {
    
    # Match info
    away_team_name = matches$awayTeam$title
    home_team_name = matches$homeTeam$title
    match_name = paste(home_team_name, "v", away_team_name)
    match_start_time = matches$startTime
    
    # Match info
    match_info <-
    tibble(
        match = match_name,
        start_time = match_start_time,
        home_team = home_team_name,
        away_team = away_team_name
    )
    
    # Empty vectors to append to
    match_names = c()
    home_teams = c()
    away_teams = c()
    start_times = c()
    market_names = c()
    market_handicaps = c()
    market_propositionss = c()
    market_pricess = c()
    market_home_aways = c()
    
    # Loop through and add to tibble
    for (index in seq_along(matches$additionalMarkets)) {
        market = matches$additionalMarkets[[index]]
        for (outcome in market$outcomes) {
            
            # Extract info
            match_name = match_info$match[[1]]
            home_team = match_info$home_team[[1]]
            away_team = match_info$away_team[[1]]
            start_time = match_info$start_time[[1]]
            market_name = market$title
            market_handicap = market$handicap
            market_propositions = outcome$title
            market_prices = outcome$price
            market_home_away = outcome$type
            
            # Append to vectors
            match_names = c(match_names, match_name)
            home_teams = c(home_teams, home_team)
            away_teams = c(away_teams, away_team)
            start_times = c(start_times, start_time)
            market_names = c(market_names, market_name)
            market_handicaps = c(market_handicaps, market_handicap)
            market_propositionss = c(market_propositionss, market_propositions)
            market_pricess = c(market_pricess, market_prices)
            market_home_aways = c(market_home_aways, market_home_away)
        }
        
    }
    
    # Output Tibble
    tibble(
        match = match_names,
        home_team = home_teams,
        away_team = away_teams,
        start_time = start_times,
        market_name = market_names,
        market_handicap = market_handicaps,
        propositions = market_propositionss,
        prices = market_pricess,
        home_away = market_home_aways
    )
}

# Map functions to data---------------------------------------------------------

# H2H
all_palmerbet_matches <-
    map(palmerbet_response$matches, get_match_info) |> bind_rows()

# Additional markets
all_palmerbet_markets <-
    map(palmerbet_response$matches, get_market_info) |> bind_rows()

# Head to head------------------------------------------------------------------

# Get DF
palmerbet_h2h <-
    all_palmerbet_matches |>
    mutate(market_name = "Head To Head") |> 
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Palmerbet")

# Fix team names
palmerbet_h2h <-
    palmerbet_h2h |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

# Write as csv
palmerbet_h2h |> 
    write_csv("Data/scraped_odds/palmerbet_h2h.csv")
