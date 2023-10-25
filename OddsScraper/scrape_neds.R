# Libraries
library(tidyverse)
library(rvest)
library(httr2)

# Load user functions
source("Scripts/04-helper-functions.R")

# Get player name and team data
player_names_teams <-
    read_csv("Data/supercoach-data.csv") |> 
    mutate(first_initial = str_sub(player_first_name, 1, 1)) |>
    select(player_first_name, first_initial, player_last_name, player_team) |> 
    mutate(player_name_initials = paste(first_initial, player_last_name, sep = " "))

# URL to get responses
neds_url = "https://api.neds.com.au/v2/sport/event-request?category_ids=%5B%223c34d075-dc14-436d-bfc4-9272a49c2b39%22%5D&include_any_team_vs_any_team_events=true"

# Make request and get response
neds_response <-
    request(neds_url) |>
    req_perform() |> 
    resp_body_json()

# Initialize empty lists to store data
event_name <- character()
event_id <- character()
competition_name <- character()

# Extract event IDs and names from JSON response
for (value in neds_response$events) {
    event_name <- c(event_name, value$name)
    event_id <- c(event_id, value$id)
    competition_name <- c(competition_name, value$competition$name)
}

# Create a data frame from the vectors
df <- data.frame(event_name, event_id, competition_name)

# Filter the data frame to only include matches with ' vs ' in the event name
df <- df %>% filter(str_detect(event_name, ' v '))

# Only get NBL Games
df <- df %>% filter(str_detect(competition_name, 'Australian NBL'))

#===============================================================================
# Get event card data for each match
#===============================================================================

# Base URL for event card
event_url <- "https://api.neds.com.au/v2/sport/event-card?id="

# List of event URLs
event_json <- paste0(event_url, df$event_id)

# Initialize an empty list to store event JSON data
event_json_list <- list()

# Loop through each event URL and get the event card JSON data
for (url in event_json) {
    tryCatch({
        response2 <- request(url) %>%
            req_perform() %>%
            resp_body_json()
        event_json_list <- append(event_json_list, list(response2))
    }, error = function(e) {
        cat("Error:", url, "\n")
    })
}

#===============================================================================
# Get the market information for each match
#===============================================================================

# Initialize empty vectors to store the market names and IDs for mapping
market_lookup_name <- character()
market_lookup_id <- character()

# Initialize empty vectors to store data
entrants <- character()
market_id <- character()
match_names <- character()
prices <- numeric()

# Loop through the entrants
for (i in seq_along(event_json_list)) {
    match_name <- df$event_name[i]
    match <- event_json_list[[i]]
    
    for (entrant in match$entrants) {
        entrants <- c(entrants, entrant$name)
        market_id <- c(market_id, entrant$market_id)
        match_names <- c(match_names, match_name)
    }
    
    # Loop through the markets
    for (market in match$markets) {
        market_lookup_name <- c(market_lookup_name, market$name)
        market_lookup_id <- c(market_lookup_id, market$id)
    }
    
    # Loop through the prices
    for (price in match$prices) {
        fractional_odds <- price$odds$numerator / price$odds$denominator
        decimal_odds <- fractional_odds + 1
        prices <- c(prices, decimal_odds)
    }
}

# Create market lookup dataframe
market_lookup_df <- data.frame(market_id = market_lookup_id, market_name = market_lookup_name)

# Create market dataframe
market_df <- data.frame(match_name = match_names, market_id = market_id, entrants = entrants, price = prices)

# Merge market lookup dataframe with market dataframe
market_df <- merge(market_df, market_lookup_df, by = 'market_id', all.x = TRUE)

# Reorder columns in market_df
market_df <- market_df %>% select(match_name, market_name, entrants, price)
