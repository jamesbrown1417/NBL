# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

# Load user functions
source("Scripts/04-helper-functions.R")

# URL of website
sportsbet_url = "https://www.sportsbet.com.au/betting/basketball-aus-other/australian-nbl"

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

# Get data from main market page
matches <-
    sportsbet_url |> 
    read_html() |>
    html_nodes(".multiMarketCouponContainer_f234ak7")
    
# Function to get team names
get_team_names <- function(match) {
    team_names <-
        match |>
        html_nodes(".participantText_fivg86r") |>
        html_text()
    
    # Home team
    home_team <- team_names[1]
    away_team <- team_names[2]
    
    # Output
    tibble(home_team, away_team)
}

# Function to get odds
get_odds <- function(match) {
    odds <-
        match |>
        html_nodes(".priceTextSize_frw9zm9") |>
        html_text() |>
        as.numeric()
    
    # Home team
    home_win <- odds[1]
    away_win <- odds[2]
    
    # Output
    tibble(home_win, away_win)
}

# Function to get start time
get_start_time <- function(match) {
    start_time <-
        match |>
        html_nodes(".oneLine_f15ay66x") |>
        html_text()
    
    # Output
    tibble(start_time)
}

# Map functions to each match and combine together
all_main_market_data <-
bind_cols(
    map(matches, get_team_names) |> bind_rows(),
    map(matches, get_odds) |> bind_rows(),
    map(matches, get_start_time) |> bind_rows()
)

#===============================================================================
# Head to Head markets---------------------------------------------------------#
#===============================================================================

sportsbet_h2h <-
all_main_market_data |>
    mutate(home_team = fix_team_names(home_team),
           away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(market_name = "Head To Head") |>
    mutate(home_win = as.numeric(home_win)) |>
    mutate(away_win = as.numeric(away_win)) |>
    select(match,
           start_time,
           market_name,
           home_team,
           home_win,
           away_team,
           away_win) |>
    mutate(margin = round((1 / home_win + 1 / away_win), digits = 3)) |>
    mutate(agency = "Sportsbet") |>
    mutate(start_time = str_extract(start_time, "\\,.*")) |> 
    mutate(start_time = str_remove(start_time, "\\, ")) |>
    mutate(start_time = str_remove(start_time, " \\d{2}\\:\\d{2}")) |> 
    mutate(start_time = dmy(paste(start_time, "2023"))) |> 
    mutate(start_time = if_else(month(start_time) < 9, start_time + years(1), start_time))

# Write to csv
write_csv(sportsbet_h2h, "Data/scraped_odds/sportsbet_h2h.csv")
