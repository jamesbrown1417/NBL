# Libraries
library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)

# Load user functions
source("Scripts/04-helper-functions.R")

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

get_head_to_head <- function() {

# Read scraped HTML from the BET365_HTML Folder
scraped_file <- list.files("OddsScraper/Bet365/HTML", full.names = TRUE, pattern = "h2h")

# Get Teams
bet365_teams <-
    read_html(scraped_file) |> 
    html_nodes(".scb-ParticipantFixtureDetailsHigherBasketball_TeamWrapper") |> 
    html_text()

# Get H2H Odds
bet365_h2h_odds <-
    read_html(scraped_file) |> 
    html_nodes(".sac-ParticipantOddsOnly50OTB") |> 
    html_text()

#===============================================================================
# Create head to head table----------------------------------------------------#
#===============================================================================

# Get Home teams - Odd elements
home_teams <- bet365_teams[seq(1, length(bet365_teams), 2)]
home_odds <- bet365_h2h_odds[seq(1, length(bet365_h2h_odds), 2)]

home_h2h <- tibble(home_teams, home_odds)

# Get Away teams - Even elements
away_teams <- bet365_teams[seq(2, length(bet365_teams), 2)]
away_odds <- bet365_h2h_odds[seq(2, length(bet365_h2h_odds), 2)]

away_h2h <- tibble(away_teams, away_odds)

# Combine together into one table
bet365_h2h <-
    bind_cols(home_h2h, away_h2h) |>
    mutate(home_teams = fix_team_names(home_teams),
           away_teams = fix_team_names(away_teams)) |>
    transmute(match = paste(home_teams, away_teams, sep = " v "),
              market_name = "Head To Head",
              home_team = home_teams,
              home_win = as.numeric(home_odds),
              away_team = away_teams,
              away_win = as.numeric(away_odds)) |>
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Bet365")

# Write to csv
write_csv(bet365_h2h, "Data/scraped_odds/bet365_h2h.csv")
}

get_head_to_head()