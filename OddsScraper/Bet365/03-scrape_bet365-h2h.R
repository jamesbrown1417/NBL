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

# Get Start Time
bet365_start_time <-
    read_html(scraped_file) |> 
    html_nodes(".sgl-MarketFixtureDetailsLabel") |>
    html_nodes(".rcl-MarketHeaderLabel-isdate, .scb-ParticipantFixtureDetailsHigherBasketball_TeamWrapper") |> 
    html_text()

# Get indices that contain a number surrounded by spaces
start_time_indices <- which(str_detect(bet365_start_time, " \\d+ "))

# Empty list
result_list <- list()

# Split into chunks from each index to the next and from the last to the end of the vector
for (i in 1:length(start_time_indices)) {
    if (i != length(start_time_indices)) {
        result = (bet365_start_time[start_time_indices[i]:start_time_indices[i + 1]])
        result = result[-length(result)]
    } else {
        result = (bet365_start_time[start_time_indices[i]:length(bet365_start_time)])
    }
    result_list[[i]] <- result
}

# Turn each vector into a tibble
start_dates <- map(result_list, ~ expand_grid(.x[1], .x[2:length(.x)])) |> bind_rows()
names(start_dates) <- c("start_date", "team")

#===============================================================================
# Create head to head table----------------------------------------------------#
#===============================================================================

# Get Home teams - Odd elements
home_teams <- bet365_teams[seq(1, length(bet365_teams), 2)]
home_odds <- bet365_h2h_odds[seq(1, length(bet365_h2h_odds), 2)]

home_h2h <- tibble(home_teams, home_odds) |>
    left_join(start_dates, by = c("home_teams" = "team"))

# Get Away teams - Even elements
away_teams <- bet365_teams[seq(2, length(bet365_teams), 2)]
away_odds <- bet365_h2h_odds[seq(2, length(bet365_h2h_odds), 2)]

away_h2h <- tibble(away_teams, away_odds) |>
    left_join(start_dates, by = c("away_teams" = "team"))

# Combine together into one table
bet365_h2h <-
    bind_cols(home_h2h, away_h2h) |>
    mutate(home_teams = fix_team_names(home_teams),
           away_teams = fix_team_names(away_teams)) |>
    transmute(match = paste(home_teams, away_teams, sep = " v "),
              start_time = `start_date...3`,
              market_name = "Head To Head",
              home_team = home_teams,
              home_win = as.numeric(home_odds),
              away_team = away_teams,
              away_win = as.numeric(away_odds)) |>
    mutate(start_time = dmy(paste(start_time, "2023"))) |> 
    mutate(start_time = if_else(month(start_time) < 9, start_time + years(1), start_time)) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Bet365")

# Write to csv
write_csv(bet365_h2h, "Data/scraped_odds/bet365_h2h.csv")
}

get_head_to_head()