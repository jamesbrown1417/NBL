##%######################################################%##
#                                                          #
####              Scrape the 2025-2026 NBL              ####
####           season schedule from wikipedia           ####
#                                                          #
##%######################################################%##

#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(rvest)
`%notin%` <- Negate(`%in%`)

# Load user functions
source("Scripts/04-helper-functions.R")

#===============================================================================
# Scrape data
#===============================================================================

# Read in HTML Downloaded from flashscore
# nbl_schedule_html <- read_html("Data/flashscore_nbl_schedule_2025_2026.txt")

# Get Home Teams
home_teams <-
nbl_schedule_html |> 
    html_elements(".event__participant--home, .event__round") |> 
    html_text()

# Get Away Teams
away_teams <-
nbl_schedule_html |> 
    html_elements(".event__participant--away, .event__round") |> 
    html_text()

# Get Start Times
start_times <-
nbl_schedule_html |> 
    html_elements(".event__time, .event__round") |> 
    html_text()

# Function to extract data for the required round
get_round_data <- function(vector, round_number) {
    # Get strings for each round
    opening_round = paste("Round", round_number, sep = " ")
    closing_round = paste("Round", round_number + 1, sep = " ")
    
    # Perform matching
    
    # If round is not the last round
    if (round_number < 20) {
        result <- vector[which(vector == opening_round):which(vector == closing_round)]
        result[str_detect(result, "Round", negate = TRUE)]
    }
    
    # If round is the last round
    else {
        result <- vector[which(vector == opening_round):length(vector)]
        result[str_detect(result, "Round", negate = TRUE)]
    }
}

# Vector of all round numbers
round_names <- paste("Round", 1:20)
round_numbers <- 1:20

# Map function to get data for each round---------------------------------------
home_teams_by_round <-
    map(round_numbers, get_round_data, vector = home_teams) |>
    set_names(round_names)

away_teams_by_round <-
    map(round_numbers, get_round_data, vector = away_teams) |>
    set_names(round_names)

start_times_by_round <-
    map(round_numbers, get_round_data, vector = start_times) |>
    set_names(round_names)

# Combine data for each round---------------------------------------------------

# Function to convert named vector to tibble
convert_to_tibble <- function(named_vec, name) {
    tibble(value = named_vec, name = name)
}

# Convert list of named vectors to list of tibbles
home_teams_by_round <- lapply(names(home_teams_by_round), function(name) {
    convert_to_tibble(home_teams_by_round[[name]], name)
})

away_teams_by_round <- lapply(names(away_teams_by_round), function(name) {
    convert_to_tibble(away_teams_by_round[[name]], name)
})

start_times_by_round <- lapply(names(start_times_by_round), function(name) {
    convert_to_tibble(start_times_by_round[[name]], name)
})

# Combine list of tibbles into one tibble
home_teams_by_round <- bind_rows(home_teams_by_round)
away_teams_by_round <- bind_rows(away_teams_by_round)
start_times_by_round <- bind_rows(start_times_by_round)

# Bind cols
nbl_schedule <- bind_cols(home_teams_by_round, away_teams_by_round, start_times_by_round)

# Rename cols
nbl_schedule <-
    nbl_schedule %>% 
    select(round = 2, home_team = 1, away_team = 3, start_time = 5) %>% 
    mutate(start_time = if_else(
        str_sub(start_time, 4, 5) %in% c("09", "10", "11", "12"),
        str_replace(start_time, "\\d{2}\\.\\d{2}\\.", paste0(str_sub(start_time, 1, 5), ".2025")),
        str_replace(start_time, "\\d{2}\\.\\d{2}\\.", paste0(str_sub(start_time, 1, 5), ".2026"))
    )) |> 
    mutate(start_time = dmy_hm(start_time)) |> 
    arrange(start_time)

# Fix team names
nbl_schedule <-
    nbl_schedule |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |> 
    mutate(match = paste(home_team, away_team, sep = " v ")) |> 
    relocate(match, .before = round)

# Write out data
write_rds(nbl_schedule, "Data/season_schedule_2025_2026.rds")

# Get number of times a team appears in a given round---------------------------

home_count <-
nbl_schedule |> 
    group_by(home_team, round) |>
    tally()

away_count <-
nbl_schedule |> 
    group_by(away_team, round) |>
    tally()

team_games_per_round <-
full_join(home_count, away_count, by = c("home_team" = "away_team", "round" = "round")) |> 
    replace_na(list(n.x = 0, n.y = 0)) |>
    mutate(n = n.x + n.y) |>
    select(-c(n.x, n.y)) |> 
    rename(team = home_team)
