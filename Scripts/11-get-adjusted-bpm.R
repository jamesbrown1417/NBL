#===============================================================================
# Function to Calculate Offensive Pace for Both Teams in a Match
#===============================================================================

# Data and libraries
library(tidyverse)
library(furrr)

# Set up parallel processing
plan(multisession)

# PBP Data
all_pbp_data <- nblR::nbl_pbp()

# Get only the data for the current season
current_season_pbp <-
    all_pbp_data |>
    filter(season == "2025-2026")

calculate_adjusted_bpm <- function(match_id_num) {
    # Get Match PBP Data
    match_pbp <- current_season_pbp |>
        filter(match_id == match_id_num) |> 
        arrange(action_number)
    
    # Get Each Player in the Match
    players_in_match <- match_pbp |>
        distinct(scoreboard_name) |> 
        filter(scoreboard_name != "<NA>")
    
    # Create a function that takes a scoreboard name and gets their on-court time
    get_substitutions_data <- function(player_name) {
        # Get dataset of all substitutions for the player
        player_on_court_data <- match_pbp |>
            filter(scoreboard_name == player_name) |> 
            arrange(action_number)
        
        # Get the maximum for period and period_type
        max_period <- last(match_pbp$period)
        max_period_type <- last(match_pbp$period_type)
        
        # Get all substitution data for the player
        player_substitution_data <-
            player_on_court_data |> 
            filter(action_type == "substitution") |>
            arrange(action_number) |> 
            select(first_name, family_name, period, period_type, time = gt, action_type, sub_type)
        
        # If first action is a substitution out create a row for the start of the match
        if(player_substitution_data$sub_type[1] == "out") {
            player_substitution_data <- 
                player_substitution_data |> 
                add_row(first_name = player_substitution_data$first_name[1],
                        family_name = player_substitution_data$family_name[1],
                        period = 1,
                        period_type = "REGULAR",
                        time = "10:00",
                        action_type = "substitution",
                        sub_type = "in") |>
                arrange(period, desc(time))
        }
        
        # If last action is a substitution in create a row for the end of the match
        if(player_substitution_data$sub_type[nrow(player_substitution_data)] == "in") {
            player_substitution_data <- 
                player_substitution_data |> 
                add_row(first_name = player_substitution_data$first_name[nrow(player_substitution_data)],
                        family_name = player_substitution_data$family_name[nrow(player_substitution_data)],
                        period = max_period,
                        period_type = max_period_type,
                        time = "00:00",
                        action_type = "substitution",
                        sub_type = "out")
        }
        
        # If length of player substitution data is zero but length of player_on_court_data is not zero, player was on court the whole match
        if(nrow(player_substitution_data) == 0 & nrow(player_on_court_data) != 0) {
            player_substitution_data <-
            player_substitution_data |> 
                add_row(first_name = player_substitution_data$first_name[1],
                        family_name = player_substitution_data$family_name[1],
                        period = 1,
                        period_type = "REGULAR",
                        time = "10:00",
                        action_type = "substitution",
                        sub_type = "in") |>
                arrange(period, desc(time)) |>
                add_row(first_name = player_substitution_data$first_name[1],
                        family_name = player_substitution_data$family_name[1],
                        period = max_period,
                        period_type = max_period_type,
                        time = "00:00",
                        action_type = "substitution",
                        sub_type = "out")
        }
        
        return(player_substitution_data)
    }
    
    # Get substitution data for each player
    player_substitution_data <- future_map_dfr(players_in_match$scoreboard_name, get_substitutions_data)
    
}
