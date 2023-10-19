#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(rstanarm)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Get Training Data (Exclude Most Recent Round)
#===============================================================================

# read first row of data to see date updated
date_updated <-
    read_excel("Data/combined_stats_table.xlsx", n_max = 1) |>
    pull(date_scraped)

# If date scraped is before todays date, run the script
if (date_updated < ymd(Sys.Date())) {
    source("Scripts/01-get-data.R")
}

# Read in the full RDS dataset
combined_stats_table <- read_rds("Data/combined_stats_table.rds")

# Filter to this season and get only relevant columns
player_points_data <- 
    combined_stats_table |>
    filter(season == "2023-2024") |>
    mutate(minutes_played = period_to_seconds(ms(player_minutes)) / 60) |> 
    filter(minutes_played >= 5) |> 
    mutate(player_name = paste(first_name, family_name)) |>
    select(
        match_id,
        player_name,
        player_team = name,
        opposition = opp_name,
        start_time = match_time_utc,
        minutes_played,
        player_points
    ) |> 
    arrange(player_name, start_time)

# Get variables as of last round and then add to this round's outcomes
player_points_predictors <- 
    player_points_data |>
    group_by(player_name, player_team) |> 
    filter(start_time < max(start_time)) |> 
    filter(player_points > 0) |> 
    mutate(player_points = player_points / minutes_played) |> 
    summarise(avg_mins = mean(minutes_played),
              avg_points = mean(player_points),
              min_points = min(player_points),
              max_points = max(player_points),
              var_points = var(player_points),
              n_games = n())
    
player_points_model_data <-
    player_points_data |>
    group_by(player_name, player_team) |> 
    filter(start_time == max(start_time)) |> 
    ungroup() |> 
    left_join(player_points_predictors, by = c("player_name", "player_team")) |> 
    filter(n_games == 4)

# Get current round data to get predictions for
player_points_data_current_round <- 
    player_points_data |>
    group_by(player_name, player_team) |> 
    filter(player_points > 0) |> 
    mutate(player_points = player_points / minutes_played) |> 
    summarise(avg_mins = mean(minutes_played),
              avg_points = mean(player_points),
              min_points = min(player_points),
              max_points = max(player_points),
              var_points = var(player_points),
              n_games = n()) |> 
    filter(n_games == 5)

#===============================================================================
# Fit a Poisson Model
#===============================================================================

# Fit the model
player_points_model <-
    stan_glm(
        player_points ~ avg_mins*avg_points + var_points,
        data = player_points_model_data,
        family = poisson(link = "log")
    )

# Check model fit
pp_check(player_points_model)

# Get posterior predictions for current round
current_round_predictions <- posterior_predict(player_points_model, newdata = player_points_data_current_round)

# Get probabilities of 5, 10, 15, 20 and 25+ for each player
current_round_probs_5 <-
apply(current_round_predictions, 2, function(x) {
    sum(x >= 5) / length(x)
})

current_round_probs_10 <-
apply(current_round_predictions, 2, function(x) {
    sum(x >= 10) / length(x)
})

current_round_probs_15 <-
apply(current_round_predictions, 2, function(x) {
    sum(x >= 15) / length(x)
})

current_round_probs_20 <-
apply(current_round_predictions, 2, function(x) {
    sum(x >= 20) / length(x)
})

current_round_probs_25 <-
apply(current_round_predictions, 2, function(x) {
    sum(x >= 25) / length(x)
})

# Combine into a dataframe
current_round_probs <-
    data.frame(
        player_name = player_points_data_current_round$player_name,
        player_team = player_points_data_current_round$player_team,
        current_round_probs_5,
        current_round_probs_10,
        current_round_probs_15,
        current_round_probs_20,
        current_round_probs_25
    )
