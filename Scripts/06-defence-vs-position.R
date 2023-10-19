# Get NBL DVP

#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(readxl)

#===============================================================================
# Read in data
#===============================================================================

# Get positions
positions <- read_excel("Data/Player-Positions.xlsx")

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

#===============================================================================
# Reshape positions data
#===============================================================================

# Get long form of positions data
positions_long <-
positions |> 
    pivot_longer(cols = c(position_1, position_2), names_to = "position_number", values_to = "position") |> 
    select(-position_number, -Rating) |> 
    filter(!is.na(position))

# Consolidate Wing and Athletic Wing into Wing
positions_long <-
positions_long |> 
    mutate(position = ifelse(position %in% c("Wing", "Athletic Wing"), "Wing", position))
    
#===============================================================================
# Function to get DVP for a position
#===============================================================================

get_dvp <- function(team, stat, offset = 0) {
    # Stats
    stats_table <-
    combined_stats_table |> 
        filter(season == "2023-2024") |> 
        mutate(minutes_played = period_to_seconds(ms(player_minutes)) / 60) |> 
        filter(minutes_played >= 5) |> 
        transmute(player_name = paste(first_name, family_name),
                  player_team = name,
                  opposition = opp_name,
                  start_time = match_time_utc,
                  minutes_played,
                  player_points,
                  player_assists,
                  player_rebounds_total) |> 
        left_join(positions_long, by = c("player_name", "player_team"), relationship = "many-to-many") |> 
        filter(!is.na(position)) |>
        mutate(player_points = 36 * (player_points / minutes_played),
               player_assists = 36 * (player_assists / minutes_played),
               player_rebounds_total = 36 * (player_rebounds_total / minutes_played)) |> 
        arrange(player_name, start_time) |> 
        group_by(player_name) |> 
        mutate(match_number = dense_rank(start_time)) |> 
        mutate(games_played = max(match_number)) |> 
        filter(match_number <= games_played - offset)
    
    # Get average vs team
    avg_vs_team <-
        stats_table |> 
        filter(opposition == team) |>
        group_by(player_name, position, player_team, opposition) |> 
        summarise(avg_points_vs = mean(player_points, na.rm = TRUE),
                  avg_assists_vs = mean(player_assists, na.rm = TRUE),
                  avg_rebounds_vs = mean(player_rebounds_total, na.rm = TRUE))
    
    # Get average vs all other teams
    avg_vs_others <-
        stats_table |> 
        filter(opposition != team) |>
        group_by(player_name, position, player_team) |> 
        summarise(avg_points_others = mean(player_points, na.rm = TRUE),
                  avg_assists_others = mean(player_assists, na.rm = TRUE),
                  avg_rebounds_others = mean(player_rebounds_total, na.rm = TRUE))
    
    # Join Together
    dvp_data <-
        avg_vs_team |>
        left_join(avg_vs_others, by = c("player_name", "position", "player_team")) |> 
        transmute(player_name,
                  position,
                  player_team,
                  opposition,
                  point_diff = avg_points_vs - avg_points_others,
                  assist_diff = avg_assists_vs - avg_assists_others,
                  rebound_diff = avg_rebounds_vs - avg_rebounds_others)
    
    # Get for desired stat
    if (stat == "points") {
        dvp_data |> 
            group_by(position, opposition) |>
            summarise(games = n(),
                      avg_points = mean(point_diff, na.rm = TRUE)) |> 
            arrange(desc(avg_points))
    } else if (stat == "rebounds") {
        dvp_data |> 
            group_by(position, opposition) |>
            summarise(games = n(),
                      avg_rebounds = mean(rebound_diff, na.rm = TRUE)) |> 
            arrange(desc(avg_rebounds))
    } else {
        dvp_data |> 
            group_by(position, opposition) |>
            summarise(
                games = n(),
                avg_assists = mean(assist_diff, na.rm = TRUE)) |> 
            arrange(desc(avg_assists))
    }
}
    
# Get team list
team_list <-
    positions |> 
    pull(player_team) |> 
    unique()

#===============================================================================
# Get DVP for each stat
#===============================================================================

# Get points DVP
points_dvp <-
    team_list |> 
    map_df(get_dvp, stat = "points") |> 
    arrange(position, desc(avg_points))

# Get rebounds DVP
rebounds_dvp <-
    team_list |> 
    map_df(get_dvp, stat = "rebounds") |> 
    arrange(position, desc(avg_rebounds))

# Get assists DVP
assists_dvp <-
    team_list |> 
    map_df(get_dvp, stat = "assists") |> 
    arrange(position, desc(avg_assists))

#===============================================================================
# Create Heatmaps
#===============================================================================

# Create points heatmap
points_heatmap <-
    points_dvp |> 
    ggplot(aes(x = position, y = opposition, fill = avg_points)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.position = "none") +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL, title = "Player Points") +
    geom_text(aes(label = round(avg_points, 1)), size = 3)

# Create rebounds heatmap
rebounds_heatmap <-
    rebounds_dvp |> 
    ggplot(aes(x = position, y = opposition, fill = avg_rebounds)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.position = "none") +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL, title = "Player Rebounds") +
    geom_text(aes(label = round(avg_rebounds, 1)), size = 3)

# Create assists heatmap
assists_heatmap <-
    assists_dvp |> 
    ggplot(aes(x = position, y = opposition, fill = avg_assists)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.position = "none") +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL, title = "Player Assists") +
    geom_text(aes(label = round(avg_assists, 1)), size = 3)
    
