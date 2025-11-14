# Get NBL DVP

#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(readxl)
library(lubridate)

#===============================================================================
# Read in data
#===============================================================================

# Get positions
# positions <- read_excel("Data/Player-Positions.xlsx")

# Supercoach positions
positions <- read_csv("Data/supercoach-data.csv") |>
    select(player_name, player_team, position = supercoach_position_1)

# read first row of data to see date updated
date_updated <-
    read_excel("Data/combined_stats_table.xlsx", n_max = 1) |>
    pull(date_scraped)

# If date scraped is before today's date, run the script
if (as.Date(date_updated) < Sys.Date()) {
    source("Scripts/01-get-data.R")
}

# Read in the full RDS dataset
combined_stats_table <- read_rds("Data/combined_stats_table.rds")

#===============================================================================
# Reshape positions data
#===============================================================================

# Get long form of positions data
positions_long <- positions |> filter(!is.na(position))

#===============================================================================
# Parameters and pre-computation
#===============================================================================

# Target season and normalisation
season_target <- "2025-2026"        # set to desired season
per_minutes   <- 36                 # normalise per X minutes
min_minutes   <- 5                  # exclude very low-minute stints
offset_n      <- 0                  # drop last n games from each player

# Build stats table once (avoid recomputation inside get_dvp)
stats_table <-
    combined_stats_table |>
    filter(season == season_target) |>
    mutate(minutes_played = as.numeric(ms(player_minutes)) / 60) |>
    filter(minutes_played >= min_minutes) |>
    transmute(
        player_name = paste(first_name, family_name),
        player_team  = name,
        opposition   = opp_name,
        start_time   = match_time_utc,
        minutes_played,
        player_points,
        player_assists,
        player_three_pointers_made,
        player_rebounds_total,
        player_steals,
        player_blocks
    ) |>
    left_join(positions_long, by = c("player_name", "player_team")) |>
    filter(!is.na(position)) |>
    mutate(
        player_points               = per_minutes * (player_points / minutes_played),
        player_assists              = per_minutes * (player_assists / minutes_played),
        player_three_pointers_made  = per_minutes * (player_three_pointers_made / minutes_played),
        player_rebounds_total       = per_minutes * (player_rebounds_total / minutes_played),
        player_steals               = per_minutes * (player_steals / minutes_played),
        player_blocks               = per_minutes * (player_blocks / minutes_played),
        player_pras                 = player_points + player_rebounds_total + player_assists
    ) |>
    arrange(player_name, start_time) |>
    group_by(player_name) |>
    mutate(match_number = dense_rank(start_time)) |>
    mutate(games_played = max(match_number)) |>
    filter(match_number <= games_played - offset_n) |>
    ungroup()

#===============================================================================
# Function to get DVP for a position
#===============================================================================

get_dvp <- function(team, stat, stats_table) {
    # Average vs given team
    avg_vs_team <-
        stats_table |>
        filter(opposition == team) |>
        group_by(player_name, position, player_team, opposition) |>
        summarise(
            avg_points_vs  = mean(player_points, na.rm = TRUE),
            avg_assists_vs = mean(player_assists, na.rm = TRUE),
            avg_threes_vs  = mean(player_three_pointers_made, na.rm = TRUE),
            avg_rebounds_vs= mean(player_rebounds_total, na.rm = TRUE),
            avg_steals_vs  = mean(player_steals, na.rm = TRUE),
            avg_blocks_vs  = mean(player_blocks, na.rm = TRUE),
            avg_pras_vs    = mean(player_pras, na.rm = TRUE),
            .groups = "drop"
        )

    # Average vs all other teams
    avg_vs_others <-
        stats_table |>
        filter(opposition != team) |>
        group_by(player_name, position, player_team) |>
        summarise(
            avg_points_others   = mean(player_points, na.rm = TRUE),
            avg_assists_others  = mean(player_assists, na.rm = TRUE),
            avg_threes_others   = mean(player_three_pointers_made, na.rm = TRUE),
            avg_rebounds_others = mean(player_rebounds_total, na.rm = TRUE),
            avg_steals_others   = mean(player_steals, na.rm = TRUE),
            avg_blocks_others   = mean(player_blocks, na.rm = TRUE),
            avg_pras_others     = mean(player_pras, na.rm = TRUE),
            .groups = "drop"
        )

    # Join and compute diffs
    dvp_data <-
        avg_vs_team |>
        left_join(avg_vs_others, by = c("player_name", "position", "player_team")) |>
        transmute(
            player_name,
            position,
            player_team,
            opposition,
            point_diff   = avg_points_vs  - avg_points_others,
            assist_diff  = avg_assists_vs - avg_assists_others,
            three_diff   = avg_threes_vs  - avg_threes_others,
            rebound_diff = avg_rebounds_vs- avg_rebounds_others,
            steal_diff   = avg_steals_vs  - avg_steals_others,
            block_diff   = avg_blocks_vs  - avg_blocks_others,
            pra_diff     = avg_pras_vs    - avg_pras_others
        )

    # Summarise for desired stat
    if (stat == "points") {
        dvp_data |>
            group_by(position, opposition) |>
            summarise(games = n(), avg_points = mean(point_diff, na.rm = TRUE), .groups = "drop") |>
            arrange(position, desc(avg_points))
    } else if (stat == "rebounds") {
        dvp_data |>
            group_by(position, opposition) |>
            summarise(games = n(), avg_rebounds = mean(rebound_diff, na.rm = TRUE), .groups = "drop") |>
            arrange(position, desc(avg_rebounds))
    } else if (stat == "threes") {
        dvp_data |>
            group_by(position, opposition) |>
            summarise(games = n(), avg_threes = mean(three_diff, na.rm = TRUE), .groups = "drop") |>
            arrange(position, desc(avg_threes))
    } else if (stat == "assists") {
        dvp_data |>
            group_by(position, opposition) |>
            summarise(games = n(), avg_assists = mean(assist_diff, na.rm = TRUE), .groups = "drop") |>
            arrange(position, desc(avg_assists))
    } else if (stat == "steals") {
        dvp_data |>
            group_by(position, opposition) |>
            summarise(games = n(), avg_steals = mean(steal_diff, na.rm = TRUE), .groups = "drop") |>
            arrange(position, desc(avg_steals))
    } else if (stat == "blocks") {
        dvp_data |>
            group_by(position, opposition) |>
            summarise(games = n(), avg_blocks = mean(block_diff, na.rm = TRUE), .groups = "drop") |>
            arrange(position, desc(avg_blocks))
    } else { # pras
        dvp_data |>
            group_by(position, opposition) |>
            summarise(games = n(), avg_pras = mean(pra_diff, na.rm = TRUE), .groups = "drop") |>
            arrange(position, desc(avg_pras))
    }
}
    
# Get team list from data actually present
team_list <- stats_table |> pull(opposition) |> unique()

#===============================================================================
# Get DVP for each stat
#===============================================================================

# Get points DVP
points_dvp <-
    team_list |>
    map_df(get_dvp, stat = "points", stats_table = stats_table) |>
    arrange(position, desc(avg_points))

# Get rebounds DVP
rebounds_dvp <-
    team_list |>
    map_df(get_dvp, stat = "rebounds", stats_table = stats_table) |>
    arrange(position, desc(avg_rebounds))

# Get assists DVP
assists_dvp <-
    team_list |>
    map_df(get_dvp, stat = "assists", stats_table = stats_table) |>
    arrange(position, desc(avg_assists))

# Get Threes DVP
threes_dvp <-
    team_list |>
    map_df(get_dvp, stat = "threes", stats_table = stats_table) |>
    arrange(position, desc(avg_threes))

# Get Steals DVP
steals_dvp <-
    team_list |>
    map_df(get_dvp, stat = "steals", stats_table = stats_table) |>
    arrange(position, desc(avg_steals))

# Get Blocks DVP
blocks_dvp <-
    team_list |>
    map_df(get_dvp, stat = "blocks", stats_table = stats_table) |>
    arrange(position, desc(avg_blocks))

# Get PRAs DVP
pras_dvp <-
    team_list |>
    map_df(get_dvp, stat = "pras", stats_table = stats_table) |>
    arrange(position, desc(avg_pras))

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

# Create threes heatmap
threes_heatmap <-
    threes_dvp |> 
    ggplot(aes(x = position, y = opposition, fill = avg_threes)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.position = "none") +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL, title = "Player Threes") +
    geom_text(aes(label = round(avg_threes, 1)), size = 3)

#===============================================================================
# Persist outputs as RDS for reuse
#===============================================================================

invisible(dir.create("Data/processed_stats", recursive = TRUE, showWarnings = FALSE))

# Individual RDS files
write_rds(points_dvp,   file = file.path("Data/processed_stats", "dvp_points.rds"))
write_rds(rebounds_dvp, file = file.path("Data/processed_stats", "dvp_rebounds.rds"))
write_rds(assists_dvp,  file = file.path("Data/processed_stats", "dvp_assists.rds"))
write_rds(threes_dvp,   file = file.path("Data/processed_stats", "dvp_threes.rds"))
write_rds(steals_dvp,   file = file.path("Data/processed_stats", "dvp_steals.rds"))
write_rds(blocks_dvp,   file = file.path("Data/processed_stats", "dvp_blocks.rds"))
write_rds(pras_dvp,     file = file.path("Data/processed_stats", "dvp_pras.rds"))

# Combined long-form for quick plotting later
dvp_all <- bind_rows(
    points_dvp  |> mutate(stat = "points",   value = avg_points)  |> select(opposition, position, games, stat, value),
    rebounds_dvp|> mutate(stat = "rebounds", value = avg_rebounds)|> select(opposition, position, games, stat, value),
    assists_dvp |> mutate(stat = "assists",  value = avg_assists) |> select(opposition, position, games, stat, value),
    threes_dvp  |> mutate(stat = "threes",   value = avg_threes)  |> select(opposition, position, games, stat, value),
    steals_dvp  |> mutate(stat = "steals",   value = avg_steals)  |> select(opposition, position, games, stat, value),
    blocks_dvp  |> mutate(stat = "blocks",   value = avg_blocks)  |> select(opposition, position, games, stat, value),
    pras_dvp    |> mutate(stat = "pras",     value = avg_pras)    |> select(opposition, position, games, stat, value)
)

write_rds(dvp_all, file = file.path("Data/processed_stats", "dvp_all.rds"))

# Named list bundle for convenience
dvp_results <- list(
    season   = season_target,
    per      = per_minutes,
    min_min  = min_minutes,
    offset   = offset_n,
    points   = points_dvp,
    rebounds = rebounds_dvp,
    assists  = assists_dvp,
    threes   = threes_dvp,
    steals   = steals_dvp,
    blocks   = blocks_dvp,
    pras     = pras_dvp,
    all      = dvp_all
)

write_rds(dvp_results, file = file.path("Data/processed_stats", "dvp_results.rds"))

message("DVP tables saved to Data/processed_stats as RDS files.")
