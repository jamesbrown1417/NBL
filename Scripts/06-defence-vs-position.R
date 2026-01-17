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

# Supercoach positions
positions <- read_csv("Data/supercoach-data.csv") |>
    select(player_name, player_team, position = supercoach_position_1) |>
    filter(!is.na(position))

# Read first row of data to see date updated
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
# Parameters
#===============================================================================

season_target <- "2025-2026"        # set to desired season
per_minutes   <- 36                 # normalise per X minutes
min_minutes   <- 5                  # exclude very low-minute stints
offset_n      <- 0                  # drop last n games from each player
min_games     <- 2                  # minimum games vs opponent for inclusion

# Stat configuration: maps stat name to source column and diff column
stat_config <- list(
    points   = list(src = "player_points",              diff = "point_diff"),
    rebounds = list(src = "player_rebounds_total",      diff = "rebound_diff"),
    assists  = list(src = "player_assists",             diff = "assist_diff"),
    threes   = list(src = "player_three_pointers_made", diff = "three_diff"),
    steals   = list(src = "player_steals",              diff = "steal_diff"),
    blocks   = list(src = "player_blocks",              diff = "block_diff"),
    pras     = list(src = "player_pras",                diff = "pra_diff")
)

#===============================================================================
# Build stats table
#===============================================================================

stats_table <-
    combined_stats_table |>
    filter(season == season_target) |>
    mutate(minutes_played = as.numeric(ms(player_minutes)) / 60) |>
    filter(minutes_played >= min_minutes) |>
    transmute(
        player_name  = paste(first_name, family_name),
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
    inner_join(positions, by = c("player_name", "player_team")) |>
    mutate(
        player_points              = per_minutes * (player_points / minutes_played),
        player_assists             = per_minutes * (player_assists / minutes_played),
        player_three_pointers_made = per_minutes * (player_three_pointers_made / minutes_played),
        player_rebounds_total      = per_minutes * (player_rebounds_total / minutes_played),
        player_steals              = per_minutes * (player_steals / minutes_played),
        player_blocks              = per_minutes * (player_blocks / minutes_played),
        player_pras                = player_points + player_rebounds_total + player_assists
    ) |>
    arrange(player_name, start_time) |>
    group_by(player_name) |>
    mutate(
        match_number = dense_rank(start_time),
        games_played = max(match_number)
    ) |>
    filter(match_number <= games_played - offset_n) |>
    ungroup()

# Get team list from data
team_list <- stats_table |> pull(opposition) |> unique()

#===============================================================================
# Pre-compute career averages (computed once, used for all teams)
#===============================================================================

career_avgs <- stats_table |>
    group_by(player_name, position, player_team) |>
    summarise(
        total_games            = n(),
        sum_points             = sum(player_points, na.rm = TRUE),
        sum_assists            = sum(player_assists, na.rm = TRUE),
        sum_threes             = sum(player_three_pointers_made, na.rm = TRUE),
        sum_rebounds           = sum(player_rebounds_total, na.rm = TRUE),
        sum_steals             = sum(player_steals, na.rm = TRUE),
        sum_blocks             = sum(player_blocks, na.rm = TRUE),
        sum_pras               = sum(player_pras, na.rm = TRUE),
        .groups = "drop"
    )

#===============================================================================
# Function to compute all DVP diffs for a single team
#===============================================================================

get_dvp_all <- function(team, stats_table, career_avgs) {
    # Average vs given team
    avg_vs_team <- stats_table |>
        filter(opposition == team) |>
        group_by(player_name, position, player_team) |>
        summarise(
            games_vs               = n(),
            avg_points_vs          = mean(player_points, na.rm = TRUE),
            avg_assists_vs         = mean(player_assists, na.rm = TRUE),
            avg_threes_vs          = mean(player_three_pointers_made, na.rm = TRUE),
            avg_rebounds_vs        = mean(player_rebounds_total, na.rm = TRUE),
            avg_steals_vs          = mean(player_steals, na.rm = TRUE),
            avg_blocks_vs          = mean(player_blocks, na.rm = TRUE),
            avg_pras_vs            = mean(player_pras, na.rm = TRUE),
            sum_points_vs          = sum(player_points, na.rm = TRUE),
            sum_assists_vs         = sum(player_assists, na.rm = TRUE),
            sum_threes_vs          = sum(player_three_pointers_made, na.rm = TRUE),
            sum_rebounds_vs        = sum(player_rebounds_total, na.rm = TRUE),
            sum_steals_vs          = sum(player_steals, na.rm = TRUE),
            sum_blocks_vs          = sum(player_blocks, na.rm = TRUE),
            sum_pras_vs            = sum(player_pras, na.rm = TRUE),
            .groups = "drop"
        )

    # Join with career totals and compute "vs others" by subtraction
    avg_vs_team |>
        inner_join(career_avgs, by = c("player_name", "position", "player_team")) |>
        mutate(
            games_others = total_games - games_vs,
            # Compute "vs others" averages by subtracting "vs team" from career totals
            avg_points_others   = (sum_points - sum_points_vs) / games_others,
            avg_assists_others  = (sum_assists - sum_assists_vs) / games_others,
            avg_threes_others   = (sum_threes - sum_threes_vs) / games_others,
            avg_rebounds_others = (sum_rebounds - sum_rebounds_vs) / games_others,
            avg_steals_others   = (sum_steals - sum_steals_vs) / games_others,
            avg_blocks_others   = (sum_blocks - sum_blocks_vs) / games_others,
            avg_pras_others     = (sum_pras - sum_pras_vs) / games_others
        ) |>
        # Only include players who have played other teams too
        filter(games_others > 0) |>
        transmute(
            player_name,
            position,
            player_team,
            opposition   = team,
            games_vs,
            point_diff   = avg_points_vs - avg_points_others,
            assist_diff  = avg_assists_vs - avg_assists_others,
            three_diff   = avg_threes_vs - avg_threes_others,
            rebound_diff = avg_rebounds_vs - avg_rebounds_others,
            steal_diff   = avg_steals_vs - avg_steals_others,
            block_diff   = avg_blocks_vs - avg_blocks_others,
            pra_diff     = avg_pras_vs - avg_pras_others
        )
}

#===============================================================================
# Compute all DVP diffs in one pass
#===============================================================================

dvp_all_diffs <- team_list |>
    map_df(get_dvp_all, stats_table = stats_table, career_avgs = career_avgs)

#===============================================================================
# Function to summarise DVP for a specific stat
#===============================================================================

summarise_dvp <- function(dvp_data, stat_name, stat_config, min_games) {
    diff_col <- stat_config[[stat_name]]$diff
    avg_col  <- paste0("avg_", stat_name)

    dvp_data |>
        group_by(position, opposition) |>
        summarise(
            games = n(),
            !!avg_col := mean(.data[[diff_col]], na.rm = TRUE),
            .groups = "drop"
        ) |>
        filter(games >= min_games) |>
        arrange(position, desc(.data[[avg_col]]))
}

#===============================================================================
# Generate DVP tables for all stats using iteration
#===============================================================================

dvp_list <- names(stat_config) |>
    set_names() |>
    map(~summarise_dvp(dvp_all_diffs, .x, stat_config, min_games))

# Extract individual tables for backward compatibility
points_dvp   <- dvp_list$points
rebounds_dvp <- dvp_list$rebounds
assists_dvp  <- dvp_list$assists
threes_dvp   <- dvp_list$threes
steals_dvp   <- dvp_list$steals
blocks_dvp   <- dvp_list$blocks
pras_dvp     <- dvp_list$pras

#===============================================================================
# Persist outputs as RDS for reuse
#===============================================================================

invisible(dir.create("Data/processed_stats", recursive = TRUE, showWarnings = FALSE))

# Save individual RDS files using iteration
iwalk(dvp_list, ~write_rds(.x, file.path("Data/processed_stats", paste0("dvp_", .y, ".rds"))))

# Combined long-form for quick plotting later
dvp_all <- dvp_list |>
    imap_dfr(function(df, stat_name) {
        avg_col <- paste0("avg_", stat_name)
        df |>
            mutate(stat = stat_name, value = .data[[avg_col]]) |>
            select(opposition, position, games, stat, value)
    })

write_rds(dvp_all, file = file.path("Data/processed_stats", "dvp_all.rds"))

# Named list bundle for convenience
dvp_results <- c(
    list(
        season    = season_target,
        per       = per_minutes,
        min_min   = min_minutes,
        min_games = min_games,
        offset    = offset_n
    ),
    dvp_list,
    list(all = dvp_all)
)

write_rds(dvp_results, file = file.path("Data/processed_stats", "dvp_results.rds"))

message("DVP tables saved to Data/processed_stats as RDS files.")
