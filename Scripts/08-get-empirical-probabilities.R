#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(readxl)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in past season stats
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

# Create player name var
combined_stats_table <-
    combined_stats_table |>
    mutate(PLAYER_NAME = paste(first_name, family_name)) |>
    # Rename stats
    rename(
        PTS = player_points,
        REB = player_rebounds_total,
        AST = player_assists,
        STL = player_steals,
        BLK = player_blocks,
        Threes = player_three_pointers_made
    )

# Create season data - 2022-2023
combined_stats_2022_2023 <-
    combined_stats_table |>
    filter(season == "2022-2023") |>
    select(PLAYER_NAME, PTS, REB, AST, STL, BLK, Threes) |>
    mutate(season = "2022_2023")

# Create season data - 2023-2024
combined_stats_2023_2024 <-
    combined_stats_table |>
    filter(season == "2023-2024") |>
    select(PLAYER_NAME, PTS, REB, AST, STL, BLK, Threes) |>
    mutate(season = "2023_2024")

#===============================================================================
# Create a function that takes a player name + line and returns their hit rate
#===============================================================================

get_empirical_prob <- function(player_name, line, stat, season) {
    # Choose the data based on the selected season
    if (season == "2022_2023") {
        player_stats <-
            combined_stats_2022_2023 |> filter(PLAYER_NAME == player_name)
    } else if (season == "2023_2024") {
        player_stats <-
            combined_stats_2023_2024 |> filter(PLAYER_NAME == player_name)
    } else {
        stop("Invalid season selected")
    }
    
    # Initialize empirical_prob
    empirical_prob <- NULL
    
    # Branch based on whether stat is PTS, REB or AST
    if (stat == "PTS") {
        empirical_prob <- player_stats |>
            summarise(games_played = n(),
                      empirical_prob = mean(PTS >= line))
    } else if (stat == "REB") {
        empirical_prob <- player_stats |>
            summarise(games_played = n(),
                      empirical_prob = mean(REB >= line))
    } else if (stat == "AST") {
        empirical_prob <- player_stats |>
            summarise(games_played = n(),
                      empirical_prob = mean(AST >= line))
    } else if (stat == "Threes") {
        empirical_prob <- player_stats |>
            summarise(games_played = n(),
                      empirical_prob = mean(Threes >= line))
    }    else {
        stop("stat must be one of PTS, REB, AST or Threes")
    }
    
    # Add line, player_name, and season information
    empirical_prob <- empirical_prob |>
        mutate(line = line,
               player_name = player_name,
               season = season)
    
    # Rename the empirical_prob column to include season
    new_col_name <- paste("empirical_prob", season, sep = "_")
    empirical_prob <- empirical_prob |>
        rename_with( ~ new_col_name, .cols = "empirical_prob")
    
    # Return empirical probability
    return(empirical_prob)
}
