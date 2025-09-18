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

# If date scraped is before today's date, run the script
if (date_updated < ymd(Sys.Date())) {
    source("Scripts/01-get-data.R")
}

# Read in the full RDS dataset
combined_stats_table <- read_rds("Data/combined_stats_table.rds")

# Create player name variable
combined_stats_table <-
    combined_stats_table |>
    mutate(PLAYER_NAME = paste(first_name, family_name)) |>
    mutate(PLAYER_NAME = ifelse(PLAYER_NAME == "Matthew Mooney", "Matt Mooney", PLAYER_NAME)) |> 
    # Rename stats
    rename(
        PTS = player_points,
        REB = player_rebounds_total,
        AST = player_assists,
        STL = player_steals,
        BLK = player_blocks,
        Threes = player_three_pointers_made
    )

# Create season data - 2023-2024
combined_stats_2023_2024 <-
    combined_stats_table |>
    filter(season == "2023-2024") |>
    select(PLAYER_NAME, match_time_utc, PTS, REB, AST, STL, BLK, Threes, player_minutes) |>
    mutate(PRA = PTS + REB + AST) |>
    mutate(season = "2023_2024")

# Create season data - 2024-2025
combined_stats_2024_2025 <-
    combined_stats_table |>
    filter(season == "2024-2025") |>
    select(PLAYER_NAME, match_time_utc, PTS, REB, AST, STL, BLK, Threes, player_minutes) |>
    mutate(PRA = PTS + REB + AST) |>
    mutate(season = "2024_2025")

# Create season data - 2025-2026
combined_stats_2025_2026 <-
    combined_stats_table |>
    filter(season == "2025-2026") |>
    select(PLAYER_NAME, match_time_utc, PTS, REB, AST, STL, BLK, Threes, player_minutes) |>
    mutate(PRA = PTS + REB + AST) |>
    mutate(season = "2025_2026")

#===============================================================================
# Create a function that takes a player name + line and returns their hit rate
#===============================================================================

get_empirical_prob <- function(player_name, line, stat, season) {
    
    # Choose the data based on the selected season
    if (season == "2023_2024") {
        player_stats <- combined_stats_2023_2024 |> filter(PLAYER_NAME == player_name) |> filter(!is.na(player_minutes))
        # For last-10 context, use 2023_2024 + 2024_2025
        both_seasons_data <-
            combined_stats_2023_2024 |>
            bind_rows(combined_stats_2024_2025) |> 
            filter(PLAYER_NAME == player_name) |>
            filter(!is.na(player_minutes))
    } else if (season == "2024_2025") {
        player_stats <- combined_stats_2024_2025 |> filter(PLAYER_NAME == player_name) |> filter(!is.na(player_minutes))
        # For last-10 context, use 2023_2024 + 2024_2025
        both_seasons_data <-
            combined_stats_2023_2024 |>
            bind_rows(combined_stats_2024_2025) |> 
            filter(PLAYER_NAME == player_name) |>
            filter(!is.na(player_minutes))
    } else if (season == "2025_2026") {
        player_stats <- combined_stats_2025_2026 |> filter(PLAYER_NAME == player_name) |> filter(!is.na(player_minutes))
        # For last-10 context, use 2024_2025 + 2025_2026
        both_seasons_data <-
            combined_stats_2024_2025 |>
            bind_rows(combined_stats_2025_2026) |> 
            filter(PLAYER_NAME == player_name) |>
            filter(!is.na(player_minutes))
    } else {
        stop("Invalid season selected")
    }
    
    # Last 10 games
    player_stats_last_10 <-
        both_seasons_data |>
        group_by(PLAYER_NAME) |> 
        arrange(desc(match_time_utc)) |> 
        slice(1:10) |>
        ungroup()
    
    # Initialize empirical_prob
    empirical_prob <- NULL
    
    # Branch based on whether stat is PTS, REB or AST
    if (stat == "PTS") {
        empirical_prob <-
            player_stats |> 
            group_by(PLAYER_NAME) |> 
            summarise(games_played = n(),
                      empirical_prob = mean(PTS >= line)) |> 
            ungroup()
        
        if (season == "2023_2024" || season == "2024_2025" || season == "2025_2026") {
            
            last_10 <- player_stats_last_10 |> 
                group_by(PLAYER_NAME) |>
                summarise(empirical_prob_last_10 = mean(PTS >= line)) |> 
                ungroup()
            
            empirical_prob <-
                empirical_prob |>
                left_join(last_10, by = "PLAYER_NAME") |> 
                select(-PLAYER_NAME)
        }
        
    } else if (stat == "REB") {
        empirical_prob <-
            player_stats |> 
            group_by(PLAYER_NAME) |> 
            summarise(games_played = n(),
                      empirical_prob = mean(REB >= line)) |> 
            ungroup()
        
        if (season == "2023_2024" || season == "2024_2025" || season == "2025_2026") {
            
            last_10 <- player_stats_last_10 |> 
                group_by(PLAYER_NAME) |>
                summarise(empirical_prob_last_10 = mean(REB >= line)) |> 
                ungroup()
            
            empirical_prob <-
                empirical_prob |>
                left_join(last_10, by = "PLAYER_NAME") |> 
                select(-PLAYER_NAME)
        }
    } else if (stat == "AST") {
        empirical_prob <-
            player_stats |> 
            group_by(PLAYER_NAME) |> 
            summarise(games_played = n(),
                      empirical_prob = mean(AST >= line)) |> 
            ungroup()
        
        if (season == "2023_2024" || season == "2024_2025" || season == "2025_2026") {
            
            last_10 <- player_stats_last_10 |> 
                group_by(PLAYER_NAME) |>
                summarise(empirical_prob_last_10 = mean(AST >= line)) |> 
                ungroup()
            
            empirical_prob <-
                empirical_prob |>
                left_join(last_10, by = "PLAYER_NAME") |> 
                select(-PLAYER_NAME)
        }
        
    } else if (stat == "STL") {
        empirical_prob <-
            player_stats |> 
            group_by(PLAYER_NAME) |> 
            summarise(games_played = n(),
                      empirical_prob = mean(STL >= line)) |> 
            ungroup()
        
        if (season == "2023_2024" || season == "2024_2025" || season == "2025_2026") {
            
            last_10 <- player_stats_last_10 |> 
                group_by(PLAYER_NAME) |>
                summarise(empirical_prob_last_10 = mean(STL >= line)) |> 
                ungroup()
            
            empirical_prob <-
                empirical_prob |>
                left_join(last_10, by = "PLAYER_NAME") |> 
                select(-PLAYER_NAME)
        }
        
    } else if (stat == "BLK") {
        empirical_prob <-
            player_stats |> 
            group_by(PLAYER_NAME) |> 
            summarise(games_played = n(),
                      empirical_prob = mean(BLK >= line)) |> 
            ungroup()
        
        if (season == "2023_2024" || season == "2024_2025" || season == "2025_2026") {
            
            last_10 <- player_stats_last_10 |> 
                group_by(PLAYER_NAME) |>
                summarise(empirical_prob_last_10 = mean(BLK >= line)) |> 
                ungroup()
            
            empirical_prob <-
                empirical_prob |>
                left_join(last_10, by = "PLAYER_NAME") |> 
                select(-PLAYER_NAME)
        }
        
    } else if (stat == "Threes") {
        empirical_prob <-
            player_stats |> 
            group_by(PLAYER_NAME) |> 
            summarise(games_played = n(),
                      empirical_prob = mean(Threes >= line)) |> 
            ungroup()
        
        if (season == "2023_2024" || season == "2024_2025" || season == "2025_2026") {
            
            last_10 <- player_stats_last_10 |> 
                group_by(PLAYER_NAME) |>
                summarise(empirical_prob_last_10 = mean(Threes >= line)) |> 
                ungroup()
            
            empirical_prob <-
                empirical_prob |>
                left_join(last_10, by = "PLAYER_NAME") |> 
                select(-PLAYER_NAME)
        }
        
    } else if (stat == "PRA") {
        empirical_prob <-
            player_stats |> 
            group_by(PLAYER_NAME) |> 
            summarise(games_played = n(),
                      empirical_prob = mean(PRA >= line)) |> 
            ungroup()
        
        if (season == "2023_2024" || season == "2024_2025") {
            
            last_10 <- player_stats_last_10 |> 
                group_by(PLAYER_NAME) |>
                summarise(empirical_prob_last_10 = mean(PRA >= line)) |> 
                ungroup()
            
            empirical_prob <-
                empirical_prob |>
                left_join(last_10, by = "PLAYER_NAME") |> 
                select(-PLAYER_NAME)
        }
        
    } else {
        stop("stat must be one of PTS, REB, AST, STL, BLK, Threes, or PRA")
    }
    
    # Add line, player_name, and season information
    empirical_prob <- empirical_prob |> 
        mutate(line = line, 
               player_name = player_name, 
               season = season)
    
    # Rename the empirical_prob column to include season
    new_col_name <- paste("empirical_prob", season, sep = "_")
    empirical_prob <- empirical_prob |> 
        rename_with(~ new_col_name, .cols = "empirical_prob")
    
    # Return empirical probability
    return(empirical_prob)
}
