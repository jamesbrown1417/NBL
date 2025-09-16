# Get Player Correlations

#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(readxl)

#===============================================================================
# Read in data
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

# Get in right format
all_player_stats <-
    combined_stats_table |> 
    mutate(PLAYER_NAME = paste(first_name, family_name)) |>
    mutate(minutes_played = period_to_seconds(ms(player_minutes)) / 60) |> 
    rename(
        PTS = player_points,
        REB = player_rebounds_total,
        AST = player_assists,
        STL = player_steals,
        BLK = player_blocks,
        MIN = minutes_played,
        SEASON_YEAR = season) |> 
    mutate(MIN = round(MIN, 2)) |> 
    mutate(PRA = PTS + REB + AST) |> 
    mutate(HOME_TEAM = ifelse(home_away == "home", name, opp_name)) |>
    mutate(AWAY_TEAM = ifelse(home_away == "away", name, opp_name))

# Get teams data

#===============================================================================
# Function to get player correlations
#===============================================================================

# Function to get correlation between players-----------------------------------
get_player_correlation <- function(seasons = NULL, name_a, name_b, metric_a, metric_b) {
    # Column names for later use
    col_name_a <- paste0(name_a, " ", metric_a)
    col_name_b <- paste0(name_b, " ", metric_b)
    
    # Get dataframe for player A
    df_player_a <- 
        all_player_stats %>%
        filter(PLAYER_NAME == name_a & SEASON_YEAR %in% seasons) |> 
        select(match_id, PLAYER_NAME, all_of(metric_a)) |> 
        rename(!!col_name_a := all_of(metric_a))
    
    # Get dataframe for player B
    df_player_b <- 
        all_player_stats %>%
        filter(PLAYER_NAME == name_b & SEASON_YEAR %in% seasons) |> 
        select(match_id, PLAYER_NAME, all_of(metric_b)) |> 
        rename(!!col_name_b := all_of(metric_b))
    
    # Merge the two dataframes
    df_merged <- inner_join(df_player_a, df_player_b, by = "match_id")
    
    # Compute correlation
    correlation <- cor.test(df_merged[[col_name_a]], df_merged[[col_name_b]], method = "pearson")
    
    cor_estimate = correlation$estimate
    cor_p_value = correlation$p.value
    
    # Return Tibble
    df_merged |> 
        summarise(games_together = n()) |>
        mutate(correlation = cor_estimate,
               p_value = cor_p_value) |> 
        mutate(player_1 = name_a, player_2 = name_b) |>
        mutate(metric_1 = metric_a, metric_2 = metric_b) |>
        select(player_1, metric_1, player_2, metric_2, games_together, correlation, p_value)
}

#===============================================================================
# Apply function to get player correlations
#===============================================================================

# Get all player combinations for a given team----------------------------------
get_all_player_combinations <- function(team_name, seasons = c("2025-2026")) {
    # Preparing team_players data frame
    team_players <- 
        all_player_stats %>% 
        filter(SEASON_YEAR %in% seasons, name == team_name) %>% 
        select(name = PLAYER_NAME) %>% 
        distinct()
    
    # Creating a data frame with metrics
    metrics <- data.frame(metric = c("PTS", "REB", "AST"))
    
    # Expanding team_players to include all combinations with metrics
    team_player_metrics <- 
        team_players %>% 
        expand_grid(., metrics) %>%
        select(PLAYER_NAME = name, metric)
    
    # Generate all pairwise combinations of players and metrics
    pairwise_combinations <- 
        expand_grid(team_player_metrics, team_player_metrics, .name_repair = "universal") %>%
        filter(PLAYER_NAME...1 != PLAYER_NAME...3) |> 
        rename(name_a = PLAYER_NAME...1, name_b = PLAYER_NAME...3, metric_a = metric...2, metric_b = metric...4) |> 
        distinct(name_a, name_b, metric_a, metric_b) |> 
        filter(name_a < name_b)
    
    # Return dataframe
    pairwise_combinations |> 
        mutate(seasons = seasons)
}

# Create safe version of get_player_correlation---------------------------------
safe_get_player_correlation <- safely(get_player_correlation, otherwise = c(NULL))

# PMAP to get all player correlations-------------------------------------------
player_correlations_sem <- 
    get_all_player_combinations("South East Melbourne Phoenix") |> 
    select(-seasons) |> 
    pmap(safe_get_player_correlation, .progress = TRUE, seasons = c("2024-2025", "2025-2026"))

# Filter out NULL values
player_correlations_sem <- 
    player_correlations_sem |> 
    map("result") |> 
    keep(~ !is.null(.x)) |> 
    bind_rows()
