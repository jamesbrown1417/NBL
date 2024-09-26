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
    filter(season == "2024-2025")

calculate_offensive_pace <- function(match_id) {
    #===============================================================================
    # Get Play-By-Play Data for the Specified Match
    #===============================================================================
    
    # Fetch PBP data for the given match_id
    match_pbp <-
        all_pbp_data %>%
        filter(match_id == {{ match_id }})
    
    # Identify the two teams involved in the match
    teams <- match_pbp %>%
        distinct(team_name) %>%
        filter(!is.na(team_name)) %>%
        pull(team_name)
    
    # Initialize a list to store results for each team
    results <- list()
    
    #===============================================================================
    # Calculate Offensive Pace for Each Team
    #===============================================================================
    
    for (team in teams) {
        # Identify the opponent team
        opponent_team <- setdiff(teams, team)
        
        #-------------------------------------------------------------------------
        # Get All Changes of Possession Where the Team Gains Possession
        #-------------------------------------------------------------------------
        
        # Defensive rebounds by the team
        defensive_rebounds <- match_pbp %>%
            filter(action_type == "rebound", sub_type == "defensive", team_name == team)
        
        # Turnovers by the opponent team
        opponent_turnovers <- match_pbp %>%
            filter(action_type == "turnover", team_name == opponent_team)
        
        # Field goals made by the opponent team
        opponent_field_goals_made <- match_pbp %>%
            filter(scoring == 1, success == 1, team_name == opponent_team)
        
        # Jump balls won by the team
        jumpball_won <- match_pbp %>%
            filter(action_type == "jumpball", sub_type == "won", team_name == team)
        
        # Combine all changes of possession
        changes_of_possession <- bind_rows(
            defensive_rebounds,
            opponent_turnovers,
            opponent_field_goals_made,
            jumpball_won
        ) %>%
            arrange(action_number)
        
        #-------------------------------------------------------------------------
        # Get All Possession-Ending Actions by the Team
        #-------------------------------------------------------------------------
        
        # Shot attempts by the team
        shot_attempts <- match_pbp %>%
            filter(scoring == 1, team_name == team) %>%
            arrange(action_number)
        
        # Turnovers by the team
        turnovers <- match_pbp %>%
            filter(action_type == "turnover", team_name == team) %>%
            arrange(action_number)
        
        # Period ends
        period_ends <- match_pbp %>%
            filter(action_type == "period", sub_type == "end") %>%
            arrange(action_number)
        
        # Combine possession-ending actions
        possession_ends <- bind_rows(shot_attempts, turnovers, period_ends) %>%
            arrange(action_number)
        
        #-------------------------------------------------------------------------
        # Calculate Time Differences Between Possession Start and End
        #-------------------------------------------------------------------------
        
        # Get the time of the start of each possession
        start_of_possession <- changes_of_possession %>%
            select(period, action_number, time = gt)
        
        # Function to get the next possession-ending action after a given action
        get_next_action <- function(action_number_possession, time_possession) {
            next_action <- possession_ends %>%
                filter(action_number > action_number_possession) %>%
                slice_head(n = 1) %>%
                mutate(start_of_possession = time_possession) %>%
                relocate(start_of_possession, .before = gt)
            
            return(next_action)
        }
        
        # Create a table of all possession starts
        table_of_possession_start <- start_of_possession %>%
            select(
                action_number_possession = action_number,
                time_possession = time
            )
        
        # Apply the function to each possession start
        next_actions <- pmap(table_of_possession_start, get_next_action) %>%
            bind_rows()
        
        #-------------------------------------------------------------------------
        # Filter for Valid Time Differences and Calculate Offensive Pace
        #-------------------------------------------------------------------------
        
        # Convert times to minutes and seconds
        next_scoring_shots <- next_actions %>%
            filter(scoring == 1) %>%
            mutate(
                time_ms = ms(gt),
                time_possession = ms(start_of_possession),
                time_diff = as.duration(time_possession - time_ms)
            ) %>%
            filter(time_diff > 0 & time_diff < 30)
        
        # Calculate the offensive pace
        offensive_pace <- next_scoring_shots %>%
            summarise(offensive_pace = mean(time_diff)) %>%
            mutate(
                offensive_pace = round(offensive_pace, 2),
                team_name = team
            )
        
        # Store the result
        results[[team]] <- offensive_pace
    }
    
    #===============================================================================
    # Combine Results and Return
    #===============================================================================
    
    final_result <- bind_rows(results) %>%
        select(team_name, offensive_pace)
    
    final_result$defensive_pace <- rev(final_result$offensive_pace)
    
    final_result$match_id = match_id
    
    return(final_result)
}

#===============================================================================
# Map function to each match this season
#===============================================================================

# Get all match IDs from the 2024/2025 season
match_ids <- all_pbp_data %>%
    filter(!is.na(match_id)) %>%
    filter(season == "2024-2025") %>%
    pull(match_id) %>%
    unique()

# Calculate offensive pace for each match
all_pace_results <- future_map_dfr(match_ids, calculate_offensive_pace, .progress = TRUE)

# Get average for each team
average_team_pace <-
    all_pace_results %>%
    group_by(team_name) %>%
    summarise(
        average_offensive_pace = mean(offensive_pace, na.rm = TRUE),
        average_defensive_pace = mean(defensive_pace, na.rm = TRUE)
    ) %>%
    arrange(desc(average_offensive_pace))
