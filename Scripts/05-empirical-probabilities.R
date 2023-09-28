# Libraries
library(tidyverse)

# Read in dataset
nbl_data <- read_rds("Data/combined_stats_table.rds")

# Filter to last season
nbl_data_2022_2023 <-
    nbl_data |>
    filter(season == "2022-2023")

# Function to calculate empirical probabilities
get_emp_prob <- function(stat, line) {
    
    # Filter to at least 10 games played
    nbl_data_2022_2023 |>
        
        # Group by player
        group_by(first_name, family_name) |>
        
        # Calculate empirical probability
        summarise(games_played = n(),
            emp_prob = mean({{ stat }} >= {{ line }})) |>
        filter(games_played >= 10) |> 
        arrange(desc(emp_prob))
}


get_emp_prob(player_assists, 5)
