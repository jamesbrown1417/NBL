#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(httr2)
`%notin%` <- Negate(`%in%`)

safe_subscript <- function(x, index) {
    if (index <= length(x) && index > 0) {
        return(x[[index]])
    } else {
        return(NULL)
    }
}

# Supercoach API URL
url = "https://supercoach.dailytelegraph.com.au/2023/api/nbl/classic/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&round=0&xredir=1"

# Make request
req <- request(url)

# Get response
resp <- req_perform(req)

# Process response
all_data <- resp |> resp_body_json()

# Create a function to extract the data from the json list for each player
get_supercoach_data <- function(player_data) {
    tibble(
        player_id = player_data$id,
        player_name = paste(player_data$first_name, player_data$last_name),
        player_team = player_data$team$name,
        player_team_id = player_data$team_id,
        previous_games = player_data$previous_games,s
        previous_average = player_data$previous_average,
        previous_total = player_data$previous_total,
        injury_suspension_status = player_data$injury_suspension_status,
        injury_suspension_status_text = player_data$injury_suspension_status_text,
        locked = player_data$locked,
        active = player_data$active,
        played_status = player_data$played_status$display,
        supercoach_price = player_data$player_stats[[1]]$price,
        supercoach_position_1 = safe_subscript(player_data$positions, 1)$position,
        supercoach_position_2 = safe_subscript(player_data$positions, 2)$position
    )
}

# Map to list
extracted_data <-
    map(all_data, get_supercoach_data) |> 
    bind_rows()

extracted_data |> 
    select(player_name, player_team, supercoach_position_1, supercoach_position_2) |> 
    write_csv("NBL_Players.csv")
