library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(mongolite)
library(tidyverse)

# Pointsbet SGM-----------------------------------------------------------------
pointsbet_sgm_list <-
    list(
        read_csv("../../Data/scraped_odds/pointsbet_player_points.csv"),
        read_csv("../../Data/scraped_odds/pointsbet_player_rebounds.csv"),
        read_csv("../../Data/scraped_odds/pointsbet_player_assists.csv")
    )

pointsbet_sgm <-
    pointsbet_sgm_list |> 
    keep(~nrow(.x) > 0) |>
    bind_rows()

if (nrow(pointsbet_sgm) == 0) {
    pointsbet_sgm <-
        tibble(match = character(),
               player_name = character(),
               line = numeric(),
               over_price = numeric(),
               market_name = character(),
               agency = character())
}

pointsbet_sgm <-
    pointsbet_sgm |> 
    rename(price = over_price) |>  
    distinct(match, player_name, line, market_name, agency, .keep_all = TRUE) |>
    select(-contains("under"))

#===============================================================================
# Function to get SGM data
#=-=============================================================================

# Create function to call the API
get_sgm_pointsbet <- function(data, player_names, stat_counts, markets) {
    if (length(player_names) != length(stat_counts)) {
        stop("Both lists should have the same length")
    }
    
    filtered_df <- data.frame()
    for (i in seq_along(player_names)) {
        temp_df <- data %>% 
            filter(player_name == player_names[[i]] &
                       line == stat_counts[i] &
                       market_name == markets[i])
        filtered_df <- bind_rows(filtered_df, temp_df)
    }
    
    id_list <- as.character(filtered_df$OutcomeKey)
    market_id_list <- as.character(filtered_df$MarketKey)
    event_key <- as.character(filtered_df$EventKey[1])
    
    selected_outcomes <- lapply(1:length(id_list), function(i) 
        list(MarketKey = unbox(market_id_list[i]), OutcomeKey = unbox(id_list[i]))
    )
    
    payload <- list(
        EventKey = unbox(event_key),
        SelectedOutcomes = selected_outcomes
    )
    
    return(payload)
}

#==============================================================================
# Make Post Request
#==============================================================================

call_sgm_pointsbet <- function(data, player_names, stat_counts, markets) {
    if (length(player_names) != length(stat_counts)) {
        stop("Both lists should have the same length")
    }
    
    filtered_df <- data.frame()
    for (i in seq_along(player_names)) {
        temp_df <- data %>% 
            filter(player_name == player_names[i],
                   line == stat_counts[i],
                   market_name == markets[i])
        filtered_df <- bind_rows(filtered_df, temp_df)
    }
    
    if (nrow(filtered_df) != length(player_names)) {
        return(NULL)
    }
    
    unadjusted_price <- prod(filtered_df$price)
    
    payload <- get_sgm_pointsbet(data, player_names, stat_counts, markets)
    
    url <- 'https://api.au.pointsbet.com/api/v2/sgm/price'
    
    headers <- c('User-Agent' = 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Mobile Safari/537.36',
                 'Content-Type' = 'application/json;charset=UTF-8',
                 'Origin' = 'https://pointsbet.com.au',
                 'Referer' = 'https://pointsbet.com.au/')
    
    # Error handling for the POST request
    tryCatch({
        response <- POST(url, body = toJSON(payload), add_headers(.headers = headers), encode = "json")
    }, error = function(e) {
        message("Error in POST request: ", e)
        return(NULL)
    })
    
    # If there is no response, return NULL
    if (is.null(response)) {
        return(NULL)
    }
    
    response_content <- content(response, "parsed")
    adjusted_price <- as.numeric(response_content$price)
    adjustment_factor <- adjusted_price / unadjusted_price
    combined_list <- paste(player_names, stat_counts, sep = ": ")
    player_string <- paste(combined_list, collapse = ", ")
    market_string <- paste(markets, collapse = ", ")
    
    output_data <- data.frame(
        Selections = player_string,
        Markets = market_string,
        Unadjusted_Price = unadjusted_price,
        Adjusted_Price = adjusted_price,
        Adjustment_Factor = adjustment_factor,
        Agency = 'Pointsbet'
    )
    
    return(output_data)
}

# call_sgm_pointsbet(
#   data = pointsbet_sgm,
#   player_names = c("Darius Days", "Sam Froling"),
#   stat_counts = c(14.5, 14.5),
#   markets = c("Player Points", "Player Points")
# )