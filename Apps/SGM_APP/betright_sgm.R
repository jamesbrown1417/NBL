library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)

# BetRight SGM------------------------------------------------------------------
betright_sgm_list <- list(
    read_csv("../../Data/scraped_odds/betright_player_points.csv"),
    read_csv("../../Data/scraped_odds/betright_player_rebounds.csv"),
    read_csv("../../Data/scraped_odds/betright_player_assists.csv")
)

betright_sgm <-
    betright_sgm_list |> 
    keep(~nrow(.x) > 0) |>
    bind_rows() |> 
    rename(price = over_price) |> 
    distinct(match, player_name, line, market_name, agency, .keep_all = TRUE) |> 
    select(-contains("under"))

#===============================================================================
# Function to get SGM data
#===============================================================================

# Function to get SGM data
get_sgm_betright <- function(data, player_names, stat_counts, markets) {
    
    if (length(player_names) != length(stat_counts)) {
        stop("Both lists should have the same length")
    }
    
    filtered_df <- data.frame()
    for (i in 1:length(player_names)) {
        temp_df <- data[data$player_name == player_names[i] & 
                            data$line == stat_counts[i] &
                            data$market_name == markets[i], ]
        if (nrow(temp_df) == 0) {
            stop(paste("No data found for", player_names[i], "with", stat_counts[i], "disposals."))
        }
        filtered_df <- rbind(filtered_df, temp_df)
    }
    
    header <- filtered_df$group_by_header
    event_id <- filtered_df$event_id
    outcome_name <- filtered_df$outcome_name
    outcome_id <- filtered_df$outcome_id
    fixed_market_id <- filtered_df$fixed_market_id
    points <- "0"
    fixed_win <- filtered_df$price
    
    payload <- lapply(1:length(player_names), function(i) {
        list(
            eventId = unlist(event_id[i]),
            outcomeId = unlist(outcome_id[i]),
            marketType = "WIN",
            fixedWin = unlist(fixed_win[i]),
            fixedMarketId = unlist(fixed_market_id[i]),
            marketTypeDesc = "Win",
            groupByHeader = header[i],
            points = points,
            outcomeName = outcome_name[i]
        )
    })
    
    return(payload)
}


#==============================================================================
# Make Post Request
#==============================================================================

# Make POST request
call_sgm_betright <- function(data, player_names, stat_counts, markets) {
    if (length(player_names) != length(stat_counts)) {
        stop("Both lists should have the same length")
    }
    
    filtered_df <- data.frame()
    for (i in 1:length(player_names)) {
        temp_df <- data[data$player_name == player_names[i] & 
                            data$line == stat_counts[i] &
                            data$market_name == markets[i], ]
        if (nrow(temp_df) == 0) {
            stop(paste("No data found for", player_names[i], "with", stat_counts[i], "disposals."))
        }
        filtered_df <- rbind(filtered_df, temp_df)
    }
    
    if (nrow(filtered_df) != length(player_names)) {
        return(NULL)
    }
    
    unadjusted_price <- prod(filtered_df$price)
    
    payload <- get_sgm_betright(data, player_names, stat_counts, markets)
    
    url <- "https://sgm-api.betright.com.au/Pricing/SgmPrice?"
    
    headers <- add_headers('User-Agent' = 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Mobile Safari/537.36',
                           'Content-Type' = 'application/json;charset=UTF-8',
                           'Origin' = 'https://betright.com.au',
                           'Referer' = 'https://betright.com.au/')
    
    response <- POST(url, headers, body = toJSON(payload, auto_unbox = TRUE))
    
    if (http_error(response)) {
        stop("HTTP error occurred while calling API.")
    }
    
    response_content <- fromJSON(content(response, "text"))
    
    if (!"price" %in% names(response_content)) {
        stop("No price information found in the API response.")
    }
    
    adjusted_price <- as.numeric(response_content$price)
    adjustment_factor <- adjusted_price / unadjusted_price
    player_string <- paste(paste(player_names, stat_counts, sep = ": "), collapse = ", ")
    market_string <- paste(markets, collapse = ", ")
    
    output_data <- data.frame(
        Selections = player_string,
        Markets = market_string,
        Unadjusted_Price = unadjusted_price,
        Adjusted_Price = adjusted_price,
        Adjustment_Factor = adjustment_factor,
        Agency = 'Betright'
    )
    
    return(output_data)
}

# call_sgm_betright(
#   data = betright_sgm,
#   player_names = c("Darius Days", "Sam Froling"),
#   stat_counts = c(14.5, 14.5),
#   markets = c("Player Points", "Player Points")
# )