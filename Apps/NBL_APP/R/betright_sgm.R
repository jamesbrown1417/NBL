library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)

# BetRight SGM------------------------------------------------------------------
betright_sgm_list <- list(
  read_csv("../../Data/scraped_odds/betright_player_points.csv"),
  read_csv("../../Data/scraped_odds/betright_player_rebounds.csv"),
  read_csv("../../Data/scraped_odds/betright_player_assists.csv"),
  read_csv("../../Data/scraped_odds/betright_player_threes.csv"),
  read_csv("../../Data/scraped_odds/betright_player_pras.csv")
)

betright_sgm <-
  betright_sgm_list |>
  keep(~nrow(.x) > 0) |>
  bind_rows()

# Build Over/Under rows if available (BetRight CSVs may not include Unders)
betright_over <- betright_sgm |>
  transmute(match = .data$match,
            player_name = .data$player_name,
            line = .data$line,
            market_name = .data$market_name,
            agency = .data$agency,
            type = "Over",
            price = .data$over_price,
            event_id = .data$event_id,
            outcome_name = .data$outcome_name,
            outcome_id = .data$outcome_id,
            fixed_market_id = .data$fixed_market_id)

betright_sgm <- betright_over |>
  distinct(match, player_name, line, market_name, type, agency, .keep_all = TRUE)

#===============================================================================
# Function to get SGM data
#===============================================================================

# Function to get SGM data
get_sgm_betright <- function(data, player_names, stat_counts, markets, types) {

  if (length(player_names) != length(stat_counts)) {
    stop("Both lists should have the same length")
  }

  filtered_df <- data.frame()
  for (i in 1:length(player_names)) {
    temp_df <- data[data$player_name == player_names[i] &
                      data$line == stat_counts[i] &
                      data$market_name == markets[i] &
                      data$type == types[i], ]
    if (nrow(temp_df) == 0) {
      stop(paste("No data found for", player_names[i], "with", stat_counts[i], "disposals."))
    }
    filtered_df <- rbind(filtered_df, temp_df)
  }

  header <- filtered_df$market_name
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
call_sgm_betright <- function(data, player_names, stat_counts, markets, types) {
  if (length(player_names) != length(stat_counts)) {
    stop("Both lists should have the same length")
  }

  filtered_df <- data.frame()
  for (i in 1:length(player_names)) {
    temp_df <- data[data$player_name == player_names[i] &
                      data$line == stat_counts[i] &
                      data$market_name == markets[i] &
                      data$type == types[i], ]
    if (nrow(temp_df) == 0) {
      stop(paste("No data found for", player_names[i], "with", stat_counts[i], "disposals."))
    }
    filtered_df <- rbind(filtered_df, temp_df)
  }

  if (nrow(filtered_df) != length(player_names)) {
    return(NULL)
  }

  unadjusted_price <- prod(filtered_df$price)

  payload <- get_sgm_betright(data, player_names, stat_counts, markets, types)

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
