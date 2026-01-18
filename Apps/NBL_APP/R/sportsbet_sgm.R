library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyverse)

# Sportsbet SGM-----------------------------------------------------------------
# Safe read function
safe_read_sportsbet <- function(file) {
  tryCatch({
    df <- read_csv(file, show_col_types = FALSE)
    if (nrow(df) == 0) return(tibble())
    df
  }, error = function(e) tibble())
}

sportsbet_sgm <-
  safe_read_sportsbet("../../Data/scraped_odds/sportsbet_player_points.csv") |>
  bind_rows(safe_read_sportsbet("../../Data/scraped_odds/sportsbet_player_rebounds.csv")) |>
  bind_rows(safe_read_sportsbet("../../Data/scraped_odds/sportsbet_player_assists.csv")) |>
  bind_rows(safe_read_sportsbet("../../Data/scraped_odds/sportsbet_player_threes.csv"))

if (nrow(sportsbet_sgm) > 0 && all(c("event_external_id", "competition_external_id", "class_external_id", "market_id", "player_id") %in% names(sportsbet_sgm))) {
  sportsbet_sgm <-
    rename(
      sportsbet_sgm,
      eventExternalId = event_external_id ,
      competitionExternalId = competition_external_id,
      classExternalId = class_external_id ,
      marketExternalId = market_id,
      outcomeExternalId = player_id,
      outcomeExternalId_unders = player_id_unders
    )

  # Build Over/Under rows with appropriate outcome IDs and price
  sportsbet_sgm_over <- sportsbet_sgm |>
    transmute(match = .data$match,
              player_name = .data$player_name,
              line = .data$line,
              market_name = .data$market_name,
              agency = .data$agency,
              type = "Over",
              price = .data$over_price,
              classExternalId = .data$classExternalId,
              competitionExternalId = .data$competitionExternalId,
              eventExternalId = .data$eventExternalId,
              marketExternalId = .data$marketExternalId,
              outcomeExternalId_sgm = .data$outcomeExternalId)

  sportsbet_sgm_under <- sportsbet_sgm |>
    filter(!is.na(under_price), !is.na(outcomeExternalId_unders)) |>
    transmute(match = .data$match,
              player_name = .data$player_name,
              line = .data$line,
              market_name = .data$market_name,
              agency = .data$agency,
              type = "Under",
              price = .data$under_price,
              classExternalId = .data$classExternalId,
              competitionExternalId = .data$competitionExternalId,
              eventExternalId = .data$eventExternalId,
              marketExternalId = .data$marketExternalId,
              outcomeExternalId_sgm = .data$outcomeExternalId_unders)

  sportsbet_sgm <- bind_rows(sportsbet_sgm_over, sportsbet_sgm_under) |>
    distinct(match, player_name, line, market_name, type, agency, .keep_all = TRUE)
} else {
  sportsbet_sgm <- tibble(
    match = character(),
    player_name = character(),
    line = numeric(),
    market_name = character(),
    agency = character(),
    type = character(),
    price = numeric(),
    classExternalId = numeric(),
    competitionExternalId = numeric(),
    eventExternalId = numeric(),
    marketExternalId = numeric(),
    outcomeExternalId_sgm = numeric()
  )
}

#==============================================================================
# Function to get SGM data
#=-=============================================================================

get_sgm_sportsbet <- function(data, player_names, stat_counts, markets, types) {
  if (length(player_names) != length(stat_counts)) {
    stop("Both lists should have the same length")
  }

  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>%
      filter(player_name == player_names[i],
             line == stat_counts[i],
             market_name == markets[i],
             type == types[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }

  outcomes_list <- lapply(1:nrow(filtered_df), function(i) {
    list(marketExternalId = as.numeric(filtered_df$marketExternalId[i]),
         outcomeExternalId = as.numeric(filtered_df$outcomeExternalId_sgm[i]))
  })

  payload <- list(
    classExternalId = as.numeric(filtered_df$classExternalId[1]),
    competitionExternalId = as.numeric(filtered_df$competitionExternalId[1]),
    eventExternalId = as.numeric(filtered_df$eventExternalId[1]),
    outcomesExternalIds = outcomes_list
  )

  return(payload)
}

#==============================================================================
# Make Post Request
#==============================================================================

call_sgm_sportsbet <- function(data, player_names, stat_counts, markets, types) {
  if (length(player_names) != length(stat_counts)) {
    stop("Both lists should have the same length")
  }

  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>%
      filter(player_name == player_names[i],
             line == stat_counts[i],
             market_name == markets[i],
             type == types[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }

  if (nrow(filtered_df) != length(player_names)) {
    return(NULL)
  }

  unadjusted_price <- prod(filtered_df$price)

  payload <- get_sgm_sportsbet(data, player_names, stat_counts, markets, types)

  url <- 'https://www.sportsbet.com.au/apigw/multi-pricer/combinations/price'

  headers <- c('User-Agent' = 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Mobile Safari/537.36',
               'Content-Type' = 'application/json;charset=UTF-8')

  response <- POST(url, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers), encode = "json")

  # Check if the request was successful
  if (http_error(response)) {
    stop("API request failed: ", content(response, "text"))
  }

  response_content <- content(response, "parsed")

  # Check if the response contains the expected data
  if (!"price" %in% names(response_content)) {
    stop("Unexpected API response: 'price' not found")
  }

  adjusted_price <- 1 + (response_content$price$numerator / response_content$price$denominator)
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
    Agency = 'Sportsbet'
  )

  return(output_data)
}
