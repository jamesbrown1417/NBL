library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyverse)

# Pointsbet SGM-----------------------------------------------------------------
# Safe read function
safe_read_pointsbet <- function(file) {
  tryCatch({
    df <- read_csv(file, show_col_types = FALSE)
    if (nrow(df) == 0) return(tibble())
    df
  }, error = function(e) tibble())
}

pointsbet_sgm_list <-
  list(
    safe_read_pointsbet("../../Data/scraped_odds/pointsbet_player_points.csv"),
    safe_read_pointsbet("../../Data/scraped_odds/pointsbet_player_rebounds.csv"),
    safe_read_pointsbet("../../Data/scraped_odds/pointsbet_player_assists.csv"),
    safe_read_pointsbet("../../Data/scraped_odds/pointsbet_player_threes.csv")
  )

pointsbet_sgm <-
  pointsbet_sgm_list |>
  keep(~nrow(.x) > 0) |>
  bind_rows()

# Ensure a `match` column exists; derive from home/away if needed
if (!("match" %in% names(pointsbet_sgm))) {
  if (all(c("home_team", "away_team") %in% names(pointsbet_sgm))) {
    pointsbet_sgm <- pointsbet_sgm |> mutate(match = paste(.data$home_team, " v ", .data$away_team))
  } else {
    pointsbet_sgm <- pointsbet_sgm |> mutate(match = NA_character_)
  }
}

# Build Over/Under rows with appropriate outcome IDs and price (fail gracefully)
if (!all(c("over_price","EventKey","MarketKey","OutcomeKey") %in% names(pointsbet_sgm))) {
  # Return an empty, schema-correct tibble if odds not loaded yet
  pointsbet_sgm <- tibble(
    match = character(),
    player_name = character(),
    line = numeric(),
    market_name = character(),
    agency = character(),
    type = character(),
    price = numeric(),
    EventKey = character(),
    MarketKey = character(),
    OutcomeKey_sgm = character()
  )
} else {
  pointsbet_over <- pointsbet_sgm |>
    transmute(match = .data$match,
              player_name = .data$player_name,
              line = .data$line,
              market_name = .data$market_name,
              agency = .data$agency,
              type = "Over",
              price = .data$over_price,
              EventKey = .data$EventKey,
              MarketKey = .data$MarketKey,
              OutcomeKey_sgm = .data$OutcomeKey)

  pointsbet_under <- tibble()
  if ("under_price" %in% names(pointsbet_sgm)) {
    pointsbet_under <- pointsbet_sgm |>
      filter(!is.na(under_price) | !is.na(OutcomeKey_unders)) |>
      transmute(match = .data$match,
                player_name = .data$player_name,
                line = .data$line,
                market_name = .data$market_name,
                agency = .data$agency,
                type = "Under",
                price = .data$under_price,
                EventKey = .data$EventKey,
                MarketKey = .data$MarketKey,
                OutcomeKey_sgm = .data$OutcomeKey_unders)
  }

  pointsbet_sgm <- bind_rows(pointsbet_over, pointsbet_under) |>
    distinct(match, player_name, line, market_name, type, agency, .keep_all = TRUE)
}

#===============================================================================
# Function to get SGM data
#=-=============================================================================

# Create function to call the API
get_sgm_pointsbet <- function(data, player_names, stat_counts, markets, types) {
  if (length(player_names) != length(stat_counts)) {
    stop("Both lists should have the same length")
  }

  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>%
      filter(player_name == player_names[[i]] &
             line == stat_counts[i] &
               market_name == markets[i] &
               type == types[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }

  id_list <- as.character(filtered_df$OutcomeKey_sgm)
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

call_sgm_pointsbet <- function(data, player_names, stat_counts, markets, types) {
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

  payload <- get_sgm_pointsbet(data, player_names, stat_counts, markets, types)

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
