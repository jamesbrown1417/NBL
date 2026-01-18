library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)

# Dabble SGM-----------------------------------------------------------------

# Helper function to read CSV and return empty tibble if 0 rows
safe_read <- function(file) {
  tryCatch({
    if (!file.exists(file)) return(tibble())
    df <- read_csv(file, show_col_types = FALSE)
    if (nrow(df) == 0) return(tibble()) else return(df)
  }, error = function(e) tibble())
}

dabble_sgm <-
  safe_read("../../Data/scraped_odds/dabble_pickem_player_points.csv") |>
  bind_rows(safe_read("../../Data/scraped_odds/dabble_pickem_player_rebounds.csv")) |>
  bind_rows(safe_read("../../Data/scraped_odds/dabble_pickem_player_assists.csv")) |>
  bind_rows(safe_read("../../Data/scraped_odds/dabble_pickem_player_threes.csv")) |>
  bind_rows(safe_read("../../Data/scraped_odds/dabble_pickem_player_pras.csv"))

if (nrow(dabble_sgm) > 0 && "match" %in% names(dabble_sgm)) {
  # Build Over/Under rows (no API adjustment used)
  dabble_over <- dabble_sgm |>
    transmute(match, player_name, line, market_name, agency, type = "Over",
              price = over_price)

  dabble_under <- tibble()
  if ("under_price" %in% names(dabble_sgm)) {
    dabble_under <- dabble_sgm |>
      filter(!is.na(under_price)) |>
      transmute(match, player_name, line, market_name, agency, type = "Under",
                price = under_price)
  }

  dabble_sgm <- bind_rows(dabble_over, dabble_under) |>
    distinct(match, player_name, line, market_name, type, agency, .keep_all = TRUE)
} else {
  dabble_sgm <- tibble(
    match = character(),
    player_name = character(),
    line = numeric(),
    market_name = character(),
    agency = character(),
    type = character(),
    price = numeric()
  )
}

#===============================================================================
# Function to get SGM Price
#===============================================================================

call_sgm_dabble <- function(data, player_names, stat_counts, markets, types) {
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

  # Dabble: adjusted price = product of leg prices (rounded to 2 dp)
  adjusted_price <- round(prod(filtered_df$price), 2)

  # Unadjusted price is the same for Dabble (no API adjustment)
  unadjusted_price <- adjusted_price
  adjustment_factor <- 1

  combined_list <- paste(player_names, stat_counts, sep = ": ")
  player_string <- paste(combined_list, collapse = ", ")
  market_string <- paste(markets, collapse = ", ")

  output_data <- data.frame(
    Selections = player_string,
    Markets = market_string,
    Unadjusted_Price = unadjusted_price,
    Adjusted_Price = adjusted_price,
    Adjustment_Factor = adjustment_factor,
    Agency = first(filtered_df$agency)
  )

  return(output_data)

}
