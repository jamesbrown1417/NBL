library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)

# TAB SGM-----------------------------------------------------------------------
# Safe read function
safe_read_tab <- function(file) {
  tryCatch({
    df <- read_csv(file, show_col_types = FALSE)
    if (nrow(df) == 0) return(tibble())
    df
  }, error = function(e) tibble())
}

tab_sgm_list <-
  list(
  safe_read_tab("../../Data/scraped_odds/tab_player_points.csv"),
  safe_read_tab("../../Data/scraped_odds/tab_player_rebounds.csv"),
  safe_read_tab("../../Data/scraped_odds/tab_player_assists.csv"),
  safe_read_tab("../../Data/scraped_odds/tab_player_threes.csv")
)

tab_sgm <-
  tab_sgm_list |>
  keep(~nrow(.x) > 0) |>
  bind_rows()

if (nrow(tab_sgm) > 0 && "match" %in% names(tab_sgm)) {
  # Build Over/Under rows with appropriate proposition IDs and price
  tab_over <- tab_sgm |>
    transmute(match = .data$match,
              player_name = .data$player_name,
              line = .data$line,
              market_name = .data$market_name,
              agency = .data$agency,
              type = "Over",
              price = .data$over_price,
              prop_id_sgm = .data$prop_id)

  tab_under <- tibble()
  if ("under_price" %in% names(tab_sgm)) {
    tab_under <- tab_sgm |>
      filter(!is.na(under_price) | !is.na(under_prop_id)) |>
      transmute(match = .data$match,
                player_name = .data$player_name,
                line = .data$line,
                market_name = .data$market_name,
                agency = .data$agency,
                type = "Under",
                price = .data$under_price,
                prop_id_sgm = .data$under_prop_id)
  }

  tab_sgm <- bind_rows(tab_over, tab_under) |>
    distinct(match, player_name, line, market_name, type, agency, .keep_all = TRUE)
} else {
  tab_sgm <- tibble(
    match = character(),
    player_name = character(),
    line = numeric(),
    market_name = character(),
    agency = character(),
    type = character(),
    price = numeric(),
    prop_id_sgm = character()
  )
}

#==============================================================================
# Function to get SGM data
#===============================================================================

# Function to get SGM data
get_sgm_tab <- function(data, player_names, stat_counts, markets, types) {
  if (length(player_names) != length(stat_counts)) {
    stop("Both lists should have the same length")
  }

  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>%
      filter(player_name == player_names[i] &
               line == stat_counts[i] &
               market_name == markets[i] &
               type == types[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }

  # Get the proposition ID column as a list
  id_list <- filtered_df$prop_id_sgm

  # Create the propositions list using the id_list
  propositions <- lapply(id_list, function(id) list(type = unbox("WIN"), propositionId = unbox(id)))

  return(propositions)
}

#==============================================================================
# Make Post Request
#==============================================================================

# Make Post Request
call_sgm_tab <- function(data, player_names, stat_counts, markets, types) {
  tryCatch({
    if (length(player_names) != length(stat_counts)) {
      stop("Both lists should have the same length")
    }

    filtered_df <- data.frame()
    for (i in seq_along(player_names)) {
      temp_df <- data %>%
        filter(player_name == player_names[i] &
                 line == stat_counts[i] &
                 market_name == markets[i] &
                 type == types[i])
      filtered_df <- bind_rows(filtered_df, temp_df)
    }

    if (nrow(filtered_df) != length(player_names)) {
      return(NULL)
    }

    # Unadjusted price
    unadjusted_price <- prod(filtered_df$price)

    # Get propositions
    propositions <- get_sgm_tab(data, player_names, stat_counts, markets, types)

    url <- "https://api.beta.tab.com.au/v1/pricing-service/enquiry"

    headers <- c(
      "Accept" = "application/json, text/plain, */*",
      "Accept-Encoding" = "gzip, deflate, br, zstd",
      "Accept-Language" = "en-US,en;q=0.9",
      "Content-Type" = "application/json;charset=UTF-8",
      "Origin" = "https://www.tab.com.au",
      "Referer" = "https://www.tab.com.au/",
      "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/136.0.0.0 Safari/537.36",
      "sec-ch-ua" = '"Chromium";v="136", "Google Chrome";v="136", "Not.A/Brand";v="99"',
      "sec-ch-ua-mobile" = "?0",
      "sec-ch-ua-platform" = '"macOS"',
      "Cookie" = "YOUR_COOKIE_STRING_HERE"  # You'll need to add the full cookie string
    )

    payload <- list(
      clientDetails = list(jurisdiction = unbox("SA"), channel = unbox("web")),
      bets = list(
        list(
          type = unbox("FIXED_ODDS"),
          legs = list(
            list(
              type = unbox("SAME_GAME_MULTI"),
              propositions = propositions
            )
          )
        )
      ),
      returnValidationMatrix = unbox(TRUE)  # Added this line
    )

    # Try response, if nothing in 3 seconds, make it null
    response <- tryCatch({
      POST(url,
           body = toJSON(payload),
           add_headers(.headers = headers),
           encode = "json",
           timeout(5),
           config = config(http_version = 1.1))  # Force HTTP/1.1
    }, error = function(e) {
      return(NULL)
    })

    if(is.null(response)) {
      return(data.frame(
        Selections = NA_character_,
        Markets = NA_character_,
        Unadjusted_Price = NA_real_,
        Adjusted_Price = NA_real_,
        Adjustment_Factor = NA_real_,
        Agency = NA_character_
      ))
    }

    response_content <- content(response, "parsed")
    adjusted_price <- as.numeric(response_content$bets[[1]]$legs[[1]]$odds$decimal)
    adjustment_factor <- adjusted_price / unadjusted_price
    combined_list <- paste(player_names, stat_counts, sep = ": ")
    market_string <- paste(markets, collapse = ", ")
    player_string <- paste(combined_list, collapse = ", ")

    output_data <- tryCatch({
      data.frame(
        Selections = player_string,
        Markets = market_string,
        Unadjusted_Price = unadjusted_price,
        Adjusted_Price = adjusted_price,
        Adjustment_Factor = adjustment_factor,
        Agency = 'TAB'
      )
    }, error = function(e) {
      data.frame(
        Selections = NA_character_,
        Markets = NA_character_,
        Unadjusted_Price = NA_real_,
        Adjusted_Price = NA_real_,
        Adjustment_Factor = NA_real_,
        Agency = NA_character_
      )
    })

    return(output_data)

  }, error = function(e) {
    print(paste("Error: ", e))
  })
}
