empty_player_odds <- function() {
  tibble::tibble(
    match = character(), home_team = character(), away_team = character(),
    market_name = character(), player_name = character(), player_team = character(),
    opposition_team = character(), line = numeric(), over_price = numeric(),
    under_price = numeric(), agency = character(), implied_prob_over = numeric(),
    implied_prob_under = numeric(), games_played_current = integer(),
    empirical_prob_over_current = numeric(), empirical_prob_under_current = numeric(),
    empirical_prob_over_last_10 = numeric(), empirical_prob_under_last_10 = numeric(),
    diff_over_current = numeric(), diff_under_current = numeric(),
    diff_over_last_10 = numeric(), diff_under_last_10 = numeric(),
    model_season = character(), variation = numeric()
  )
}

read_processed_player_odds <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    return(empty_player_odds())
  }

  purrr::map_dfr(paths, readr::read_rds)
}
