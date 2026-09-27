library(tidyverse)

source("Scripts/00-config.R")
source("Scripts/08-get-empirical-probabilities.R")
source("Scripts/odds-schema.R")
ensure_data_directories()

run_scraping <- function(script_name) {
  message("Running ", script_name)
  source(script_name, echo = FALSE)
}

# Only agencies checked for the 2026-27 season feed this run.
active_agencies <- c("betright", "pointsbet", "sportsbet", "tab")

if (Sys.getenv("NBL_SKIP_SCRAPING") != "true") {
  c(
    "OddsScraper/scrape_BetRight.R",
    "OddsScraper/scrape_pointsbet.R",
    "OddsScraper/scrape_sportsbet.R",
    "OddsScraper/TAB/scrape_TAB.R"
  ) |>
    walk(run_scraping)
}

if (Sys.getenv("NBL_SKIP_DVP") != "true") {
  tryCatch(
    source("Scripts/06-defence-vs-position.R"),
    error = function(e) message("DVP generation skipped: ", conditionMessage(e))
  )
}

read_odds_group <- function(pattern) {
  files <- list.files(data_paths$raw_odds,
    full.names = TRUE, pattern = pattern,
    recursive = FALSE
  )
  files <- files[tolower(sub("_.*$", "", basename(files))) %in% active_agencies]

  if (!length(files)) {
    return(tibble())
  }

  files |>
    map(~ suppressMessages(read_csv(.x, show_col_types = FALSE))) |>
    keep(~ nrow(.x) > 0L) |>
    bind_rows()
}

write_match_markets <- function() {
  h2h <- read_odds_group("h2h\\.csv$")
  if (nrow(h2h)) {
    h2h <- h2h |>
      select(match, market_name, agency, home_team, away_team, home_win, away_win)
  } else {
    h2h <- tibble(
      match = character(), market_name = character(), agency = character(),
      home_team = character(), away_team = character(),
      home_win = numeric(), away_win = numeric()
    )
  }
  write_rds(h2h, data_file("processed_odds", "head_to_head.rds"))

  totals <- read_odds_group("total.*\\.csv$")
  if (nrow(totals)) {
    totals <- totals |>
      mutate(
        line = as.numeric(line),
        under_price = as.numeric(under_price),
        over_price = as.numeric(over_price),
        market_name = "Total Points"
      ) |>
      arrange(match, line, desc(under_price)) |>
      select(match, market_name, home_team, away_team, line, over_price, under_price, agency)
  } else {
    totals <- tibble(
      match = character(), market_name = character(), home_team = character(),
      away_team = character(), line = numeric(), over_price = numeric(),
      under_price = numeric(), agency = character()
    )
  }
  write_rds(totals, data_file("processed_odds", "total_match_points.rds"))
}

process_player_market <- function(file_key, stat) {
  odds <- read_odds_group(paste0("player_", file_key, "\\.csv$"))
  output_path <- data_file("processed_odds", paste0("all_player_", file_key, ".rds"))

  if (!nrow(odds)) {
    write_rds(empty_player_odds(), output_path)
    return(invisible(NULL))
  }

  if (!"under_price" %in% names(odds)) {
    odds$under_price <- NA_real_
  }

  combos <- odds |>
    distinct(player_name, line)

  probabilities <- pmap_dfr(
    combos,
    function(player_name, line) {
      get_empirical_prob(player_name, line, stat, nbl_config$active_season)
    },
    .progress = TRUE
  ) |>
    select(player_name, line, games_played, empirical_prob, empirical_prob_last_10)

  processed <- odds |>
    mutate(
      over_price = as.numeric(over_price),
      under_price = as.numeric(under_price),
      implied_prob_over = 1 / over_price,
      implied_prob_under = 1 / under_price
    ) |>
    left_join(probabilities, by = c("player_name", "line")) |>
    rename(
      games_played_current = games_played,
      empirical_prob_over_current = empirical_prob,
      empirical_prob_over_last_10 = empirical_prob_last_10
    ) |>
    mutate(
      empirical_prob_under_current = 1 - empirical_prob_over_current,
      empirical_prob_under_last_10 = 1 - empirical_prob_over_last_10,
      diff_over_current = empirical_prob_over_current - implied_prob_over,
      diff_under_current = empirical_prob_under_current - implied_prob_under,
      diff_over_last_10 = empirical_prob_over_last_10 - implied_prob_over,
      diff_under_last_10 = empirical_prob_under_last_10 - implied_prob_under,
      model_season = nbl_config$active_season
    ) |>
    filter(!is.na(opposition_team)) |>
    group_by(player_name, line) |>
    mutate(
      variation = if (all(is.na(implied_prob_over))) {
        NA_real_
      } else {
        max(implied_prob_over, na.rm = TRUE) - min(implied_prob_over, na.rm = TRUE)
      }
    ) |>
    ungroup() |>
    mutate(across(where(is.double), ~ round(.x, 2))) |>
    arrange(desc(variation), player_name, desc(over_price), line) |>
    select(
      -matches("_id$"), -matches("_key$"), -matches("_id_"),
      -any_of("outcome_name")
    )

  write_rds(processed, output_path)
  invisible(NULL)
}

write_match_markets()

player_markets <- tribble(
  ~file_key, ~stat,
  "points", "PTS",
  "assists", "AST",
  "rebounds", "REB",
  "threes", "Threes",
  "pras", "PRA",
  "steals", "STL",
  "blocks", "BLK"
)

pwalk(player_markets, process_player_market)
