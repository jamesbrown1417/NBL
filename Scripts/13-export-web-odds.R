library(dplyr)
library(jsonlite)
library(readr)

source("Scripts/00-config.R")

active_agencies <- c("BetRight", "Pointsbet", "Sportsbet", "TAB")
odds_dir <- data_paths$processed_odds

read_market <- function(name) {
  path <- file.path(odds_dir, paste0(name, ".rds"))
  if (!file.exists(path)) stop("Missing processed odds: ", path)
  read_rds(path) |>
    filter(agency %in% active_agencies)
}

props <- bind_rows(lapply(
  c("points", "rebounds", "assists", "threes", "pras", "steals", "blocks"),
  function(market) read_market(paste0("all_player_", market))
)) |>
  filter(model_season == nbl_config$active_season) |>
  transmute(
    match, homeTeam = home_team, awayTeam = away_team,
    market = market_name, player = player_name, team = player_team,
    line = as.numeric(line), agency,
    overPrice = as.numeric(over_price), underPrice = as.numeric(under_price),
    gamesCurrent = as.integer(games_played_current),
    hitCurrent = as.numeric(empirical_prob_over_current),
    hitLast10 = as.numeric(empirical_prob_over_last_10)
  ) |>
  distinct()

# Model prices from Models/props/03-price-current-markets.R, joined per quoted
# line. modelProbOver is the market-conditioned probability the model prices
# with; the app derives fair prices and edges from it for each side. For
# one-sided X+ lines the app ranks by altLegBacktestRoi instead, because raw
# model edges on those lines did not hold up in the backtest.
model_file <- file.path(project_root, "Models", "props", "output", "current_prices.rds")
model_generated_at <- NULL
model_meta_file <- file.path(project_root, "Models", "props", "output", "current_prices_meta.rds")
model_meta <- if (file.exists(model_meta_file)) read_rds(model_meta_file) else list()
if (file.exists(model_file)) {
  model_generated_at <- format(file.mtime(model_file), "%Y-%m-%dT%H:%M:%S%z")
  model_prices <- read_rds(model_file) |>
    transmute(
      match, market = market_name, player = player_name, line = as.numeric(line), agency,
      modelMean = round(model_mean, 2),
      modelProbOver = round(final_p_over, 4),
      modelSource = price_source,
      betSignal = bet_signal,
      betSide = if_else(best_side == "over", "Over", "Under"),
      altLegTier = alt_leg_tier,
      altLegBacktestRoi = round(alt_leg_backtest_roi, 4)
    ) |>
    distinct(match, market, player, line, agency, .keep_all = TRUE)
  props <- props |> left_join(model_prices, by = c("match", "market", "player", "line", "agency"))
  if (file.mtime(model_file) < max(file.mtime(list.files(odds_dir, full.names = TRUE)))) {
    warning("Model prices are older than the processed odds; rerun Models/props/03-price-current-markets.R")
  }
} else {
  warning("No model prices found; exporting odds without model columns")
}

h2h <- read_market("head_to_head") |>
  transmute(match, homeTeam = home_team, awayTeam = away_team, agency,
            homePrice = as.numeric(home_win), awayPrice = as.numeric(away_win)) |>
  distinct()

totals <- read_market("total_match_points") |>
  transmute(match, homeTeam = home_team, awayTeam = away_team, agency,
            line = as.numeric(line), overPrice = as.numeric(over_price),
            underPrice = as.numeric(under_price)) |>
  distinct()

payload <- list(
  metadata = list(
    generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    season = nbl_config$active_season,
    agencies = active_agencies,
    modelGeneratedAt = model_generated_at,
    modelSeasonWeek = model_meta$season_week,
    modelSignalsFromWeek = model_meta$signals_from_week
  ),
  headToHead = h2h,
  totals = totals,
  props = props
)

output_dir <- file.path(project_root, "web", "public", "data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
staging_file <- tempfile("nbl-odds-", tmpdir = output_dir, fileext = ".json")
write_json(payload, staging_file, dataframe = "rows", auto_unbox = TRUE, na = "null", digits = NA)
if (!file.rename(staging_file, file.path(output_dir, "nbl-odds.json"))) {
  unlink(staging_file)
  stop("Could not publish the web odds export")
}
message(
  "Exported ", nrow(h2h), " H2H, ", nrow(totals), " totals and ", nrow(props), " player prices (",
  sum(!is.na(props$modelProbOver %||% NA)), " with model prices)"
)
