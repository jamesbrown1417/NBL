# Walk-forward backtest of the player prop model ------------------------------
#
# For each week containing historical odds, refit every model on games played
# before that week, then:
#   - price every archived line offered during the week
#   - score the full simulated distribution of every player in those matches
# Outputs (Models/props/output/backtest/):
#   backtest_lines<tag>.rds        priced lines
#   player_game_scores<tag>.rds    log score / abs error per player-game-stat
#
# Environment overrides for experiments (see 04-compare-backtests.R):
#   PROP_FEATURES   comma-separated feature list, or "none"
#   PROP_BACKTEST_TAG  suffix for output files, e.g. "_baseline"

suppressPackageStartupMessages({
  library(parallel)
})
source("Scripts/00-config.R")
source("Models/props/R/prop_model_functions.R")

output_dir <- file.path(project_root, "Models", "props", "output", "backtest")
settings <- modifyList(default_model_settings, list(
  half_life_days = as.numeric(Sys.getenv("PROP_HALF_LIFE", default_model_settings$half_life_days)),
  training_seasons = as.integer(Sys.getenv("PROP_TRAINING_SEASONS", default_model_settings$training_seasons))
))
feature_env <- Sys.getenv("PROP_FEATURES")
if (nzchar(feature_env)) {
  settings$features <- if (feature_env == "none") character() else str_split_1(feature_env, ",")
}
tag <- Sys.getenv("PROP_BACKTEST_TAG")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
message("Features: ", if (length(settings$features)) paste(settings$features, collapse = ", ") else "none")

features <- readRDS(data_file("processed_stats", "combined_stats_table.rds")) |>
  prepare_player_games() |>
  add_context_features() |>
  add_minutes_features() |>
  mutate(PRA = PTS + REB + AST)

# Every archived line (two-way and one-sided), latest snapshot on match day.
odds <- load_backtest_odds(features, output_dir)

# Consensus two-way points line per player-game (for line_minutes).
points_lines <- odds |>
  filter(stat == "PTS", !is.na(under_price)) |>
  group_by(match_id, player_name, match_date, minutes) |>
  summarise(pts_line = median(line), .groups = "drop")
odds <- odds |> select(-minutes)

message(sprintf("%d lines to price across %d weeks", nrow(odds), n_distinct(odds$refit_date)))

run_week <- function(week_start) {
  week_odds <- odds |> filter(refit_date == week_start)
  week_games <- features |>
    filter(match_id %in% week_odds$match_id) |>
    left_join(points_lines |> select(match_id, player_name, pts_line), by = c("match_id", "player_name"))

  models <- fit_prop_models(
    features, week_start,
    settings = settings,
    line_data = points_lines |> filter(match_date < week_start)
  )
  sim <- simulate_player_games(models, week_games)
  list(
    lines = price_lines(sim, week_odds),
    scores = score_player_games(sim, week_games) |> mutate(refit_date = week_start)
  )
}

RNGkind("L'Ecuyer-CMRG")
set.seed(20260927)
weeks <- sort(unique(odds$refit_date))
started <- Sys.time()
results <- mclapply(weeks, function(w) {
  tryCatch(run_week(w), error = function(e) {
    message("Week ", w, " failed: ", conditionMessage(e))
    NULL
  })
}, mc.cores = max(1L, detectCores() - 2L))
message("Backtest finished in ", format(round(Sys.time() - started, 1)))

failed <- sum(map_lgl(results, is.null))
if (failed > 0) stop(failed, " backtest weeks failed")

backtest_lines <- map_dfr(results, "lines")
player_game_scores <- map_dfr(results, "scores") |>
  left_join(
    features |> distinct(match_id, player_name, season, match_date, history),
    by = c("match_id", "player_name")
  )
saveRDS(backtest_lines, file.path(output_dir, paste0("backtest_lines", tag, ".rds")))
saveRDS(player_game_scores, file.path(output_dir, paste0("player_game_scores", tag, ".rds")))
message(sprintf(
  "Saved %d priced lines and %d player-game scores",
  nrow(backtest_lines), n_distinct(player_game_scores$match_id, player_game_scores$player_name)
))
