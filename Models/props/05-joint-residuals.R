# Walk-forward residuals for the joint (SGM) model -------------------------------
#
# Refits the prop model at the start of each month from 2021-22 onward and
# records the randomised PIT of every player-game-stat under its predictive
# distribution, plus discreteness attenuation factors per stat. These are the
# inputs to 06-estimate-correlations.R. Odds are not needed, so this covers far
# more games than the odds-based backtest.
# Output: Models/props/output/joint/pit.rds (about 5 minutes)

suppressPackageStartupMessages({
  library(parallel)
})
source("Scripts/00-config.R")
source("Models/props/R/prop_model_functions.R")
source("Models/props/R/joint_functions.R")

first_season <- "2021-2022"
n_draws <- 2000L
output_dir <- file.path(project_root, "Models", "props", "output", "joint")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

features <- readRDS(data_file("processed_stats", "combined_stats_table.rds")) |>
  prepare_player_games() |>
  add_context_features() |>
  add_minutes_features() |>
  add_usual_role()

target <- features |>
  filter(season >= first_season, match_date < Sys.Date()) |>
  mutate(month = floor_date(match_date, "month"))
months <- sort(unique(target$month))
message(sprintf("%d player-games across %d monthly refits", nrow(target), length(months)))

run_month <- function(month_start) {
  games <- target |> filter(month == month_start)
  models <- fit_prop_models(features, month_start)
  sim <- simulate_player_games(models, games, n_draws = n_draws)
  list(
    pit = pit_table(sim, games) |> mutate(match_date = games$match_date[match(match_id, games$match_id)]),
    attenuation = attenuation_factors(sim)
  )
}

RNGkind("L'Ecuyer-CMRG")
set.seed(20260927)
started <- Sys.time()
results <- mclapply(months, function(m) {
  tryCatch(run_month(m), error = function(e) {
    message("Month ", m, " failed: ", conditionMessage(e))
    NULL
  })
}, mc.cores = max(1L, detectCores() - 2L))
message("Finished in ", format(round(Sys.time() - started, 1)))
if (any(map_lgl(results, is.null))) stop("Some months failed")

pit <- map_dfr(results, "pit")
attenuation <- colMeans(do.call(rbind, map(results, "attenuation")))
saveRDS(list(pit = pit, attenuation = attenuation), file.path(output_dir, "pit.rds"))
message("Attenuation: ", paste(names(attenuation), round(attenuation, 3), collapse = ", "))
