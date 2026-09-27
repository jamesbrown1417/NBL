# Canonical DVP export. No independent R estimator and no network refresh.
library(tidyverse)
library(jsonlite)
source("Scripts/00-config.R")
source("Scripts/12-export-web-stats.R")

payload <- fromJSON(file.path(project_root, "web", "public", "data", "nbl-stats.json"), simplifyVector = FALSE)
requested_season <- Sys.getenv("NBL_DVP_SEASON", unset = nbl_config$active_season)
season_target <- requested_season
if (is.null(payload$dvp$files[[season_target]])) {
  if (nzchar(Sys.getenv("NBL_DVP_SEASON"))) stop("Requested DVP season has no games: ", season_target)
  season_target <- payload$metadata$latestSeasonWithGames
  message("No games for ", requested_season, "; exporting explicitly labelled ", season_target, " DVP")
}
snapshot <- fromJSON(file.path(project_root, "web", "public", sub("^/", "", payload$dvp$files[[season_target]])), simplifyVector = FALSE)
number_or_na <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
position_labels <- c(C = "CTR", F = "FWD", G = "GRD")
all_cells <- map_dfr(snapshot$cells, function(cell) {
  tibble(opposition = cell$team, position = unname(position_labels[[cell$position]]),
         stat = ifelse(cell$stat == "pra", "pras", cell$stat), basis = cell$basis,
         games = as.integer(cell$games), players = as.integer(cell$players),
         appearances = as.integer(cell$appearances), effective_players = cell$effectivePlayers,
         target_minutes = cell$targetMinutes, baseline_minutes = cell$baselineMinutes,
         value = number_or_na(cell$regularized), raw_value = number_or_na(cell$difference),
         standard_error = number_or_na(cell$standardError),
         sufficient = cell$sufficient, shrinkage = number_or_na(cell$shrinkage),
         lower = if (is.null(cell$interval)) NA_real_ else cell$interval[[1]],
         upper = if (is.null(cell$interval)) NA_real_ else cell$interval[[2]])
})
# Legacy consumers keep their familiar stat names, but games now means distinct matches.
# Unreliable/unavailable estimates remain NA, never zero or an old season's unlabeled value.
dvp_all <- all_cells |> filter(basis == "per36")
dvp_list <- set_names(c("points", "rebounds", "assists", "threes", "steals", "blocks", "pras")) |>
  map(function(stat_name) {
    dvp_all |> filter(stat == stat_name) |>
      mutate(!!paste0("avg_", stat_name) := value) |>
      select(-stat, -value) |>
      arrange(position, desc(.data[[paste0("avg_", stat_name)]]))
  })
dvp_results <- c(list(season = season_target, requested_season = requested_season,
                     version = payload$dvp$version, per = 36, min_min = snapshot$rules$minMinutes,
                     min_games = snapshot$rules$minDistinctGames, offset = 0,
                     rules = snapshot$rules, coverage = snapshot[c("eligible", "unknown", "inherited", "duplicates")]),
                dvp_list, list(all = dvp_all, all_bases = all_cells, snapshot = snapshot))
outputs <- c(set_names(dvp_list, paste0("dvp_", names(dvp_list))), list(dvp_all = dvp_all, dvp_results = dvp_results))
ensure_data_directories()
iwalk(outputs, function(object, name) {
  path <- data_file("processed_stats", paste0(name, ".rds"))
  staging <- tempfile("dvp-", tmpdir = dirname(path))
  saveRDS(object, staging)
  if (!file.rename(staging, path)) stop("Could not publish ", path)
})
message("Saved canonical DVP ", payload$dvp$version, " for ", season_target, "; raw and regularized values retained")
