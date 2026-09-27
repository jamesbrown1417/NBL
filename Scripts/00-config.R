# Shared season and data-path configuration -----------------------------------

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(current, "NBL.Rproj"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find the NBL project root from: ", start, call. = FALSE)
    }
    current <- parent
  }
}

season_key <- function(season) {
  gsub("-", "_", season, fixed = TRUE)
}

previous_season <- function(season) {
  if (!grepl("^[0-9]{4}-[0-9]{4}$", season)) {
    stop("Season must use YYYY-YYYY format: ", season, call. = FALSE)
  }
  years <- as.integer(strsplit(season, "-", fixed = TRUE)[[1]])
  sprintf("%d-%d", years[[1]] - 1L, years[[2]] - 1L)
}

project_root <- find_project_root()

nbl_config <- list(
  active_season = "2026-2027",
  active_season_key = season_key("2026-2027"),
  active_season_label = "2026-27",
  previous_season = previous_season("2026-2027"),
  supercoach_year = 2026L,
  supercoach_round = 1L
)

data_paths <- list(
  reference = file.path(project_root, "Data", "reference"),
  raw_stats = file.path(project_root, "Data", "raw", "stats"),
  raw_odds = file.path(project_root, "Data", "raw", "odds"),
  raw_odds_responses = file.path(project_root, "Data", "raw", "odds", "responses"),
  processed_stats = file.path(project_root, "Data", "processed", "stats"),
  processed_odds = file.path(project_root, "Data", "processed", "odds"),
  odds_archive = file.path(project_root, "Data", "archive", "odds")
)

data_file <- function(area, ...) {
  base <- data_paths[[area]]
  if (is.null(base)) {
    stop("Unknown data area: ", area, call. = FALSE)
  }
  file.path(base, ...)
}

ensure_data_directories <- function() {
  invisible(lapply(unname(data_paths), dir.create,
    recursive = TRUE, showWarnings = FALSE
  ))
}
