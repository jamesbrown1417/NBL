prepare_empirical_stats <- function(data) {
  required <- c("match_id", "first_name", "family_name", "season")
  if (!all(required %in% names(data))) {
    stop("Empirical stats are missing match/player identity columns.")
  }
  data |>
    dplyr::mutate(
      PLAYER_NAME = paste(first_name, family_name),
      PLAYER_NAME = dplyr::if_else(PLAYER_NAME == "Matthew Mooney", "Matt Mooney", PLAYER_NAME),
      PTS = player_points,
      REB = player_rebounds_total,
      AST = player_assists,
      STL = player_steals,
      BLK = player_blocks,
      Threes = player_three_pointers_made,
      PRA = PTS + REB + AST
    ) |>
    dplyr::distinct(season, match_id, PLAYER_NAME, .keep_all = TRUE) |>
    dplyr::select(
      PLAYER_NAME, season, match_time_utc, player_minutes,
      PTS, REB, AST, STL, BLK, Threes, PRA
    )
}

empirical_hit_rate <- function(values, line) {
  values <- values[!is.na(values)]
  if (length(values)) mean(values >= line) else NA_real_
}

get_empirical_prob <- function(player_name, line, stat, season) {
  valid_stats <- c("PTS", "REB", "AST", "STL", "BLK", "Threes", "PRA")
  if (!stat %in% valid_stats) {
    stop("stat must be one of: ", paste(valid_stats, collapse = ", "), call. = FALSE)
  }

  if (!season %in% unique(empirical_stats$season) && season != nbl_config$active_season) {
    stop("Season is not available in the stats table: ", season, call. = FALSE)
  }

  target_season <- season
  prior_season <- previous_season(target_season)

  player_history <- empirical_stats |>
    dplyr::filter(
      PLAYER_NAME == player_name,
      .data$season %in% c(prior_season, target_season),
      !is.na(player_minutes)
    ) |>
    dplyr::arrange(dplyr::desc(match_time_utc))

  current_values <- player_history |>
    dplyr::filter(.data$season == target_season) |>
    dplyr::pull(.data[[stat]])

  recent_values <- player_history |>
    dplyr::slice_head(n = 10L) |>
    dplyr::pull(.data[[stat]])

  tibble::tibble(
    games_played = length(current_values),
    empirical_prob = empirical_hit_rate(current_values, line),
    empirical_prob_last_10 = empirical_hit_rate(recent_values, line),
    line = line,
    player_name = player_name,
    season = season
  )
}
