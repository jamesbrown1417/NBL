library(tidyverse)
library(jsonlite)

source("Scripts/00-config.R")

stats <- read_rds(data_file("processed_stats", "combined_stats_table.rds"))

minutes_to_number <- function(value) {
  value <- as.character(value)
  has_clock <- str_detect(value, ":")
  result <- suppressWarnings(as.numeric(value))
  parts <- str_split_fixed(replace_na(value, ""), ":", 2)
  result[has_clock] <- suppressWarnings(
    as.numeric(parts[has_clock, 1]) + as.numeric(parts[has_clock, 2]) / 60
  )
  result
}

player_games <- stats |>
  filter(!is.na(first_name), !is.na(family_name), !is.na(season)) |>
  transmute(
    matchId = as.character(match_id),
    season,
    date = format(as.Date(match_time_utc), "%Y-%m-%d"),
    round = as.character(round_number),
    player = str_squish(paste(first_name, family_name)),
    team = name,
    opponent = opp_name,
    homeAway = home_away,
    position = playing_position,
    pace = as.numeric(pace),
    starter = as.logical(starter),
    minutes = minutes_to_number(player_minutes),
    points = as.numeric(player_points),
    rebounds = as.numeric(player_rebounds_total),
    assists = as.numeric(player_assists),
    threes = as.numeric(player_three_pointers_made),
    steals = as.numeric(player_steals),
    blocks = as.numeric(player_blocks),
    turnovers = as.numeric(player_turnovers),
    plusMinus = as.numeric(plus_minus_points),
    fga = as.numeric(player_field_goals_attempted), fgm = as.numeric(player_field_goals_made),
    threeAttempts = as.numeric(player_three_pointers_attempted),
    fta = as.numeric(player_free_throws_attempted), ftm = as.numeric(player_free_throws_made),
    offensiveRebounds = as.numeric(player_rebounds_offensive), defensiveRebounds = as.numeric(player_rebounds_defensive),
    fouls = as.numeric(player_fouls_personal), foulsDrawn = as.numeric(player_fouls_received),
    paintPoints = as.numeric(player_points_in_the_paint), fastBreakPoints = as.numeric(player_points_fast_break),
    secondChancePoints = as.numeric(player_points_second_chance)
  ) |>
  arrange(date, matchId, player)

team_games <- stats |>
  filter(!is.na(name), !is.na(season)) |>
  distinct(match_id, name, .keep_all = TRUE) |>
  transmute(
    matchId = as.character(match_id),
    season,
    date = format(as.Date(match_time_utc), "%Y-%m-%d"),
    round = as.character(round_number),
    team = name,
    opponent = opp_name,
    homeAway = home_away,
    points = as.numeric(score),
    opponentPoints = as.numeric(opp_score),
    rebounds = as.numeric(match_rebounds_total),
    assists = as.numeric(match_assists),
    turnovers = as.numeric(match_turnovers),
    steals = as.numeric(match_steals),
    blocks = as.numeric(match_blocks),
    fieldGoalPct = as.numeric(match_field_goals_percentage),
    threePct = as.numeric(match_three_pointers_percentage),
    freeThrowPct = as.numeric(match_free_throws_percentage),
    pace = as.numeric(pace), possessions = as.numeric(possessions),
    gameMinutes = 40 + 5 * as.numeric(extra_periods_used),
    fga = as.numeric(match_field_goals_attempted), fgm = as.numeric(match_field_goals_made),
    threes = as.numeric(match_three_pointers_made), threeAttempts = as.numeric(match_three_pointers_attempted),
    fta = as.numeric(match_free_throws_attempted), ftm = as.numeric(match_free_throws_made),
    offensiveRebounds = as.numeric(match_rebounds_offensive), defensiveRebounds = as.numeric(match_rebounds_defensive),
    fouls = as.numeric(match_fouls_personal), foulsDrawn = as.numeric(match_fouls_received),
    paintPoints = as.numeric(match_points_in_the_paint), fastBreakPoints = as.numeric(match_points_fast_break),
    secondChancePoints = as.numeric(match_points_second_chance), benchPoints = as.numeric(bench_points),
    q1 = as.numeric(p1_score), q2 = as.numeric(p2_score), q3 = as.numeric(p3_score), q4 = as.numeric(p4_score)
  ) |>
  arrange(date, matchId, team)


# Estimate team pace only when the source pace is unavailable. Both opponents must
# have complete box-score inputs; this is not measured on-court player exposure.
estimated_pace <- team_games |>
  mutate(boxPossessions = fga - offensiveRebounds + turnovers + 0.44 * fta) |>
  group_by(matchId) |>
  summarise(estimatedPace = if (n() == 2L && all(is.finite(boxPossessions)) && all(is.finite(gameMinutes)) && n_distinct(gameMinutes) == 1L && first(gameMinutes) > 0) mean(boxPossessions) * 40 / first(gameMinutes) else NA_real_, .groups = "drop")
player_games <- player_games |>
  left_join(estimated_pace, by = "matchId") |>
  mutate(paceSource = if_else(is.finite(pace) & pace > 0, "source team pace", "estimated from both team box scores"),
         pace = if_else(is.finite(pace) & pace > 0, pace, estimatedPace)) |>
  select(-estimatedPace)

available_seasons <- sort(unique(c(player_games$season, team_games$season)), decreasing = TRUE)

payload <- list(
  metadata = list(
    generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    activeSeason = nbl_config$active_season,
    latestSeasonWithGames = first(available_seasons),
    seasons = c(nbl_config$active_season, available_seasons)
  ),
  playerGames = player_games,
  teamGames = team_games
)

output_dir <- file.path(project_root, "web", "public", "data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
staging_file <- tempfile("nbl-stats-", tmpdir = output_dir, fileext = ".json")
write_json(
  payload,
  staging_file,
  dataframe = "rows",
  auto_unbox = TRUE,
  na = "null",
  digits = NA
)

message(
  "Prepared ", nrow(player_games), " player games and ",
  nrow(team_games), " team games for canonical validation"
)

# The shared TypeScript estimator validates duplicate records before final publication.
# Stage both the ordinary statistics and DVP snapshot, then replace the export atomically.
node <- Sys.which("node")
if (!nzchar(node)) stop("Node >= 22.13 is required for the canonical DVP export")
completed_file <- paste0(staging_file, ".complete")
status <- system2(node, c("--experimental-strip-types", shQuote(file.path(project_root, "Scripts", "export-dvp.mjs")), shQuote(staging_file), shQuote(completed_file)))
unlink(staging_file)
if (status != 0L) {
  unlink(completed_file)
  stop("Canonical DVP validation failed; previous web export retained")
}
if (!file.rename(completed_file, file.path(output_dir, "nbl-stats.json"))) stop("Could not publish validated web export")
