library(testthat)
library(dplyr)
library(tibble)

source(file.path("..", "..", "Scripts", "00-config.R"))
source(file.path("..", "..", "Scripts", "empirical-functions.R"))
source(file.path("..", "..", "Scripts", "odds-schema.R"))

test_that("season helpers produce canonical values", {
  expect_equal(season_key("2026-2027"), "2026_2027")
  expect_equal(previous_season("2026-2027"), "2025-2026")
  expect_error(previous_season("2026-27"), "YYYY-YYYY")
})

test_that("project root is found from a nested directory", {
  expect_equal(
    find_project_root(file.path(project_root, "Apps", "NBL_APP")),
    project_root
  )
})

test_that("missing bookmaker outputs retain the processed schema", {
  result <- read_processed_player_odds(tempfile("missing-odds-"))
  expect_equal(nrow(result), 0)
  expect_true(all(c(
    "empirical_prob_over_current", "diff_over_current",
    "empirical_prob_over_last_10", "model_season"
  ) %in% names(result)))
})

fixture <- tibble(
  match_id = paste0("fixture-", seq_len(5)),
  first_name = rep("Test", 5),
  family_name = rep("Player", 5),
  season = c(rep("2025-2026", 3), rep("2026-2027", 2)),
  match_time_utc = as.POSIXct("2026-01-01", tz = "UTC") + seq_len(5) * 86400,
  player_minutes = rep("20:00", 5),
  player_points = c(8, 12, 15, 9, 14),
  player_rebounds_total = c(4, 5, 6, 3, 7),
  player_assists = c(1, 2, 3, 4, 5),
  player_steals = c(0, 1, 1, 0, 2),
  player_blocks = c(0, 0, 1, 1, 0),
  player_three_pointers_made = c(1, 2, 2, 1, 3)
)

test_that("duplicated player-game rows count only once", {
  repeated <- bind_rows(fixture, fixture[1, ])
  empirical_stats <<- prepare_empirical_stats(repeated)
  result <- get_empirical_prob("Test Player", 10, "PTS", "2026-2027")
  expect_equal(result$games_played, 2)
  expect_equal(result$empirical_prob_last_10, 3 / 5)
  expect_equal(nrow(empirical_stats), 5)
})

test_that("current and rolling probabilities span the season boundary", {
  empirical_stats <<- prepare_empirical_stats(fixture)
  result <- get_empirical_prob("Test Player", 10, "PTS", "2026-2027")

  expect_equal(result$games_played, 2)
  expect_equal(result$empirical_prob, 0.5)
  expect_equal(result$empirical_prob_last_10, 3 / 5)
  expect_equal(result$season, "2026-2027")
})

test_that("preseason and unknown players return stable NA rows", {
  empirical_stats <<- prepare_empirical_stats(fixture |> filter(season == "2025-2026"))

  preseason <- get_empirical_prob("Test Player", 10, "PTS", "2026-2027")
  unknown <- get_empirical_prob("Missing Player", 10, "PTS", "2026-2027")

  expect_equal(preseason$games_played, 0)
  expect_true(is.na(preseason$empirical_prob))
  expect_equal(preseason$empirical_prob_last_10, 2 / 3)
  expect_equal(unknown$games_played, 0)
  expect_true(is.na(unknown$empirical_prob_last_10))
})

test_that("invalid statistics and unavailable seasons fail clearly", {
  empirical_stats <<- prepare_empirical_stats(fixture)
  expect_error(get_empirical_prob("Test Player", 10, "TURNOVERS", "2026-2027"), "stat must")
  expect_error(get_empirical_prob("Test Player", 10, "PTS", "2030-2031"), "not available")
})
