library(testthat)
pb <- new.env()
local({
  old_dir <- getwd()
  old_options <- options(nbl.pointsbet.skip_run = TRUE)
  on.exit({setwd(old_dir); options(old_options)})
  setwd(file.path("..", ".."))
  suppressMessages(sys.source("OddsScraper/scrape_pointsbet.R", envir = pb))
})

pb_selection <- function(name, key = "s1", price = 1.9, type = NULL, open = TRUE, hidden = FALSE) {
  list(name = name, key = key, price = price, outcomeType = type, isOpenForBetting = open, isHidden = hidden)
}
pb_market <- function(name, outcomes, key = "m1", period = "FT", open = TRUE, event_name = name) {
  list(name = name, eventName = event_name, key = key, outcomes = outcomes, period = period, isOpenForBetting = open)
}
pb_event <- function(markets = list(), key = "e1") {
  list(key = key, homeTeam = "Melbourne United", awayTeam = "Adelaide 36ers", startsAt = "2026-09-19T09:36:00Z",
       fixedOddsMarkets = markets, isLive = FALSE)
}
pb_roster <- tibble::tibble(player_first_name = "Chris", player_last_name = "Goulding", player_team = "Melbourne United")
pb_fetch <- function(event) {
  force(event)
  function(url) {
    if (grepl("/featured", url)) return(list(events = list(list(key = event$key, specialFixedOddsMarkets = list())), nextPage = NULL))
    event
  }
}
pb_run <- function(event) {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE))
  pb$pointsbet_h2h_main(pb_fetch(event), directory, pb_roster)
}

test_that("events without featured markets are still fetched and empty props keep schema", {
  out <- pb_run(pb_event(list(pb_market("Head to Head", list(
    pb_selection("Adelaide 36ers", "away", 2.95), pb_selection("Melbourne United", "home", 1.4))))))
  expect_equal(out$pointsbet_h2h$home_win, 1.4)
  expect_equal(out$pointsbet_h2h$away_win, 2.95)
  expect_equal(nrow(out$pointsbet_player_points), 0)
  expect_true(all(c("OutcomeKey", "OutcomeKey_unders", "under_price") %in% names(out$pointsbet_player_points)))
})

test_that("alternate and over-under props retain thresholds and correct SGM keys", {
  out <- pb_run(pb_event(list(
    pb_market("To Get 10+ Points", list(pb_selection("Chris Goulding", "alt")), "altm"),
    pb_market("Player Points Over/Under", list(
      pb_selection("Chris Goulding Over 12.5", "over", 1.85, "Over"),
      pb_selection("Chris Goulding Under 12.5", "under", 1.95, "Under")), "pairm"))))
  x <- out$pointsbet_player_points
  expect_equal(x$line, c(9.5, 12.5))
  expect_equal(x$OutcomeKey, c("alt", "over"))
  expect_equal(x$OutcomeKey_unders, c(NA_character_, "under"))
  expect_equal(x$MarketKey, c("altm", "pairm"))
  expect_equal(x$under_price, c(NA_real_, 1.95))
  expect_true(all(x$opposition_team == "Adelaide 36ers"))
})

test_that("under-only prices are retained without an invented over selection", {
  out <- pb_run(pb_event(list(pb_market("Player Rebounds Over/Under", list(
    pb_selection("Chris Goulding Under 3.5", "under", 1.95, "Under"))))))
  expect_equal(out$pointsbet_player_rebounds$OutcomeKey_unders, "under")
  expect_true(is.na(out$pointsbet_player_rebounds$OutcomeKey))
  expect_true(is.na(out$pointsbet_player_rebounds$over_price))
})

test_that("assists rebounds and threes alternate formats are supported", {
  out <- pb_run(pb_event(list(
    pb_market("To Get 4+ Assists", list(pb_selection("Chris Goulding"))),
    pb_market("To Get 6+ Rebounds", list(pb_selection("Chris Goulding"))),
    pb_market("To Make 3+ Made Threes", list(pb_selection("Chris Goulding"))))))
  expect_equal(out$pointsbet_player_assists$line, 3.5)
  expect_equal(out$pointsbet_player_rebounds$line, 5.5)
  expect_equal(out$pointsbet_player_threes$line, 2.5)
})

test_that("closed hidden unpriced and partial-game selections are excluded", {
  market <- pb_market("To Get 10+ Points", list(
    pb_selection("Chris Goulding", "valid"), pb_selection("Chris Goulding", "closed", open = FALSE),
    pb_selection("Chris Goulding", "hidden", hidden = TRUE), pb_selection("Chris Goulding", "unpriced", price = 0)))
  event <- pb_event(list(market, pb_market("To Get 10+ Points", list(pb_selection("Chris Goulding")), period = "Q1")))
  expect_equal(pb$parse_pointsbet_event(event)$OutcomeKey, "valid")
  event$isLive <- TRUE
  expect_equal(nrow(pb$parse_pointsbet_event(event)), 0)
})

test_that("empty competitions clear stale outputs and malformed responses fail", {
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  writeLines("stale", file.path(directory, "pointsbet_player_points.csv"))
  out <- pb$pointsbet_h2h_main(function(url) list(events = list()), directory, pb_roster)
  expect_true(all(vapply(out, nrow, integer(1)) == 0))
  expect_equal(nrow(readr::read_csv(file.path(directory, "pointsbet_player_points.csv"), show_col_types = FALSE)), 0)
  expect_error(pb$pointsbet_h2h_main(function(url) list(error = "denied"), directory, pb_roster), "events list")
  expect_error(pb$parse_pointsbet_event(list()), "incomplete event")
})

test_that("failed event requests leave previous files unchanged", {
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  path <- file.path(directory, "pointsbet_h2h.csv")
  writeLines("previous", path)
  fetch <- function(url) {
    if (grepl("/featured", url)) return(list(events = list(list(key = "e1"))))
    stop("request failed")
  }
  expect_error(pb$pointsbet_h2h_main(fetch, directory, pb_roster), "request failed")
  expect_equal(readLines(path), "previous")
})

test_that("roster conflicts cannot create a false opponent", {
  data <- tibble::tibble(player_name = "Transferred Player", player_team = "Cairns Taipans",
                        home_team = "Melbourne United", away_team = "Adelaide 36ers", opposition_team = "Melbourne United")
  expect_warning(out <- pb$validate_pointsbet_roster(data), "unresolved")
  expect_true(is.na(out$player_team))
  expect_true(is.na(out$opposition_team))
})
