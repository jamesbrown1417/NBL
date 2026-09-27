library(testthat)

tab_parser <- new.env()
local({
  old_dir <- getwd()
  old_options <- options(nbl.tab.skip_run = TRUE)
  on.exit({setwd(old_dir); options(old_options)})
  setwd(file.path("..", ".."))
  suppressMessages(sys.source("OddsScraper/TAB/scrape_TAB.R", envir = tab_parser))
})

tab_prop <- function(id, name, price = 1.9, status = "Open") {
  list(id = id, name = name, returnWin = price, bettingStatus = status, isOpen = status == "Open")
}
tab_market <- function(name, props, status = "Open") {
  list(betOption = name, propositions = props, bettingStatus = status, onlineBetting = TRUE)
}
tab_match <- function(name = "Melbourne v Adelaide", markets = list()) {
  list(name = name, startTime = "2026-09-19T09:37:00Z", markets = markets)
}
tab_roster <- tibble::tibble(
  player_first_name = c("Will", "Jo", "Chris"),
  player_last_name = c("McDowell-White", "Lual-Acuil Jr", "Goulding"),
  player_team = c("Melbourne United", "Adelaide 36ers", "Melbourne United"))
run_tab_fixture <- function(response) {
  path <- tempfile(fileext = ".json")
  out_dir <- tempfile()
  on.exit(unlink(c(path, out_dir), recursive = TRUE))
  jsonlite::write_json(response, path, auto_unbox = TRUE)
  tab_parser$main_tab(path, out_dir, tab_roster)
}

test_that("empty and suspended responses produce typed empty files", {
  result <- run_tab_fixture(list(matches = list()))
  expect_length(result, 6)
  expect_true(all(vapply(result, nrow, integer(1)) == 0))
  expect_true(all(c("player_name", "prop_id", "under_prop_id") %in% names(result$tab_player_points)))
  response <- list(matches = list(tab_match(markets = list(
    tab_market("Head To Head", list(tab_prop("1", "Melbourne")), "Suspended")))))
  expect_equal(nrow(tab_parser$tab_market_rows(response)), 0)
  expect_error(tab_parser$tab_market_rows(list(error = "Access denied")), "competition response")
})

test_that("head-to-head prices follow team names when selections arrive reversed", {
  out <- run_tab_fixture(list(matches = list(tab_match(markets = list(
    tab_market("Head To Head", list(tab_prop("2", "Adelaide", 2.8), tab_prop("1", "Melbourne", 1.42))))))))
  expect_equal(out$tab_h2h$home_win, 1.42)
  expect_equal(out$tab_h2h$away_win, 2.8)
})

test_that("multiword names, thresholds, paired prices and SGM IDs survive parsing", {
  out <- run_tab_fixture(list(matches = list(tab_match(markets = list(
    tab_market("Player Points", list(tab_prop("over", "W McDowell-White Over 12.5 Pts"),
                                     tab_prop("under", "W McDowell-White Under 12.5 Pts", 1.8))),
    tab_market("10+ Points", list(tab_prop("alt", "J Lual-Acuil Jr"))),
    tab_market("Total Points Over/Under", list(tab_prop("to", "Over 180", 1.85), tab_prop("tu", "Under 180", 1.95)))
  )))))
  points <- out$tab_player_points
  will <- points[points$player_name == "Will McDowell-White", ]
  expect_equal(will$line, 12.5)
  expect_equal(will$prop_id, "over")
  expect_equal(will$under_prop_id, "under")
  expect_equal(will$under_price, 1.8)
  jo <- points[points$player_name == "Jo Lual-Acuil Jr", ]
  expect_equal(jo$line, 9.5)
  expect_equal(jo$player_team, "Adelaide 36ers")
  expect_equal(jo$opposition_team, "Melbourne United")
  expect_equal(out$tab_total_points$line, 180)
})

test_that("the same player threshold is retained in different fixtures", {
  market <- tab_market("10+ Points", list(tab_prop("1", "C Goulding")))
  out <- run_tab_fixture(list(matches = list(
    tab_match("Melbourne v Adelaide", list(market)),
    tab_match("Adelaide v Melbourne", list(market)))))
  expect_equal(nrow(out$tab_player_points), 2)
})

test_that("stale response files cannot silently refresh old odds", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  jsonlite::write_json(list(matches = list()), path)
  Sys.setFileTime(path, Sys.time() - 3600)
  expect_error(tab_parser$main_tab(path, tempfile(), tab_roster), "stale")
})

test_that("ambiguous and unknown roster names are retained and flagged", {
  roster <- dplyr::bind_rows(tab_roster, tibble::tibble(player_first_name = "Will", player_last_name = "McDowell White", player_team = "Adelaide 36ers"))
  data <- tibble::tibble(market_name = "Player Points", player_name = c("W McDowell-White", "New Player"),
                         home_team = "Melbourne United", away_team = "Adelaide 36ers")
  expect_warning(out <- tab_parser$resolve_tab_players(data, roster), "unresolved")
  expect_true(all(is.na(out$player_team)))
  expect_equal(out$player_name, data$player_name)
  expect_true(all(is.na(out$opposition_team)))
})


test_that("abbreviated TAB surnames resolve without duplicating Jr", {
  data <- tibble::tibble(market_name = "Player Points", player_name = c("W McD-White", "J Lual-Acuil"),
                         home_team = "Melbourne United", away_team = "Adelaide 36ers")
  result <- tab_parser$resolve_tab_players(data, tab_roster)
  expect_equal(result$player_name, c("Will McDowell-White", "Jo Lual-Acuil Jr"))
})

test_that("suspended or unpriced propositions are not published", {
  response <- list(matches = list(tab_match(markets = list(tab_market("Player Points", list(
    tab_prop("s", "C Goulding Over 10.5", status = "Suspended"),
    tab_prop("z", "C Goulding Under 10.5", price = 0),
    tab_prop("o", "C Goulding Over 12.5", price = 1.9)))))))
  result <- tab_parser$tab_market_rows(response)
  expect_equal(result$prop_id, "o")
})

test_that("assists rebounds and threes retain alternate thresholds and proposition IDs", {
  markets <- lapply(c("Assists", "Rebounds", "Threes"), function(stat) {
    tab_market(paste("3+", stat), list(tab_prop(stat, "C Goulding")))
  })
  out <- run_tab_fixture(list(matches = list(tab_match(markets = markets))))
  for (stat in c("assists", "rebounds", "threes")) {
    expect_equal(out[[paste0("tab_player_", stat)]]$line, 2.5)
    expect_equal(out[[paste0("tab_player_", stat)]]$player_name, "Chris Goulding")
  }
})


test_that("TAB's full-first-name Jackson-Cartwright abbreviation resolves", {
  roster <- tibble::tibble(player_first_name = "Parker", player_last_name = "Jackson-Cartwright", player_team = "New Zealand Breakers")
  data <- tibble::tibble(market_name = "Player Points", player_name = "Parker J-Cartwright",
                        home_team = "New Zealand Breakers", away_team = "Illawarra Hawks")
  out <- tab_parser$resolve_tab_players(data, roster)
  expect_equal(out$player_name, "Parker Jackson-Cartwright")
  expect_equal(out$player_team, "New Zealand Breakers")
})
