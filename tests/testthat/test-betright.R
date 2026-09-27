library(testthat)
br <- new.env()
local({
  old_dir <- getwd()
  old_options <- options(nbl.betright.skip_run = TRUE)
  on.exit({setwd(old_dir); options(old_options)})
  setwd(file.path("..", ".."))
  suppressMessages(sys.source("OddsScraper/scrape_BetRight.R", envir = br))
})
br_fixture <- function(id = 123456789, markets = list()) {
  list(masterEventId = id, masterEventName = "Perth Wildcats v S.E. Melbourne Phoenix",
       masterEventClassName = "Matches", minAdvertisedStartTimeUtc = "2026-09-19T11:30:00Z", markets = markets)
}
br_category <- function(events) {
  list(masterCategories = list(list(categories = list(list(categoryId = 99, masterEvents = list()),
    list(categoryId = 110, masterEvents = events)))))
}
br_outcome <- function(name, id = 1, price = 1.9) {
  list(outcomeName = name, outcomeId = id, fixedMarketId = 1000 + id, price = price, marketTypeCode = "WIN")
}
br_market <- function(name, outcomes, id = 100) list(eventName = name, eventId = id, outcomes = outcomes)
br_roster <- tibble::tibble(player_first_name = "Anthony", player_last_name = "Dell'Orso", player_team = "Perth Wildcats")
br_run <- function(markets) {
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE))
  calls <- character()
  fetch <- function(url) {
    calls <<- c(calls, url)
    if (grepl("/Category", url)) return(br_category(list(br_fixture())))
    list(masterEvent = br_fixture(), events = markets)
  }
  result <- br$main_betright(fetch, directory, br_roster)
  attr(result, "requests") <- length(calls)
  result
}

test_that("category traversal ignores futures and keeps full-length IDs without featured markets", {
  future <- br_fixture(20)
  future$masterEventClassName <- "Futures"
  x <- br$betright_matches(br_category(list(br_fixture(), future)))
  expect_equal(x$match_id, "123456789")
  expect_equal(x$away_team, "South East Melbourne Phoenix")
  expect_error(br$betright_matches(list(error = "denied")), "masterCategories")
})

test_that("head-to-head selections pair by team rather than response order", {
  x <- br_run(list(br_market("Money Line", list(br_outcome("S.E. Melbourne Phoenix", 2, 2.2),
                                                       br_outcome("Perth Wildcats", 1, 1.62)))))
  expect_equal(x$betright_h2h$home_win, 1.62)
  expect_equal(x$betright_h2h$away_win, 2.2)
  expect_equal(attr(x, "requests"), 2)
})

test_that("all five props retain threshold odds IDs and canonical roster names", {
  names <- c("Player Points", "Player Rebounds", "Player Assists", "Player Points & Assists & Rebounds", "Player Three Pointers")
  markets <- lapply(seq_along(names), function(i) br_market(paste(names[i], "- Anthony Dell'orso (PWC)"),
                                               list(br_outcome("Anthony Dell'orso 5+", i, 1.8)), i))
  x <- br_run(markets)
  for (data in x[-1]) {
    expect_equal(data$player_name, "Anthony Dell'Orso")
    expect_equal(data$line, 4.5)
    expect_equal(data$over_price, 1.8)
    expect_equal(data$opposition_team, "South East Melbourne Phoenix")
  }
  expect_equal(x$betright_player_threes$event_id, "5")
  expect_equal(x$betright_player_threes$outcome_id, "5")
  expect_equal(x$betright_player_threes$fixed_market_id, "1005")
  expect_equal(attr(x, "requests"), 2)
})

test_that("empty competitions and events retain all six output schemas", {
  x <- br_run(list())
  expect_length(x, 6)
  expect_true(all(vapply(x, nrow, integer(1)) == 0))
  directory <- tempfile()
  on.exit(unlink(directory, recursive = TRUE))
  x <- br$main_betright(function(url) br_category(list()), directory, br_roster)
  expect_true(all(vapply(x, nrow, integer(1)) == 0))
  expect_true("fixed_market_id" %in% names(x$betright_player_threes))
})

test_that("quarter markets and unpriced outcomes do not become full-game props", {
  x <- br_run(list(
    br_market("1st Quarter Player Three Pointers - Anthony Dell'orso (PWC)", list(br_outcome("Anthony Dell'orso 1+"))),
    br_market("Player Points - Anthony Dell'orso (PWC)", list(br_outcome("Anthony Dell'orso 5+", price = 0)))))
  expect_equal(nrow(x$betright_player_threes), 0)
  expect_equal(nrow(x$betright_player_points), 0)
})

test_that("unsupported thresholds and failed fetches leave outputs unchanged", {
  expect_error(br_run(list(br_market("Player Points - Anthony Dell'orso (PWC)",
                                    list(br_outcome("Anthony Dell'orso Under 5.5"))))), "unsupported player threshold")
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  path <- file.path(directory, "betright_h2h.csv")
  writeLines("previous", path)
  fetch <- function(url) {
    if (grepl("/Category", url)) return(br_category(list(br_fixture())))
    stop("request failed")
  }
  expect_error(br$main_betright(fetch, directory, br_roster), "request failed")
  expect_equal(readLines(path), "previous")
  expect_error(br$parse_betright_event(list(masterEvent = br_fixture(2), events = list()), "1"), "ID mismatch")
})
