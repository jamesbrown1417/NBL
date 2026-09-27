library(testthat)

sb <- new.env()
local({
  old_dir <- getwd()
  old_options <- options(nbl.sportsbet.skip_run = TRUE)
  on.exit({setwd(old_dir); options(old_options)})
  setwd(file.path("..", ".."))
  suppressMessages(sys.source("OddsScraper/scrape_sportsbet.R", envir = sb))
})

card <- function(separator = "at", prices = c("2.70", "1.47")) {
  paste0('<div data-automation-id="123456789-competition-event-card">',
    '<a href="/betting/basketball-aus-other/australian-nbl/adelaide-36ers-',
    separator, '-melbourne-united-123456789"></a>',
    '<span data-automation-id="1-two-outcome-captioned-label">Adelaide 36ers</span>',
    '<span data-automation-id="2-two-outcome-captioned-label">Melbourne United</span>',
    '<span data-automation-id="1-two-outcome-captioned-text">', prices[1], '</span>',
    '<span data-automation-id="2-two-outcome-captioned-text">', prices[2], '</span></div>')
}

test_that("away-at-home cards retain the correct odds and arbitrary-length IDs", {
  result <- sb$parse_sportsbet_matches(rvest::read_html(card()))
  expect_equal(result$home_team, "Melbourne United")
  expect_equal(result$away_team, "Adelaide 36ers")
  expect_equal(result$home_win, 1.47)
  expect_equal(result$away_win, 2.70)
  expect_equal(result$match_id, 123456789)
})

test_that("versus cards and duplicate cards are handled consistently", {
  result <- sb$parse_sportsbet_matches(rvest::read_html(paste(card("v"), card("v"))))
  expect_equal(nrow(result), 1L)
  expect_equal(result$home_team, "Adelaide 36ers")
  expect_equal(result$home_win, 2.70)
})

test_that("blocked pages and invalid prices fail visibly", {
  expect_error(sb$parse_sportsbet_matches(rvest::read_html("<html>Access Denied</html>")), "no event cards")
  expect_error(sb$parse_sportsbet_matches(rvest::read_html(card(prices = c("", "1.47")))), "invalid head-to-head")
})

test_that("old and missing roster assignments cannot manufacture an opponent", {
  data <- tibble::tibble(player_name = c("Transferred Player", "New Player", "Known Player"),
    player_team = c("Cairns Taipans", NA, "Melbourne United"),
    home_team = "Melbourne United", away_team = "Adelaide 36ers",
    opposition_team = c("Melbourne United", NA, "Adelaide 36ers"))
  expect_warning(result <- sb$validate_sportsbet_roster(data), "roster needs updating")
  expect_true(all(is.na(result$player_team[1:2])))
  expect_true(all(is.na(result$opposition_team[1:2])))
  expect_equal(result$opposition_team[3], "Adelaide 36ers")
})
