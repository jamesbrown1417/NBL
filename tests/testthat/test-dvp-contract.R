library(testthat)
library(jsonlite)
library(dplyr)
source(file.path("..", "..", "Scripts", "00-config.R"))
test_that("R compatibility bundle and web use the exact same canonical estimates", {
  bundle <- readRDS(data_file("processed_stats", "dvp_results.rds"))
  payload <- fromJSON(file.path(project_root, "web", "public", "data", "nbl-stats.json"), simplifyVector = FALSE)
  snapshot <- fromJSON(file.path(project_root, "web", "public", sub("^/", "", payload$dvp$files[[bundle$season]])), simplifyVector = FALSE)
  expect_equal(bundle$version, snapshot$version)
  labels <- c(C = "CTR", F = "FWD", G = "GRD")
  for (cell in snapshot$cells) {
    if (cell$basis != "per36") next
    stat <- ifelse(cell$stat == "pra", "pras", cell$stat)
    row <- bundle[[stat]] |> filter(opposition == cell$team, position == labels[[cell$position]])
    expect_equal(nrow(row), 1L)
    expect_equal(row$games, cell$games)
    expect_equal(row$players, cell$players)
    expect_equal(row[[paste0("avg_", stat)]], if (is.null(cell$regularized)) NA_real_ else cell$regularized)
    expect_equal(row$raw_value, if (is.null(cell$difference)) NA_real_ else cell$difference)
  }
})
