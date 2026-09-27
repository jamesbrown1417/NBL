# Empirical player-stat probabilities -----------------------------------------

library(tidyverse)
source("Scripts/00-config.R")
source("Scripts/empirical-functions.R")

stats_rds <- data_file("processed_stats", "combined_stats_table.rds")

stats_are_stale <- function(path) {
  if (!file.exists(path)) {
    return(TRUE)
  }

  data <- read_rds(path)
  !"date_scraped" %in% names(data) ||
    all(is.na(data$date_scraped)) ||
    max(as.Date(data$date_scraped), na.rm = TRUE) < Sys.Date()
}

if (stats_are_stale(stats_rds) && Sys.getenv("NBL_SKIP_STATS_REFRESH") != "true") {
  source("Scripts/01-get-data.R")
}

combined_stats_table <- read_rds(stats_rds)
empirical_stats <- prepare_empirical_stats(combined_stats_table)
