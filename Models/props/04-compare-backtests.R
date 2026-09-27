# Compare backtest variants -------------------------------------------------
#
# Usage: Rscript Models/props/04-compare-backtests.R _baseline _full _only_rest_travel ...
# The first tag is the reference. Each tag names outputs of 01-backtest.R run
# with PROP_BACKTEST_TAG=<tag>; "default" means the untagged default run.
#
# Differences are paired on identical player-games / lines and their standard
# errors are clustered by match (players in one game share its randomness).
#   log_score  - full-distribution log score of the actual result (higher is better)
#   min_mae    - minutes mean absolute error (lower is better)
#   line_ll    - log loss on archived prop lines (lower is better)

source("Scripts/00-config.R")
source("Models/props/R/prop_model_functions.R")

tags <- commandArgs(trailingOnly = TRUE)
if (length(tags) < 2) stop("Pass at least two backtest tags; the first is the reference.")
backtest_dir <- file.path(project_root, "Models", "props", "output", "backtest")
read_tag <- function(kind, tag) {
  suffix <- if (tag == "default") "" else tag
  readRDS(file.path(backtest_dir, paste0(kind, suffix, ".rds")))
}

# Mean of paired differences with a match-clustered standard error.
clustered_diff <- function(diff, cluster) {
  n <- length(diff)
  mean_diff <- mean(diff)
  totals <- tapply(diff - mean_diff, cluster, sum)
  g <- length(totals)
  se <- sqrt(g / (g - 1) * sum(totals^2)) / n
  tibble(n = n, diff = mean_diff, se = se, z = mean_diff / se)
}

compare <- function(ref, alt, value, keys, higher_is_better) {
  paired <- inner_join(
    ref |> select(all_of(keys), ref_value = all_of(value)),
    alt |> select(all_of(keys), alt_value = all_of(value)),
    by = keys
  ) |>
    filter(!is.na(ref_value), !is.na(alt_value))
  sign <- if (higher_is_better) 1 else -1
  # Report as improvement: positive = alt is better than the reference.
  paired |>
    group_by(stat) |>
    group_modify(~ clustered_diff(sign * (.x$alt_value - .x$ref_value), .x$match_id)) |>
    ungroup()
}

line_log_loss <- function(lines) {
  lines |>
    filter(outcome != line, !is.na(model_p_over)) |>
    mutate(
      p = pmin(pmax(model_p_over, 0.001), 0.999),
      y = as.integer(outcome > line),
      line_ll = -(y * log(p) + (1 - y) * log(1 - p))
    ) |>
    distinct(match_id, player_name, stat, line, agency, .keep_all = TRUE)
}

ref_scores <- read_tag("player_game_scores", tags[1])
ref_lines <- line_log_loss(read_tag("backtest_lines", tags[1]))

results <- map_dfr(tags[-1], function(tag) {
  scores <- read_tag("player_game_scores", tag)
  lines <- line_log_loss(read_tag("backtest_lines", tag))
  score_keys <- c("match_id", "player_name", "stat")
  bind_rows(
    compare(ref_scores |> filter(stat != "MIN"), scores |> filter(stat != "MIN"),
            "log_score", score_keys, TRUE) |> mutate(metric = "log_score"),
    compare(ref_scores |> filter(stat == "MIN"), scores |> filter(stat == "MIN"),
            "abs_error", score_keys, FALSE) |> mutate(metric = "min_mae"),
    compare(ref_scores |> filter(stat == "MIN", history == "none"),
            scores |> filter(stat == "MIN", history == "none"),
            "abs_error", score_keys, FALSE) |> mutate(metric = "min_mae_debut"),
    compare(ref_lines, lines, "line_ll", c("match_id", "player_name", "stat", "line", "agency"), FALSE) |>
      mutate(metric = "line_ll")
  ) |>
    mutate(variant = tag)
})

cat("Improvement over reference '", tags[1], "' (positive = better; |z| > 2 is meaningful)\n", sep = "")
results |>
  mutate(summary = sprintf("%+.4f (z %+.1f)", diff, z)) |>
  select(metric, stat, variant, summary) |>
  pivot_wider(names_from = variant, values_from = summary) |>
  arrange(metric, stat) |>
  print(n = Inf, width = Inf)

cat("\nReference levels (", tags[1], "):\n", sep = "")
ref_scores |>
  group_by(stat) |>
  summarise(n = n(), mean_log_score = mean(log_score), mae = mean(abs_error), .groups = "drop") |>
  mutate(across(where(is.double), ~ round(.x, 3))) |>
  print(n = Inf)
