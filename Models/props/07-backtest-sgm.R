# Out-of-sample SGM backtest -----------------------------------------------------
#
# For each 2025-26 week with archived odds: refit the prop model on earlier
# games, simulate every player in the week's matches, and apply the copula with
# correlations estimated on earlier seasons only (06, "pre_test"). Then sample
# SGM-style combinations of archived legs and compare three joint prices with
# what actually happened:
#   p_indep      - product of single-leg probabilities (what an uncorrelated
#                  multi assumes)
#   p_same_only  - copula with same-player correlations only
#   p_copula     - full Gaussian copula (same player, teammates, opponents)
#   p_t8, p_t4   - full t-copula with 8 / 4 degrees of freedom (tail dependence)
# Combination types:
#   random          - any 2-4 legs from the match
#   under_plus_alt  - a two-way under plus 1-2 one-sided X+ overs
# Output: Models/props/output/joint/sgm_backtest.rds

suppressPackageStartupMessages({
  library(parallel)
})
source("Scripts/00-config.R")
source("Models/props/R/prop_model_functions.R")
source("Models/props/R/joint_functions.R")

test_season <- Sys.getenv("PROP_SGM_TEST_SEASON", "2025-2026")
backtest_dir <- file.path(project_root, "Models", "props", "output", "backtest")
joint_dir <- file.path(project_root, "Models", "props", "output", "joint")
# pre_test excludes 2025-26; a 2024-25 run (for choosing the t-copula df)
# reuses it, so its correlations include that season.
params <- readRDS(file.path(joint_dir, "correlation_params.rds"))$pre_test
same_only <- params
same_only$cor[c(teammate_relations, "opp")] <- map(same_only$cor[c(teammate_relations, "opp")], ~ .x * 0)

combo_plan <- tribble(
  ~type,            ~n_legs, ~n_combos,
  "random",         2L,      300L,
  "random",         3L,      150L,
  "random",         4L,      100L,
  "under_plus_alt", 2L,      150L,
  "under_plus_alt", 3L,      100L
)

features <- readRDS(data_file("processed_stats", "combined_stats_table.rds")) |>
  prepare_player_games() |>
  add_context_features() |>
  add_minutes_features() |>
  add_usual_role()

odds <- load_backtest_odds(features, backtest_dir)
points_lines <- odds |>
  filter(stat == "PTS", !is.na(under_price)) |>
  group_by(match_id, player_name, match_date, minutes) |>
  summarise(pts_line = median(line), .groups = "drop")

# Distinct legs (half-point lines only, so no pushes): every line can be taken
# over; unders only where a book offered a two-way line.
legs <- odds |>
  filter(season == test_season, line != floor(line)) |>
  group_by(refit_date, match_id, player_name, stat, line, outcome) |>
  summarise(two_way = any(!is.na(under_price)), .groups = "drop")
legs <- bind_rows(
  legs |> mutate(side = "over", hit = outcome > line),
  legs |> filter(two_way) |> mutate(side = "under", hit = outcome < line)
)

sample_combos <- function(match_legs, type, n_legs, n_combos) {
  first_pool <- if (type == "under_plus_alt") which(match_legs$side == "under") else seq_len(nrow(match_legs))
  rest_pool <- if (type == "under_plus_alt") which(match_legs$side == "over" & !match_legs$two_way) else seq_len(nrow(match_legs))
  if (length(first_pool) == 0 || length(rest_pool) < n_legs - 1) return(list())
  combos <- list()
  for (attempt in seq_len(n_combos * 5)) {
    pick <- c(sample(first_pool, 1), sample(rest_pool, n_legs - 1))
    key <- paste(match_legs$player_name[pick], match_legs$stat[pick])
    # One leg per player-stat: a ladder of the same stat is not a real SGM.
    if (anyDuplicated(key) == 0) combos[[length(combos) + 1]] <- pick
    if (length(combos) == n_combos) break
  }
  combos
}

combo_relation <- function(match_legs, pick, team_of) {
  if (length(pick) != 2) return("mixed")
  a <- match_legs$player_name[pick[1]]
  b <- match_legs$player_name[pick[2]]
  if (a == b) "same player" else if (team_of[[a]] == team_of[[b]]) "teammates" else "opponents"
}

run_week <- function(week_start) {
  week_legs <- legs |> filter(refit_date == week_start)
  week_games <- features |>
    filter(match_id %in% week_legs$match_id) |>
    left_join(points_lines |> select(match_id, player_name, pts_line), by = c("match_id", "player_name"))
  models <- fit_prop_models(features, week_start, line_data = points_lines |> filter(match_date < week_start))
  sim <- simulate_player_games(models, week_games)

  map_dfr(unique(week_legs$match_id), function(mid) {
    rows <- which(sim$keys$match_id == mid)
    players <- week_games[match(paste(mid, sim$keys$player_name[rows]), paste(week_games$match_id, week_games$player_name)), ]
    match_sim <- subset_sim(sim, rows)
    variants <- list(
      same_only = apply_copula(match_sim, players, same_only, df = Inf),
      copula = apply_copula(match_sim, players, params, df = Inf),
      t8 = apply_copula(match_sim, players, params, df = 8),
      t4 = apply_copula(match_sim, players, params, df = 4)
    )
    match_legs <- week_legs |>
      filter(match_id == mid, player_name %in% players$player_name)
    if (nrow(match_legs) < 4) return(NULL)

    # Leg indicator matrices (draws x legs) under each dependence structure.
    indicators <- map(variants, function(v) {
      sapply(seq_len(nrow(match_legs)), function(k) {
        x <- v$draws[[match_legs$stat[k]]][match(match_legs$player_name[k], v$keys$player_name), ]
        if (match_legs$side[k] == "over") x > match_legs$line[k] else x < match_legs$line[k]
      })
    })
    p_leg <- colMeans(indicators$copula)
    team_of <- set_names(players$team, players$player_name)

    pmap_dfr(combo_plan, function(type, n_legs, n_combos) {
      combos <- sample_combos(match_legs, type, n_legs, n_combos)
      map_dfr(combos, function(pick) {
        tibble(
          match_id = mid,
          refit_date = week_start,
          type = type,
          n_legs = n_legs,
          relation = combo_relation(match_legs, pick, team_of),
          legs = paste(match_legs$player_name[pick], match_legs$stat[pick], match_legs$side[pick],
                       match_legs$line[pick], collapse = " | "),
          p_indep = prod(p_leg[pick]),
          p_same_only = mean(apply(indicators$same_only[, pick, drop = FALSE], 1, all)),
          p_copula = mean(apply(indicators$copula[, pick, drop = FALSE], 1, all)),
          p_t8 = mean(apply(indicators$t8[, pick, drop = FALSE], 1, all)),
          p_t4 = mean(apply(indicators$t4[, pick, drop = FALSE], 1, all)),
          hit = all(match_legs$hit[pick])
        )
      })
    })
  })
}

RNGkind("L'Ecuyer-CMRG")
set.seed(20260927)
weeks <- sort(unique(legs$refit_date))
started <- Sys.time()
results <- mclapply(weeks, function(w) {
  tryCatch(run_week(w), error = function(e) {
    message("Week ", w, " failed: ", conditionMessage(e))
    NULL
  })
}, mc.cores = max(1L, detectCores() - 2L))
message("Finished in ", format(round(Sys.time() - started, 1)))
if (any(map_lgl(results, is.null))) stop("Some weeks failed")

combos <- bind_rows(results)
saveRDS(combos, file.path(joint_dir, paste0("sgm_backtest_", test_season, ".rds")))
message(sprintf("%d combinations across %d matches", nrow(combos), n_distinct(combos$match_id)))

evaluate_sgm_backtest(combos)
