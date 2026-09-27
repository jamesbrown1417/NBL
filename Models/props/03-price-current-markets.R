# Price current player prop markets ---------------------------------------------
#
# Fits every model on all completed games, simulates each player in the
# current scraped markets, and compares fair prices with what the books offer.
# Requires Models/props/output/probability_maps.rds (run 01 then 02 first).
# Output: Models/props/output/current_prices.{rds,csv}
#
# bet_signal marks two-way lines whose blended EV clears 5% at a price <= 6,
# from week 7 of the season: the only segment that was profitable out of
# sample (2025-26). One-sided X+ lines are priced too, but their margins beat
# the model in the backtest.
#
# alt_leg_tier ranks one-sided X+ legs (price <= 6) for SGM building by the raw
# model's EV against backtest cut-offs (longer prices form their own tier);
# alt_leg_backtest_roi is that tier's average single-bet return across 2024-25
# and 2025-26, a more realistic guide than the raw model edge on these lines. Top-tier legs were
# close to fair (about -2% to -4%) where the rest lost about 30-38%.
#
# Team news: list players ruled out in Data/reference/player_outs.csv (one
# column, player_name). They are dropped from pricing and counted as absent
# rotation minutes for their teammates. Everyone else who played in their
# team's last 3 games is assumed available.

min_ev <- 0.05
max_price <- 6

source("Scripts/00-config.R")
source("Models/props/R/prop_model_functions.R")

output_dir <- file.path(project_root, "Models", "props", "output")
maps <- readRDS(file.path(output_dir, "probability_maps.rds"))

games <- readRDS(data_file("processed_stats", "combined_stats_table.rds")) |>
  prepare_player_games()
today <- Sys.Date()
history <- games |> filter(match_date < today)
current_week <- season_week(today, min(games$match_date[games$season == nbl_config$active_season]))
if (current_week < min_season_week_for_bets) {
  message(sprintf(
    "Season week %d: bet signals are suppressed until week %d (min_season_week_for_bets)",
    current_week, min_season_week_for_bets
  ))
}

odds <- list.files(data_paths$processed_odds, "^all_player_.*\\.rds$", full.names = TRUE) |>
  map_dfr(readRDS) |>
  mutate(stat = unname(market_stat_map[market_name])) |>
  filter(!is.na(stat)) |>
  select(match, home_team, away_team, player_name, player_team, opposition_team,
         market_name, stat, line, agency, over_price, under_price)

outs_file <- file.path(data_paths$reference, "player_outs.csv")
outs <- if (file.exists(outs_file)) readr::read_csv(outs_file, show_col_types = FALSE)$player_name else character()
if (length(outs)) message("Ruled out: ", paste(outs, collapse = ", "))

# Expected rosters: props players plus anyone who played in the team's last 3
# games this season, minus ruled-out players.
fixtures <- odds |>
  distinct(match, home_team, away_team) |>
  pivot_longer(c(home_team, away_team), names_to = "side", values_to = "player_team") |>
  mutate(opposition_team = if_else(side == "home_team", str_remove(match, "^.* v "), str_remove(match, " v .*$")))
recent_players <- history |>
  filter(season == nbl_config$active_season, played) |>
  group_by(team) |>
  filter(match_id %in% (distinct(pick(match_id, match_time_utc)) |> slice_max(match_time_utc, n = 3) |> pull(match_id))) |>
  ungroup() |>
  distinct(player_team = team, player_name)
expected_roster <- bind_rows(
  odds |> distinct(match, player_name, player_team),
  fixtures |> select(match, player_team) |> inner_join(recent_players, by = "player_team", relationship = "many-to-many")
) |>
  distinct(match, player_name, .keep_all = TRUE) |>
  filter(!player_name %in% outs) |>
  inner_join(fixtures |> select(match, player_team, opposition_team, home_team_flag = side), by = c("match", "player_team"))

points_lines <- odds |>
  filter(stat == "PTS", !is.na(under_price)) |>
  group_by(match, player_name) |>
  summarise(pts_line = median(line), .groups = "drop")

# Build one pre-game row per player per upcoming match, with features computed
# from completed games only (each match is appended to history alone).
upcoming <- expected_roster |>
  left_join(
    history |> group_by(player_name) |> slice_max(match_time_utc, n = 1, with_ties = FALSE) |>
      ungroup() |> select(player_name, position),
    by = "player_name"
  ) |>
  mutate(
    match_id = paste("upcoming", match),
    season = nbl_config$active_season,
    match_time_utc = as.POSIXct(today, tz = "UTC"),
    match_date = today,
    team = player_team,
    opp = opposition_team,
    home = as.integer(home_team_flag == "home_team"),
    position = coalesce(position, "F"),
    starter = 0L,
    played = TRUE,
    minutes = NA_real_
  )

upcoming_features <- upcoming |>
  group_split(match) |>
  map_dfr(function(m) {
    bind_rows(history, select(m, any_of(names(history)))) |>
      add_context_features() |>
      add_minutes_features() |>
      filter(match_id == m$match_id[1])
  }) |>
  left_join(points_lines, by = c("match", "player_name"))

history_features <- history |>
  add_context_features() |>
  add_minutes_features()

# Earlier player-games with a consensus two-way points line, for line_minutes.
odds_history <- file.path(output_dir, "backtest", "odds_history.rds")
line_data <- if (file.exists(odds_history)) {
  readRDS(odds_history) |>
    filter(market_name == "Player Points", !is.na(under_price)) |>
    group_by(match, match_date = snapshot_date, player_name) |>
    summarise(pts_line = median(line), .groups = "drop") |>
    inner_join(history_features |> select(match, match_date, player_name, minutes),
               by = c("match", "match_date", "player_name"))
}

models <- fit_prop_models(history_features, today, line_data = line_data)
set.seed(as.integer(today))
sim <- simulate_player_games(models, upcoming_features)

tier_roi <- maps$alt_leg_tiers |>
  mutate(alt_leg_backtest_roi = rowMeans(pick(starts_with("roi_")))) |>
  select(alt_leg_tier = tier, alt_leg_backtest_roi)

priced <- odds |>
  filter(!player_name %in% outs) |>
  mutate(match_id = paste("upcoming", match)) |>
  price_lines(sim, lines = _) |>
  mutate(book_p_over = devig_two_way(over_price, under_price)) |>
  group_by(match, player_name, stat, line) |>
  mutate(market_p_over = if (all(is.na(book_p_over))) NA_real_ else mean(book_p_over, na.rm = TRUE)) |>
  ungroup() |>
  apply_probability_maps(maps) |>
  left_join(
    upcoming_features |> select(match, player_name, history),
    by = c("match", "player_name")
  ) |>
  mutate(
    final_p_under = 1 - final_p_over,
    fair_over = 1 / final_p_over,
    fair_under = 1 / final_p_under,
    ev_over = final_p_over * over_price - 1,
    ev_under = final_p_under * under_price - 1,
    best_side = if_else(coalesce(ev_under, -Inf) > ev_over, "under", "over"),
    best_ev = pmax(ev_over, coalesce(ev_under, -Inf)),
    best_price = if_else(best_side == "over", over_price, under_price),
    thin_history = history %in% c("prev_only", "none"),
    alt_leg_model_ev = if_else(is.na(under_price) & over_price <= max_alt_leg_price,
                               model_p_over * over_price - 1, NA_real_),
    alt_leg_tier = case_when(
      !is.na(under_price) ~ NA_character_,
      over_price > max_alt_leg_price ~ long_shot_tier,
      TRUE ~ assign_alt_leg_tier(alt_leg_model_ev, maps$alt_leg_cutoffs)
    ),
    bet_signal = !is.na(under_price) & best_ev >= min_ev & best_price <= max_price & !thin_history &
      current_week >= min_season_week_for_bets
  ) |>
  select(
    match, player_name, player_team, market_name, line, agency, over_price, under_price,
    model_mean, model_minutes, model_p_over, market_p_over, final_p_over, price_source,
    fair_over, fair_under, best_side, best_ev, bet_signal,
    alt_leg_model_ev, alt_leg_tier, thin_history
  ) |>
  left_join(tier_roi, by = "alt_leg_tier") |>
  arrange(desc(bet_signal), desc(best_ev))

saveRDS(priced, file.path(output_dir, "current_prices.rds"))
saveRDS(
  list(season_week = current_week, signals_from_week = min_season_week_for_bets),
  file.path(output_dir, "current_prices_meta.rds")
)
readr::write_csv(priced, file.path(output_dir, "current_prices.csv"))

message(sprintf(
  "Priced %d lines for %d players across %d matches; %d bet signals; %d top-5%% alt legs",
  nrow(priced), n_distinct(priced$player_name), n_distinct(priced$match), sum(priced$bet_signal),
  sum(priced$alt_leg_tier == "top 5%", na.rm = TRUE)
))
