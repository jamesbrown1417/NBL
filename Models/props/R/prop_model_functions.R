# Player prop pricing model ----------------------------------------------------
#
# Two-stage simulation model:
#   1. Minutes model   - regression on each player's prior-game minutes history,
#                        with an empirical residual distribution (conditional on
#                        the player taking the court; DNPs void props).
#   2. Rate models     - hierarchical negative binomial per stat with a
#                        log(minutes) offset and partially pooled player,
#                        player-season, team-season and opponent-season effects.
#                        Fitted by Laplace approximation (glmmTMB); predictions
#                        draw from the approximate posterior of each player's
#                        rate, so thin-sample players carry wider uncertainty.
#   3. Simulation      - joint draws of minutes -> stat counts, so combined
#                        markets (PRA) inherit the shared-minutes correlation.
#
# Optional features (settings$features), each switchable for ablation:
#   opp_position     - opponent defence by position: (1 | opp x position x season)
#   rest_travel      - team days of rest and long-haul trips (Perth / NZ)
#   teammate_absence - minutes of absent rotation teammates
#   line_minutes     - debut players' minutes implied by their points line

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(glmmTMB)
})

model_stats <- c(
  PTS = "player_points",
  REB = "player_rebounds_total",
  AST = "player_assists",
  THREES = "player_three_pointers_made",
  STL = "player_steals",
  BLK = "player_blocks"
)

market_stat_map <- c(
  "Player Points" = "PTS",
  "Player Rebounds" = "REB",
  "Player Assists" = "AST",
  "Player Threes" = "THREES",
  "Player Threes Made" = "THREES",
  "Player PRAs" = "PRA",
  "Player Steals" = "STL",
  "Player Blocks" = "BLK"
)

# Backtested 2024-25 and 2025-26: time decay (60-730 day half-lives) and six
# seasons of history were no better than four seasons, unweighted; the
# player and player-season effects already carry form.
default_model_settings <- list(
  training_seasons = 4L,   # seasons of history fed to each rate model
  half_life_days = Inf,    # time-decay half-life for rate-model weights
  n_draws = 4000L,
  # Ablation vs no features (04-compare-backtests.R): teammate_absence and
  # line_minutes each improved minutes and points pricing (|z| > 2);
  # opp_position and rest_travel showed no gain, so they are off by default.
  features = c("teammate_absence", "line_minutes")
)

long_haul_teams <- c("Perth Wildcats", "New Zealand Breakers")

# Data preparation -------------------------------------------------------------

parse_minutes <- function(x) {
  x <- as.character(x)
  out <- suppressWarnings(as.numeric(x))
  has_colon <- !is.na(x) & str_detect(x, ":")
  parts <- str_split_fixed(x[has_colon], ":", 2)
  out[has_colon] <- as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60
  out
}

normalise_position <- function(pos) {
  pos <- toupper(str_trim(pos))
  case_when(
    is.na(pos) | pos == "" ~ NA_character_,
    str_detect(pos, "^(C|CEN|CENTRE)") ~ "C",
    str_detect(pos, "^(G|PG|SG|GRD|GUARD)") ~ "G",
    TRUE ~ "F"
  )
}

prepare_player_games <- function(stats) {
  games <- stats |>
    filter(!is.na(season), !is.na(match_time_utc)) |>
    mutate(
      player_name = paste(first_name, family_name),
      player_name = if_else(player_name == "Matthew Mooney", "Matt Mooney", player_name),
      minutes = parse_minutes(player_minutes),
      match_date = as.Date(with_tz(match_time_utc, "Australia/Adelaide")),
      home = as.integer(home_away == "home"),
      home_team = if_else(home_away == "home", name, opp_name),
      away_team = if_else(home_away == "home", opp_name, name),
      match = paste0(home_team, " v ", away_team),
      team = name,
      opp = opp_name,
      position = normalise_position(playing_position),
      starter = coalesce(as.integer(starter), 0L),
      # `active` is not usable: before 2025-26 it flags players on court at the
      # final buzzer. Box scores list only players who appeared (pre 2025-26)
      # or show DNPs as 00:00 (2025-26 on), so minutes > 0 is the test.
      played = coalesce(minutes, 0) > 0,
      PTS = player_points,
      REB = player_rebounds_total,
      AST = player_assists,
      THREES = player_three_pointers_made,
      STL = player_steals,
      BLK = player_blocks
    ) |>
    distinct(match_id, player_name, .keep_all = TRUE) |>
    select(
      match_id, season, round_number, match_time_utc, match_date, match,
      team, opp, home, player_name, position, starter, played, minutes,
      PTS, REB, AST, THREES, STL, BLK
    )

  # Fill missing positions with the player's most common listed position.
  modal_position <- games |>
    filter(!is.na(position)) |>
    count(player_name, position) |>
    group_by(player_name) |>
    slice_max(n, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(player_name, modal_position = position)

  games |>
    left_join(modal_position, by = "player_name") |>
    mutate(position = coalesce(position, modal_position, "F")) |>
    select(-modal_position) |>
    arrange(match_time_utc, match_id, player_name)
}

# Match context known before tip-off.
#   rest_days   - days since the team's previous game (capped at 7; 7 for a
#                 season opener), grouped into short / normal / long
#   long_haul   - away game involving Perth or New Zealand
#   missing_min - season-average minutes (in tens) of rotation players absent
#                 from this game. Rotation = played for the team in one of its
#                 last 3 games, with >= 3 games at >= 12 minutes a game this
#                 season. Older box scores omit DNPs entirely, so absence is
#                 judged against the team's own game sequence. Players out
#                 longer than 3 games are already reflected in everyone else's
#                 recent minutes.
add_context_features <- function(games) {
  team_games <- games |>
    distinct(match_id, season, team, match_date, match_time_utc) |>
    arrange(team, season, match_time_utc) |>
    group_by(team, season) |>
    mutate(
      game_no = row_number(),
      rest_days = pmin(coalesce(as.numeric(match_date - lag(match_date)), 7), 7)
    ) |>
    ungroup()

  appearances <- games |>
    filter(played) |>
    distinct(match_id, team, player_name, minutes)

  rotation_grid <- appearances |>
    inner_join(team_games |> select(match_id, team, season), by = c("match_id", "team")) |>
    distinct(team, season, player_name) |>
    inner_join(team_games |> select(match_id, team, season, game_no), by = c("team", "season"),
               relationship = "many-to-many") |>
    left_join(appearances |> mutate(appeared = TRUE), by = c("match_id", "team", "player_name")) |>
    mutate(appeared = coalesce(appeared, FALSE)) |>
    arrange(team, season, player_name, game_no) |>
    group_by(team, season, player_name) |>
    mutate(
      prior_gp = lag(cumsum(appeared), default = 0L),
      prior_min_avg = lag(cumsum(coalesce(minutes, 0)), default = 0) / prior_gp,
      played_recent = lag(appeared, 1, default = FALSE) | lag(appeared, 2, default = FALSE) |
        lag(appeared, 3, default = FALSE),
      rotation = prior_gp >= 3 & coalesce(prior_min_avg, 0) >= 12 & played_recent
    ) |>
    ungroup()

  missing <- rotation_grid |>
    filter(rotation, !appeared) |>
    group_by(match_id, team) |>
    summarise(missing_min = sum(prior_min_avg) / 10, .groups = "drop")

  games |>
    left_join(team_games |> select(match_id, team, rest_days), by = c("match_id", "team")) |>
    left_join(missing, by = c("match_id", "team")) |>
    mutate(
      rest = factor(
        case_when(rest_days <= 2 ~ "short", rest_days <= 4 ~ "normal", TRUE ~ "long"),
        levels = c("normal", "short", "long")
      ),
      long_haul = as.integer(home == 0 & (team %in% long_haul_teams | opp %in% long_haul_teams)),
      missing_min = coalesce(missing_min, 0)
    )
}

# Pre-game minutes features, built only from each player's earlier appearances.
add_minutes_features <- function(games) {
  played <- games |>
    filter(played) |>
    arrange(player_name, match_time_utc) |>
    group_by(player_name, season) |>
    mutate(
      gp_season = row_number() - 1L,
      cur_season = lag(cummean(minutes)),
      cur_l3 = lag(zoo_mean_last(minutes, 3)),
      cur_l1 = lag(minutes, 1),
      starter_l5 = lag(zoo_mean_last(starter, 5))
    ) |>
    ungroup()

  season_summary <- games |>
    filter(played) |>
    group_by(player_name, season) |>
    summarise(
      prev_min = mean(minutes),
      prev_gp = n(),
      prev_starter = mean(starter),
      .groups = "drop"
    ) |>
    mutate(season = next_season(season))

  played |>
    left_join(season_summary, by = c("player_name", "season")) |>
    mutate(
      has_cur = !is.na(cur_season),
      has_prev = !is.na(prev_min),
      history = factor(
        case_when(
          has_cur & gp_season >= 3 ~ "cur3",
          has_cur ~ "cur1",
          has_prev ~ "prev_only",
          TRUE ~ "none"
        ),
        levels = c("cur3", "cur1", "prev_only", "none")
      ),
      across(c(cur_season, cur_l3, cur_l1, starter_l5, prev_min, prev_starter), ~ coalesce(.x, 0)),
      prev_gp = coalesce(prev_gp, 0L)
    )
}

zoo_mean_last <- function(x, k) {
  # Trailing mean over the last k values, including the current one.
  cs <- cumsum(x)
  n <- seq_along(x)
  lagged <- c(rep(0, k), cs)[n]
  (cs - lagged) / pmin(n, k)
}

next_season <- function(season) {
  years <- str_split_fixed(season, "-", 2)
  sprintf("%d-%d", as.integer(years[, 1]) + 1L, as.integer(years[, 2]) + 1L)
}

# Early in a season the player-season effects have little data and the model
# runs about one unit low. Evidence on whether that costs accuracy is mixed: in
# weeks 1-3 it beat the market in 2024-25 but trailed it in 2025-26, and early
# two-way bets were profitable in 2025-26. A conservative default - set to 1L
# to disable.
min_season_week_for_bets <- 7L

season_week <- function(match_date, season_start) {
  as.integer(match_date - season_start) %/% 7L + 1L
}

# Odds history ------------------------------------------------------------------

# All archived snapshots, keeping the latest snapshot per day for each
# match / player / market / line / agency. Includes one-sided "X+" lines.
load_odds_history <- function(dir) {
  files <- list.files(dir, "[.]rds$", full.names = TRUE)
  map_dfr(files, function(f) {
    ts <- str_match(basename(f), "__([0-9]{8}_[0-9]{6})_")[, 2]
    readRDS(f) |>
      as_tibble() |>
      select(any_of(c("match", "player_name", "market_name", "line", "agency", "over_price", "under_price"))) |>
      mutate(snapshot_datetime = ymd_hms(ts, tz = "Australia/Adelaide"))
  }) |>
    filter(!is.na(over_price)) |>
    mutate(snapshot_date = as.Date(snapshot_datetime, tz = "Australia/Adelaide")) |>
    group_by(snapshot_date, match, player_name, market_name, line, agency) |>
    slice_max(snapshot_datetime, n = 1, with_ties = FALSE) |>
    ungroup()
}

# Archived lines joined to the players who took the court (others are void),
# with the result and the Monday of the match week. Caches the snapshot load.
load_backtest_odds <- function(features, backtest_dir) {
  odds_dir <- file.path(project_root, "Historical Performance", "datasets")
  odds_cache <- file.path(backtest_dir, "odds_history.rds")
  newest_snapshot <- max(file.mtime(list.files(odds_dir, "[.]rds$", full.names = TRUE)))
  if (!file.exists(odds_cache) || newest_snapshot > file.mtime(odds_cache)) {
    saveRDS(load_odds_history(odds_dir), odds_cache)
  }
  readRDS(odds_cache) |>
    mutate(stat = unname(market_stat_map[market_name])) |>
    filter(!is.na(stat)) |>
    select(match, match_date = snapshot_date, player_name, line, market_name, stat, agency, over_price, under_price) |>
    inner_join(
      features |> select(match, match_date, player_name, match_id, season, minutes, all_of(names(model_stats))),
      by = c("match", "match_date", "player_name")
    ) |>
    mutate(
      outcome = case_when(
        stat == "PTS" ~ PTS, stat == "REB" ~ REB, stat == "AST" ~ AST,
        stat == "THREES" ~ THREES, stat == "PRA" ~ PTS + REB + AST,
        stat == "STL" ~ STL, stat == "BLK" ~ BLK
      ),
      refit_date = floor_date(match_date, "week", week_start = 1)
    ) |>
    select(-all_of(names(model_stats)))
}

# Minutes model ------------------------------------------------------------------

minutes_formula <- function(features) {
  terms <- c(
    "history", "history:cur_season", "history:cur_l3", "history:cur_l1", "history:starter_l5",
    "history:prev_min", "history:prev_starter", "history:log1p(prev_gp)", "home",
    if ("rest_travel" %in% features) c("rest", "long_haul"),
    # Absences free minutes mostly for established rotation players.
    if ("teammate_absence" %in% features) c("missing_min", "missing_min:cur_season")
  )
  as.formula(paste("minutes ~", paste(terms, collapse = " + ")))
}

fit_minutes_model <- function(train, features) {
  fit <- lm(minutes_formula(features), data = train)
  resid_bins <- tibble(pred = fitted(fit), resid = residuals(fit)) |>
    mutate(bin = cut(pred, breaks = minutes_bin_breaks, include.lowest = TRUE))
  list(
    fit = fit,
    residuals = split(resid_bins$resid, resid_bins$bin)
  )
}

minutes_bin_breaks <- c(-Inf, 8, 12, 16, 20, 24, 28, 32, Inf)

# Debut players (history "none") have nothing but the population mean. When a
# points line exists, the book's view of their role is far more informative:
# minutes ~ log(points line), fitted on earlier players with lines.
fit_line_minutes_model <- function(line_data, min_rows = 200L) {
  if (is.null(line_data) || nrow(line_data) < min_rows) {
    return(NULL)
  }
  lm(minutes ~ log(pts_line), data = line_data)
}

simulate_minutes <- function(minutes_model, newdata, n_draws, line_model = NULL) {
  # history == "none" has no numeric features, so those terms are aliased.
  pred <- suppressWarnings(predict(minutes_model$fit, newdata = newdata))
  if (!is.null(line_model) && "pts_line" %in% names(newdata)) {
    use_line <- newdata$history == "none" & !is.na(newdata$pts_line)
    if (any(use_line)) {
      pred[use_line] <- predict(line_model, newdata = newdata[use_line, ])
    }
  }
  bins <- as.character(cut(pred, breaks = minutes_bin_breaks, include.lowest = TRUE))
  draws <- matrix(NA_real_, nrow = length(pred), ncol = n_draws)
  for (i in seq_along(pred)) {
    pool <- minutes_model$residuals[[bins[i]]]
    draws[i, ] <- pred[i] + sample(pool, n_draws, replace = TRUE)
  }
  list(mean = pred, draws = pmin(pmax(draws, 1), 50))
}

# Rate models --------------------------------------------------------------------

rate_formula <- function(stat, features) {
  terms <- c(
    "position", "home", "offset(log(minutes))",
    if ("rest_travel" %in% features) c("rest", "long_haul"),
    if ("teammate_absence" %in% features) "missing_min",
    "(1 | player_name)", "(1 | player_season)", "(1 | team_season)", "(1 | opp_season)",
    if ("opp_position" %in% features) "(1 | opp_pos_season)"
  )
  as.formula(paste(stat, "~", paste(terms, collapse = " + ")))
}

prepare_rate_data <- function(games) {
  games |>
    mutate(
      player_season = paste(player_name, season, sep = "|"),
      team_season = paste(team, season, sep = "|"),
      opp_season = paste(opp, season, sep = "|"),
      opp_pos_season = paste(opp, position, season, sep = "|")
    )
}

fit_rate_model <- function(train, stat, cutoff_date, settings = default_model_settings) {
  days_ago <- as.numeric(cutoff_date - train$match_date)
  w <- 0.5^(days_ago / settings$half_life_days)  # all 1 when half-life is Inf
  train$w <- w / mean(w)

  fit <- suppressWarnings(glmmTMB(
    rate_formula(stat, settings$features),
    data = train,
    family = nbinom2(),
    weights = w,
    control = glmmTMBControl(parallel = 1)
  ))
  theta <- sigma(fit)

  # No detectable overdispersion (theta -> Inf) leaves a singular Hessian;
  # refit as Poisson, the NB limit.
  if (!isTRUE(fit$sdr$pdHess) || theta > 1e3) {
    fit <- glmmTMB(
      rate_formula(stat, settings$features),
      data = train,
      family = poisson(),
      weights = w,
      control = glmmTMBControl(parallel = 1)
    )
    theta <- Inf
  }

  re_sd <- vapply(VarCorr(fit)$cond, function(v) attr(v, "stddev")[[1]], numeric(1))
  list(
    fit = fit,
    theta = theta,
    re_sd = re_sd,
    re_levels = lapply(ranef(fit)$cond, rownames)
  )
}

# Posterior draws of the per-minute log-rate. Levels unseen in training (a debut
# import, a new team-season) fall back to the population mean with the full
# between-group variance added, i.e. a draw from the hierarchical prior.
draw_log_rates <- function(rate_model, newdata, n_draws) {
  nd <- newdata |> mutate(minutes = 1, w = 1)
  pred <- predict(
    rate_model$fit,
    newdata = nd,
    type = "link",
    se.fit = TRUE,
    allow.new.levels = TRUE
  )

  extra_var <- rep(0, nrow(nd))
  for (grp in names(rate_model$re_sd)) {
    unseen <- !(nd[[grp]] %in% rate_model$re_levels[[grp]])
    extra_var[unseen] <- extra_var[unseen] + rate_model$re_sd[[grp]]^2
  }

  sd_total <- sqrt(pred$se.fit^2 + extra_var)
  z <- matrix(rnorm(nrow(nd) * n_draws), nrow = nrow(nd))
  pred$fit + sd_total * z
}

# Fit / simulate -------------------------------------------------------------

# line_data: earlier player-games with a points line (pts_line, minutes), for
# the line_minutes feature.
fit_prop_models <- function(features, cutoff_date, stats = names(model_stats),
                            settings = default_model_settings, line_data = NULL) {
  seasons <- sort(unique(features$season[features$match_date < cutoff_date]))
  keep_seasons <- tail(seasons, settings$training_seasons)

  history <- features |> filter(match_date < cutoff_date)
  rate_train <- history |>
    filter(season %in% keep_seasons, minutes >= 1) |>
    prepare_rate_data()

  list(
    cutoff_date = cutoff_date,
    minutes = fit_minutes_model(history, settings$features),
    line_minutes = if ("line_minutes" %in% settings$features) fit_line_minutes_model(line_data),
    rates = set_names(map(stats, ~ fit_rate_model(rate_train, .x, cutoff_date, settings)), stats)
  )
}

# Returns a list of draw matrices (rows = player-games in `newdata`).
simulate_player_games <- function(models, newdata, n_draws = default_model_settings$n_draws) {
  newdata <- prepare_rate_data(newdata)
  mins <- simulate_minutes(models$minutes, newdata, n_draws, models$line_minutes)

  sims <- map(models$rates, function(rm) {
    log_rate <- draw_log_rates(rm, newdata, n_draws)
    mu <- exp(log_rate) * mins$draws
    counts <- if (is.finite(rm$theta)) {
      rnbinom(length(mu), size = rm$theta, mu = mu)
    } else {
      rpois(length(mu), mu)
    }
    matrix(counts, nrow = nrow(mu))
  })

  if (all(c("PTS", "REB", "AST") %in% names(sims))) {
    sims$PRA <- sims$PTS + sims$REB + sims$AST
  }

  list(
    keys = newdata |> select(match_id, player_name),
    minutes_mean = mins$mean,
    draws = sims
  )
}

# Log score of each player-game's actual result under the simulated
# distribution (higher is better). Scores whole distributions, so it uses every
# player-game, not just those with odds. Probabilities floored at 1 / n_draws.
score_player_games <- function(sim, actual) {
  idx <- match(
    paste(sim$keys$match_id, sim$keys$player_name),
    paste(actual$match_id, actual$player_name)
  )
  actual <- actual[idx, ]
  actual$PRA <- actual$PTS + actual$REB + actual$AST
  imap_dfr(sim$draws, function(draws, stat) {
    y <- actual[[stat]]
    p <- rowMeans(draws == y)
    tibble(
      match_id = sim$keys$match_id,
      player_name = sim$keys$player_name,
      stat = stat,
      outcome = y,
      log_score = log(pmax(p, 1 / ncol(draws))),
      abs_error = abs(rowMeans(draws) - y)
    )
  }) |>
    bind_rows(tibble(
      match_id = sim$keys$match_id,
      player_name = sim$keys$player_name,
      stat = "MIN",
      outcome = actual$minutes,
      log_score = NA_real_,
      abs_error = abs(sim$minutes_mean - actual$minutes)
    ))
}

# Pricing helpers ------------------------------------------------------------

# P(over), P(under) for a line, excluding pushes on whole-number lines.
line_probabilities <- function(draws_row, line) {
  p_over <- mean(draws_row > line)
  p_under <- mean(draws_row < line)
  total <- p_over + p_under
  c(over = p_over / total, under = p_under / total)
}

price_lines <- function(sim, lines) {
  # lines: data frame with match_id, player_name, stat, line
  idx <- match(
    paste(lines$match_id, lines$player_name),
    paste(sim$keys$match_id, sim$keys$player_name)
  )
  probs <- map2(seq_len(nrow(lines)), idx, function(i, r) {
    if (is.na(r) || !lines$stat[i] %in% names(sim$draws)) {
      return(c(over = NA_real_, under = NA_real_))
    }
    line_probabilities(sim$draws[[lines$stat[i]]][r, ], lines$line[i])
  })
  probs <- do.call(rbind, probs)

  lines |>
    mutate(
      model_mean = map2_dbl(idx, stat, function(r, s) {
        if (is.na(r) || !s %in% names(sim$draws)) NA_real_ else mean(sim$draws[[s]][r, ])
      }),
      model_sd = map2_dbl(idx, stat, function(r, s) {
        if (is.na(r) || !s %in% names(sim$draws)) NA_real_ else sd(sim$draws[[s]][r, ])
      }),
      model_minutes = sim$minutes_mean[idx],
      model_p_over = probs[, "over"],
      model_p_under = probs[, "under"],
      fair_over_price = 1 / model_p_over,
      fair_under_price = 1 / model_p_under
    )
}

devig_two_way <- function(over_price, under_price) {
  io <- 1 / over_price
  iu <- 1 / under_price
  io / (io + iu)
}

# Market-conditioned probabilities --------------------------------------------
#
# The raw model is well calibrated across whole distributions, but where it
# disagrees with a market price the market is usually partly right. Final
# probabilities therefore combine both on the logit scale, per stat, with
# weights fitted on the backtest (02-evaluate-backtest.R):
#   two-way lines  - model + de-vigged consensus of the books offering that line
#   one-sided X+   - model + the book's own implied probability (margin included)
blend_logit <- function(p) qlogis(pmin(pmax(p, 0.001), 0.999))

# Alt legs (one-sided X+ lines) for SGMs: ranked by raw-model EV against
# historical cut-offs (the 80th / 90th / 95th percentiles of backtest legs).
max_alt_leg_price <- 6
long_shot_tier <- "price > 6"  # not ranked: blind and model-selected long shots both lost heavily

assign_alt_leg_tier <- function(model_ev, cutoffs) {
  case_when(
    model_ev >= cutoffs[[3]] ~ "top 5%",
    model_ev >= cutoffs[[2]] ~ "top 10%",
    model_ev >= cutoffs[[1]] ~ "top 20%",
    TRUE ~ "rest"
  )
}

apply_probability_maps <- function(priced, maps) {
  two <- maps$two_way[match(priced$stat, maps$two_way$stat), ]
  alt <- maps$one_sided[match(priced$stat, maps$one_sided$stat), ]
  priced |>
    mutate(
      two_way_p = plogis(two$intercept + two$model_coef * blend_logit(model_p_over) +
        two$market_coef * blend_logit(market_p_over)),
      one_sided_p = plogis(alt$intercept + alt$model_coef * blend_logit(model_p_over) +
        alt$market_coef * blend_logit(1 / over_price)),
      final_p_over = case_when(
        !is.na(market_p_over) & !is.na(two_way_p) ~ two_way_p,
        !is.na(one_sided_p) ~ one_sided_p,
        TRUE ~ model_p_over
      ),
      price_source = case_when(
        !is.na(market_p_over) & !is.na(two_way_p) ~ "blend: two-way consensus",
        !is.na(one_sided_p) ~ "blend: book price",
        TRUE ~ "raw model"
      )
    ) |>
    select(-two_way_p, -one_sided_p)
}
