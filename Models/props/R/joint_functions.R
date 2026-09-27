# Joint (same game multi) model -------------------------------------------------
#
# Player marginals come from the prop model unchanged. Dependence between legs
# is added with a Gaussian copula: correlated normal scores decide how each
# player-stat's simulated draws are ordered, so every single-leg probability is
# preserved exactly while joint outcomes become correlated.
#
# Correlations are estimated from historical residuals: the randomised PIT of
# each actual result under its walk-forward predictive distribution, mapped to
# normal scores. This is the dependence left after the model's own minutes and
# role information, which is exactly what the copula has to supply.
#
# Structure (pooled by relationship, 6 stats):
#   same     - same player, stat pairs
#   mate_SS / mate_SB / mate_BB - teammates by usual role (starter / bench)
#   opp      - opponents

joint_stats <- c("PTS", "REB", "AST", "THREES", "STL", "BLK")

# t-copula degrees of freedom. Chosen on the 2024-25 SGM backtest (07): the
# Gaussian copula under-priced longshot combinations (joint probability < 1%
# hit 1.20x as often as predicted); df = 8 brought that to 1.02, and held up
# best out of sample in 2025-26 (1.17 vs 1.40 Gaussian). df = 4 overshot.
default_copula_df <- 8
teammate_relations <- c("mate_SS", "mate_SB", "mate_BB")

# Usual role before the game: starter in at least half of the last 5 games this
# season, else last season's starter share; debutants default to bench.
add_usual_role <- function(games) {
  games |>
    mutate(role = case_when(
      history %in% c("cur3", "cur1") ~ if_else(starter_l5 >= 0.5, "S", "B"),
      history == "prev_only" ~ if_else(prev_starter >= 0.5, "S", "B"),
      TRUE ~ "B"
    ))
}

# Randomised PIT of y under each row's draws: F(y - 1) + V * (F(y) - F(y - 1)).
randomised_pit <- function(draws, y) {
  f_y <- rowMeans(draws <= y)
  f_below <- rowMeans(draws <= y - 1)
  u <- f_below + runif(length(y)) * (f_y - f_below)
  pmin(pmax(u, 1e-4), 1 - 1e-4)
}

pit_table <- function(sim, games) {
  idx <- match(
    paste(sim$keys$match_id, sim$keys$player_name),
    paste(games$match_id, games$player_name)
  )
  games <- games[idx, ]
  map_dfr(intersect(joint_stats, names(sim$draws)), function(s) {
    tibble(
      match_id = games$match_id,
      player_name = games$player_name,
      team = games$team,
      role = games$role,
      season = games$season,
      stat = s,
      pit = randomised_pit(sim$draws[[s]], games[[s]])
    )
  })
}

# Discreteness attenuates normal-score correlations: a latent correlation rho
# shows up as roughly rho * a_s * a_t. a_s is measured by pushing a known normal
# score through a player's predictive distribution and back via the randomised PIT.
attenuation_factors <- function(sim, rows_per_stat = 150L, n = 2000L) {
  map_dbl(set_names(intersect(joint_stats, names(sim$draws))), function(stat) {
    draws <- sim$draws[[stat]]
    rows <- sample(nrow(draws), min(rows_per_stat, nrow(draws)))
    mean(map_dbl(rows, function(r) {
      z <- rnorm(n)
      sorted <- sort(draws[r, ])
      x <- sorted[pmax(1L, ceiling(pnorm(z) * length(sorted)))]
      cdf <- ecdf(draws[r, ])
      u <- cdf(x - 1) + runif(n) * (cdf(x) - cdf(x - 1))
      suppressWarnings(cor(z, qnorm(pmin(pmax(u, 1e-4), 1 - 1e-4))))
    }), na.rm = TRUE)
  })
}

# Estimation -------------------------------------------------------------------

relation_of <- function(team_a, team_b, role_a, role_b) {
  roles <- if_else(role_a == role_b, paste0(role_a, role_b), "SB")
  if_else(team_a == team_b, paste0("mate_", roles), "opp")
}

pit_pairs <- function(pit) {
  wide <- pit |>
    mutate(z = qnorm(pit)) |>
    select(match_id, player_name, team, role, stat, z) |>
    pivot_wider(names_from = stat, values_from = z)
  pairs <- wide |>
    inner_join(wide, by = "match_id", suffix = c("_a", "_b"), relationship = "many-to-many") |>
    filter(player_name_a != player_name_b) |>
    mutate(relation = relation_of(team_a, team_b, role_a, role_b))
  list(wide = wide, pairs = pairs)
}

correlation_matrices <- function(pp, attenuation) {
  latent <- function(obs) {
    att <- attenuation[joint_stats]
    pmin(pmax(obs / outer(att, att), -0.95), 0.95)
  }
  cor_block <- function(a, b) {
    m <- matrix(NA_real_, length(joint_stats), length(joint_stats), dimnames = list(joint_stats, joint_stats))
    for (s in joint_stats) for (t in joint_stats) m[s, t] <- cor(a[[s]], b[[t]], use = "complete.obs")
    (m + t(m)) / 2
  }

  same <- latent(cor_block(pp$wide, pp$wide))
  diag(same) <- 1
  cross <- map(set_names(c(teammate_relations, "opp")), function(rel) {
    d <- pp$pairs |> filter(relation == rel)
    latent(cor_block(
      d |> select(ends_with("_a")) |> rename_with(~ str_remove(.x, "_a$")),
      d |> select(ends_with("_b")) |> rename_with(~ str_remove(.x, "_b$"))
    ))
  })
  c(list(same = same), cross)
}

estimate_joint_params <- function(pit, attenuation, n_boot = 100L) {
  pp <- pit_pairs(pit)
  point <- correlation_matrices(pp, attenuation)

  # Match-clustered bootstrap for standard errors.
  matches <- unique(pit$match_id)
  boot <- map(seq_len(n_boot), function(i) {
    ids <- sample(matches, replace = TRUE)
    counts <- table(ids)
    reps <- tibble(match_id = names(counts), k = as.integer(counts))
    expand <- function(d) d |> inner_join(reps, by = "match_id") |> tidyr::uncount(k)
    correlation_matrices(list(wide = expand(pp$wide), pairs = expand(pp$pairs)), attenuation)
  })
  se <- map(set_names(names(point)), function(nm) {
    apply(simplify2array(map(boot, nm)), c(1, 2), sd)
  })
  list(cor = point, se = se, attenuation = attenuation, n_matches = length(matches))
}

# Copula simulation -------------------------------------------------------------

# Correlation matrix over every (player, stat) in a match.
match_correlation <- function(players, params) {
  cells <- expand_grid(p = seq_len(nrow(players)), stat = joint_stats)
  d <- nrow(cells)
  R <- diag(d)
  for (i in seq_len(d - 1)) {
    for (j in (i + 1):d) {
      a <- cells$p[i]
      b <- cells$p[j]
      rel <- if (a == b) {
        "same"
      } else {
        relation_of(players$team[a], players$team[b], players$role[a], players$role[b])
      }
      R[i, j] <- R[j, i] <- params$cor[[rel]][cells$stat[i], cells$stat[j]]
    }
  }
  # Pooled blocks need not combine into a valid matrix; project to the nearest
  # correlation matrix when they do not.
  if (min(eigen(R, symmetric = TRUE, only.values = TRUE)$values) < 1e-8) {
    R <- as.matrix(Matrix::nearPD(R, corr = TRUE)$mat)
  }
  list(R = R, cells = cells)
}

# Reorders a match's marginal draws so they follow the copula. `sim` rows must
# all belong to one match; `players` gives team and role in the same order.
# df < Inf gives a t-copula: a shared scale per draw makes extreme outcomes
# cluster (tail dependence), which a Gaussian copula cannot produce.
apply_copula <- function(sim, players, params, df = default_copula_df) {
  n <- ncol(sim$draws[[joint_stats[1]]])
  mc <- match_correlation(players, params)
  z <- matrix(rnorm(n * nrow(mc$R)), nrow = n) %*% chol(mc$R)
  if (is.finite(df)) z <- z * sqrt(df / rchisq(n, df))
  joint <- map(set_names(joint_stats), ~ sim$draws[[.x]] * 0L)
  for (k in seq_len(nrow(mc$cells))) {
    stat <- mc$cells$stat[k]
    p <- mc$cells$p[k]
    joint[[stat]][p, ] <- sort(sim$draws[[stat]][p, ])[rank(z[, k], ties.method = "first")]
  }
  joint$PRA <- joint$PTS + joint$REB + joint$AST
  list(keys = sim$keys, draws = joint)
}

subset_sim <- function(sim, rows) {
  list(
    keys = sim$keys[rows, ],
    minutes_mean = sim$minutes_mean[rows],
    draws = map(sim$draws, ~ .x[rows, , drop = FALSE])
  )
}

# SGM backtest evaluation ----------------------------------------------------

sgm_methods <- c(indep = "p_indep", same_only = "p_same_only", gaussian = "p_copula", t8 = "p_t8", t4 = "p_t4")

evaluate_sgm_backtest <- function(combos) {
  methods <- sgm_methods[sgm_methods %in% names(combos)]
  log_loss_vec <- function(p, y) {
    p <- pmin(pmax(p, 1e-4), 1 - 1e-4)
    -(y * log(p) + (1 - y) * log(1 - p))
  }
  clustered_z <- function(diff, cluster) {
    totals <- tapply(diff - mean(diff), cluster, sum)
    g <- length(totals)
    mean(diff) / (sqrt(g / (g - 1) * sum(totals^2)) / length(diff))
  }
  ratio_table <- function(d) {
    d |> summarise(n = n(), hit_rate = mean(hit), across(all_of(unname(methods)), ~ sum(hit) / sum(.x)), .groups = "drop") |>
      rename(any_of(set_names(unname(methods), names(methods)))) |>
      mutate(across(where(is.double), ~ round(.x, 3)))
  }

  cat("\n== Calibration: actual hits / expected hits (1.00 = right) by combination group ==\n")
  combos |>
    mutate(group = if_else(n_legs == 2, paste(type, "2-leg", relation), paste(type, paste0(n_legs, "-leg")))) |>
    group_by(group) |>
    ratio_table() |>
    print(n = Inf, width = Inf)

  cat("\n== Calibration by probability band (bands on the Gaussian copula price) ==\n")
  combos |>
    mutate(band = cut(p_copula, c(-Inf, 0.01, 0.03, 0.1, 0.2, 0.4, 1))) |>
    group_by(band) |>
    ratio_table() |>
    print(n = Inf, width = Inf)

  cat("\n== Log loss improvement over independence (positive = better; z clustered by match) ==\n")
  ll <- map(methods, ~ log_loss_vec(combos[[.x]], combos$hit))
  combos |>
    mutate(!!!map(ll[-1], ~ ll$indep - .x)) |>
    group_by(type, n_legs) |>
    summarise(
      n = n(),
      across(all_of(names(ll)[-1]), ~ sprintf("%+.4f (z %+.1f)", mean(.x), clustered_z(.x, match_id))),
      .groups = "drop"
    ) |>
    print(n = Inf, width = Inf)

  cat("\n== Overall mean log loss (lower = better) ==\n")
  print(round(map_dbl(ll, mean), 5))
}
