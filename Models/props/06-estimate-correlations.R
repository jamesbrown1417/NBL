# Estimate joint-model correlations ------------------------------------------------
#
# Pooled residual correlations by relationship (same player, teammates by role,
# opponents), corrected for discreteness, with match-clustered bootstrap SEs.
# Fits two versions:
#   pre_test - seasons before 2025-26, for the out-of-sample SGM backtest
#   all      - every season, for live pricing
# Output: Models/props/output/joint/correlation_params.rds

source("Scripts/00-config.R")
source("Models/props/R/prop_model_functions.R")
source("Models/props/R/joint_functions.R")

test_season <- "2025-2026"
output_dir <- file.path(project_root, "Models", "props", "output", "joint")
residuals <- readRDS(file.path(output_dir, "pit.rds"))

set.seed(1)
params <- list(
  pre_test = estimate_joint_params(residuals$pit |> filter(season < test_season), residuals$attenuation, n_boot = 50L),
  all = estimate_joint_params(residuals$pit, residuals$attenuation, n_boot = 50L)
)
saveRDS(params, file.path(output_dir, "correlation_params.rds"))

show <- function(p, nm) {
  est <- p$cor[[nm]]
  se <- p$se[[nm]]
  out <- matrix(sprintf("%+.2f (%.2f)", est, se), nrow(est), dimnames = dimnames(est))
  out[lower.tri(out)] <- ""
  if (nm == "same") diag(out) <- ""
  cat("\n== ", nm, " (latent correlation, bootstrap SE) ==\n", sep = "")
  print(noquote(out))
}
cat("All seasons:", params$all$n_matches, "matches\n")
walk(names(params$all$cor), ~ show(params$all, .x))
