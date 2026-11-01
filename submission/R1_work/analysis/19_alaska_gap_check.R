# Sensitivity check for the one gap in the panel: Alaska has no usable 2013
# observation, so the regression-form AR(1) term links its 2014 residual to
# 2012 as if one year had passed. This refit starts a fresh autoregressive
# chain for Alaska in 2014 (lagged residual zero) and compares the derived
# estimands with the primary model. Everything else matches
# 04_refit_primary_model.R.

suppressPackageStartupMessages({
  library(brms)
  library(dplyr)
  library(posterior)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE))
} else {
  getwd()
}
project_root <- normalizePath(file.path(script_dir, "..", "..", ".."), mustWork = TRUE)
work_root <- file.path(project_root, "submission", "R1_work")
results_dir <- file.path(work_root, "results")
models_dir <- file.path(work_root, "models")

primary <- readRDS(file.path(models_dir, "primary_ar1_final.rds"))
d <- primary$data |>
  mutate(ar_group = if_else(state_abbr == "AK" & year >= 2014, "AK_after_gap", state_abbr))
stopifnot(sum(d$ar_group == "AK_after_gap") == 9, !any(d$state_abbr == "AK" & d$year == 2013))

formula_gap <-
  bf(logB ~ demW + demM + yearc + (1 | p | state_abbr) + ar(time = year, gr = ar_group)) +
  bf(logW ~ demW + demM + yearc + (1 | p | state_abbr) + ar(time = year, gr = ar_group)) +
  set_rescor(TRUE)
priors <- c(
  prior(normal(0, 1), class = b, resp = "logB"),
  prior(normal(0, 1), class = b, resp = "logW")
)

fit_path <- file.path(models_dir, "sensitivity_alaska_gap_refit.rds")
if (file.exists(fit_path)) {
  fit <- readRDS(fit_path)
} else {
  fit <- brm(
    formula_gap,
    data = d,
    prior = priors,
    chains = 4,
    iter = 6000,
    warmup = 1500,
    seed = 20261103,
    cores = 4,
    refresh = 250,
    control = list(adapt_delta = 0.95, max_treedepth = 12)
  )
  saveRDS(fit, fit_path)
}

window <- filter(d, year >= 2016, year <= 2020)
ref_b <- mean(exp(window$logB))
ref_w <- mean(exp(window$logW))

estimands <- function(fit, label) {
  draws <- as_draws_df(fit)
  bind_rows(lapply(c("between", "within"), function(design) {
    term <- if (design == "between") "demM" else "demW"
    bb <- draws[[paste0("b_logB_", term)]]
    bw <- draws[[paste0("b_logW_", term)]]
    ratio <- 100 * (exp(bb - bw) - 1)
    gap <- ref_b * (exp(bb) - 1) - ref_w * (exp(bw) - 1)
    tibble(
      fit = label,
      design = design,
      quantity = c("ratio_pct", "gap_per_100k", "black_rate_pct", "white_rate_pct",
                   "ar_black", "ar_white", "p_ratio_up_gap_not_up", "p_ratio_up"),
      median = c(median(ratio), median(gap), median(100 * (exp(bb) - 1)), median(100 * (exp(bw) - 1)),
                 median(draws[["ar_logB[1]"]]), median(draws[["ar_logW[1]"]]),
                 mean(ratio > 0 & gap <= 0), mean(ratio > 0)),
      lower_95 = c(quantile(ratio, 0.025), quantile(gap, 0.025), quantile(100 * (exp(bb) - 1), 0.025),
                   quantile(100 * (exp(bw) - 1), 0.025), quantile(draws[["ar_logB[1]"]], 0.025),
                   quantile(draws[["ar_logW[1]"]], 0.025), NA, NA),
      upper_95 = c(quantile(ratio, 0.975), quantile(gap, 0.975), quantile(100 * (exp(bb) - 1), 0.975),
                   quantile(100 * (exp(bw) - 1), 0.975), quantile(draws[["ar_logB[1]"]], 0.975),
                   quantile(draws[["ar_logW[1]"]], 0.975), NA, NA)
    )
  }))
}

out <- bind_rows(
  estimands(primary, "Primary (Alaska 2014 follows 2012)"),
  estimands(fit, "Alaska chain restarted in 2014")
)
nuts <- nuts_params(fit)
write.csv(out, file.path(results_dir, "r1_alaska_gap_check.csv"), row.names = FALSE)
print(out, n = Inf, width = Inf)
print(tibble(
  max_rhat = max(rhat(fit), na.rm = TRUE),
  divergences = sum(nuts$Value[nuts$Parameter == "divergent__"]),
  treedepth_hits = sum(nuts$Value[nuts$Parameter == "treedepth__"] >= 12)
))
