# Stationarity check for the year-effects sensitivity model: refit it with the
# AR(1) coefficients constrained to (-1, 1) and compare the derived estimands
# with the unconstrained fit from 17_refit_year_effects_model.R.

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

unconstrained <- readRDS(file.path(models_dir, "sensitivity_year_effects_refit.rds"))
d <- unconstrained$data

priors <- c(
  prior(normal(0, 1), class = b, resp = "logB"),
  prior(normal(0, 1), class = b, resp = "logW"),
  prior(uniform(-1, 1), class = ar, resp = "logB", lb = -1, ub = 1),
  prior(uniform(-1, 1), class = ar, resp = "logW", lb = -1, ub = 1)
)

fit_path <- file.path(models_dir, "sensitivity_year_effects_stationary_refit.rds")
if (file.exists(fit_path)) {
  constrained <- readRDS(fit_path)
} else {
  constrained <- brm(
    formula(unconstrained),
    data = d,
    prior = priors,
    chains = 4,
    iter = 6000,
    warmup = 1500,
    seed = 20261102,
    cores = 4,
    refresh = 250,
    control = list(adapt_delta = 0.95, max_treedepth = 12)
  )
  saveRDS(constrained, fit_path)
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
    tibble(
      fit = label,
      design = design,
      ratio_pct = 100 * (exp(bb - bw) - 1),
      gap_per_100k = ref_b * (exp(bb) - 1) - ref_w * (exp(bw) - 1),
      black_rate_pct = 100 * (exp(bb) - 1),
      white_rate_pct = 100 * (exp(bw) - 1),
      ar_black = draws[["ar_logB[1]"]],
      ar_white = draws[["ar_logW[1]"]]
    )
  }))
}

summary_table <- bind_rows(
  estimands(unconstrained, "Year effects, flat AR prior"),
  estimands(constrained, "Year effects, AR constrained to (-1, 1)")
) |>
  tidyr::pivot_longer(c(ratio_pct, gap_per_100k, black_rate_pct, white_rate_pct, ar_black, ar_white),
                      names_to = "quantity", values_to = "value") |>
  group_by(fit, design, quantity) |>
  summarise(
    median = median(value),
    lower_95 = quantile(value, 0.025),
    upper_95 = quantile(value, 0.975),
    probability_positive = mean(value > 0),
    .groups = "drop"
  )

nuts <- nuts_params(constrained)
diagnostics <- tibble(
  max_rhat = max(rhat(constrained), na.rm = TRUE),
  divergences = sum(nuts$Value[nuts$Parameter == "divergent__"]),
  treedepth_hits = sum(nuts$Value[nuts$Parameter == "treedepth__"] >= 12)
)

write.csv(summary_table, file.path(results_dir, "r1_year_effects_stationarity_check.csv"), row.names = FALSE)
print(summary_table, n = Inf, width = Inf)
print(diagnostics)
