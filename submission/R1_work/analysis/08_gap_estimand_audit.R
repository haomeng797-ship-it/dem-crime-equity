# Audit alternative translations of the log-rate model into an absolute rate gap.

suppressPackageStartupMessages({
  library(brms)
  library(dplyr)
  library(posterior)
  library(tibble)
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

fit <- readRDS(file.path(work_root, "models", "primary_ar1_final.rds"))
draws <- as_draws_df(fit)
window <- fit$data |>
  filter(year >= 2016, year <= 2020)

beta_b <- draws$b_logB_demM
beta_w <- draws$b_logW_demM

summarise_gap <- function(x, method) {
  tibble(
    method = method,
    median = median(x),
    lower_95 = quantile(x, 0.025),
    upper_95 = quantile(x, 0.975),
    probability_positive = mean(x > 0)
  )
}

# Translation 1: apply the posterior proportional changes to the observed
# 2016-2020 rate levels. This keeps the absolute scale anchored to the data.
observed_gap_change <-
  mean(exp(window$logB)) * (exp(beta_b) - 1) -
  mean(exp(window$logW)) * (exp(beta_w) - 1)

counterfactual_data <- function(value) {
  transform(window, demM = value, demW = 0)
}

fixed_rate <- function(response, value) {
  exp(posterior_linpred(
    fit,
    newdata = counterfactual_data(value),
    resp = response,
    re_formula = NA,
    transform = FALSE,
    incl_autocor = FALSE
  )) |>
    rowMeans()
}

sample_rate <- function(response, value) {
  exp(posterior_linpred(
    fit,
    newdata = counterfactual_data(value),
    resp = response,
    re_formula = NULL,
    transform = FALSE,
    incl_autocor = FALSE
  )) |>
    rowMeans()
}

# Translation 2: compare fixed-effect predictions at -0.5 and +0.5 SD.
fixed_gap_change <-
  (fixed_rate("logB", 0.5) - fixed_rate("logW", 0.5)) -
  (fixed_rate("logB", -0.5) - fixed_rate("logW", -0.5))

# Translation 3: retain the estimated intercept for each observed state while
# changing the between-state democracy term for the full 2016-2020 sample.
sample_gap_change <-
  (sample_rate("logB", 0.5) - sample_rate("logW", 0.5)) -
  (sample_rate("logB", -0.5) - sample_rate("logW", -0.5))

audit <- bind_rows(
  summarise_gap(observed_gap_change, "Observed-rate standardization"),
  summarise_gap(fixed_gap_change, "Fixed-effect standardization"),
  summarise_gap(sample_gap_change, "Observed-state standardization")
)

write.csv(
  audit,
  file.path(results_dir, "r1_gap_estimand_audit.csv"),
  row.names = FALSE
)

cat("Absolute-gap estimand audit\n")
print(audit, width = Inf)
