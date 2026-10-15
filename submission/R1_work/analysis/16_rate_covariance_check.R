# Show how much the posterior covariance of the two rate coefficients matters
# for the ratio estimand. Reads saved posterior draws only; no model is refit.

suppressPackageStartupMessages({
  library(dplyr)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE))
} else {
  getwd()
}
project_root <- normalizePath(file.path(script_dir, "..", "..", ".."), mustWork = TRUE)
results_dir <- file.path(project_root, "submission", "R1_work", "results")

draws <- readRDS(file.path(results_dir, "r1_posterior_draws.rds")) |>
  filter(model == "AR(1) log-rate", design == "between")
stopifnot(nrow(draws) == 18000L)

# The burden estimands are 100 * (exp(beta) - 1), so log1p recovers each beta.
beta_black <- log1p(draws$black_rate_pct / 100)
beta_white <- log1p(draws$white_rate_pct / 100)
to_pct <- function(x) 100 * (exp(x) - 1)

paired <- beta_black - beta_white
stopifnot(max(abs(to_pct(paired) - draws$ratio_pct)) < 1e-8)

# Pairing the draws at random keeps each coefficient's marginal posterior but
# discards the correlation between them.
set.seed(20261006)
unpaired <- beta_black - sample(beta_white)

summarise_ratio <- function(x, pairing) {
  tibble(
    pairing = pairing,
    median_pct = median(to_pct(x)),
    lower_95_pct = quantile(to_pct(x), 0.025, names = FALSE),
    upper_95_pct = quantile(to_pct(x), 0.975, names = FALSE),
    probability_positive = mean(x > 0)
  )
}

check <- bind_rows(
  summarise_ratio(paired, "Matched posterior draws"),
  summarise_ratio(unpaired, "Random pairing")
) |>
  mutate(coefficient_correlation = cor(beta_black, beta_white))

write.csv(check, file.path(results_dir, "r1_rate_covariance_check.csv"), row.names = FALSE)
print(check, width = Inf)
