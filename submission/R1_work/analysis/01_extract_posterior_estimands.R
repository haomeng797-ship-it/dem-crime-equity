# Derive R1 disparity and burden estimands from the existing joint-model fits.

suppressPackageStartupMessages({
  library(brms)
  library(dplyr)
  library(posterior)
  library(tidyr)
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
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

window_start <- 2016
window_end <- 2020
low_value <- -0.5
high_value <- 0.5

model_specs <- tibble::tribble(
  ~label,                    ~file,                         ~kind,       ~resp_b, ~resp_w,
  "AR(1) log-rate",         "submission/R1_work/models/primary_ar1_final.rds", "gaussian", "logB",  "logW",
  "Log-rate, no AR(1)",     "submission/R1_work/models/sensitivity_no_ar_refit.rds", "gaussian", "logB", "logW",
  "Region + AR(1)",         "submission/R1_work/models/sensitivity_region_ar1_refit.rds", "gaussian", "logB", "logW",
  "Negative-binomial count", "submission/R1_work/models/sensitivity_count_refit.rds", "count", "Bc", "Wc",
  "Recording-screen sample", "submission/R1_work/models/sensitivity_recording_screen_refit.rds", "gaussian", "logB", "logW"
)

draw_name <- function(response, term) paste0("b_", response, "_", term)
sd_name <- function(response) paste0("sd_state_abbr__", response, "_Intercept")
sigma_name <- function(response) paste0("sigma_", response)

window_rows <- function(data) {
  if ("year" %in% names(data)) {
    data$year >= window_start & data$year <= window_end
  } else {
    lower <- (window_start - 2011) / 10
    upper <- (window_end - 2011) / 10
    data$yearc >= lower - 1e-8 & data$yearc <= upper + 1e-8
  }
}

linear_rate <- function(fit, newdata, response, draws, kind) {
  mu <- posterior_linpred(
    fit,
    newdata = newdata,
    resp = response,
    re_formula = NA,
    transform = FALSE,
    incl_autocor = FALSE
  )

  adjustment <- 0.5 * draws[[sd_name(response)]]^2
  if (kind == "gaussian") {
    adjustment <- adjustment + 0.5 * draws[[sigma_name(response)]]^2
  }

  exp(sweep(mu, 1, adjustment, "+"))
}

standardized_rates <- function(fit, draws, spec, design, value) {
  data <- fit$data[window_rows(fit$data), , drop = FALSE]
  data$demM <- if (design == "between") value else 0
  data$demW <- if (design == "within") value else 0

  if (spec$kind == "count") {
    data$offB <- log(100000)
    data$offW <- log(100000)
  }

  b <- linear_rate(fit, data, spec$resp_b, draws, spec$kind)
  w <- linear_rate(fit, data, spec$resp_w, draws, spec$kind)

  tibble(
    black_rate = rowMeans(b),
    white_rate = rowMeans(w),
    gap = black_rate - white_rate
  )
}

reference_rates <- function(fit, spec) {
  data <- fit$data[window_rows(fit$data), , drop = FALSE]

  if (spec$kind == "count") {
    black_rate <- data[[spec$resp_b]] / exp(data$offB) * 100000
    white_rate <- data[[spec$resp_w]] / exp(data$offW) * 100000
  } else {
    black_rate <- exp(data[[spec$resp_b]])
    white_rate <- exp(data[[spec$resp_w]])
  }

  c(
    black_rate = mean(black_rate),
    white_rate = mean(white_rate)
  )
}

extract_model <- function(spec) {
  fit_path <- file.path(project_root, spec$file)
  message("Reading ", spec$label, " from ", fit_path)
  fit <- readRDS(fit_path)
  draws <- as_draws_df(fit)
  reference <- reference_rates(fit, spec)

  bind_rows(lapply(c("between", "within"), function(design) {
    term <- if (design == "between") "demM" else "demW"
    beta_b <- draws[[draw_name(spec$resp_b, term)]]
    beta_w <- draws[[draw_name(spec$resp_w, term)]]

    low <- standardized_rates(fit, draws, spec, design, low_value)
    high <- standardized_rates(fit, draws, spec, design, high_value)

    tibble(
      .draw = seq_len(nrow(draws)),
      model = spec$label,
      design = design,
      ratio_pct = 100 * (exp(beta_b - beta_w) - 1),
      gap_per_100k =
        reference[["black_rate"]] * (exp(beta_b) - 1) -
        reference[["white_rate"]] * (exp(beta_w) - 1),
      black_rate_pct = 100 * (exp(beta_b) - 1),
      white_rate_pct = 100 * (exp(beta_w) - 1),
      black_rate_low = low$black_rate,
      black_rate_high = high$black_rate,
      white_rate_low = low$white_rate,
      white_rate_high = high$white_rate,
      gap_low = low$gap,
      gap_high = high$gap
    )
  }))
}

posterior_draws <- bind_rows(lapply(seq_len(nrow(model_specs)), function(i) {
  extract_model(model_specs[i, ])
}))

long_draws <- posterior_draws |>
  select(.draw, model, design, ratio_pct, gap_per_100k, black_rate_pct, white_rate_pct) |>
  pivot_longer(
    cols = c(ratio_pct, gap_per_100k, black_rate_pct, white_rate_pct),
    names_to = "estimand",
    values_to = "estimate"
  )

summary_table <- long_draws |>
  group_by(model, design, estimand) |>
  summarise(
    median = median(estimate),
    lower_95 = quantile(estimate, 0.025),
    upper_95 = quantile(estimate, 0.975),
    probability_positive = mean(estimate > 0),
    probability_negative = mean(estimate < 0),
    .groups = "drop"
  )

joint_table <- posterior_draws |>
  group_by(model, design) |>
  summarise(
    probability_ratio_positive = mean(ratio_pct > 0),
    probability_gap_positive = mean(gap_per_100k > 0),
    probability_ratio_positive_gap_nonpositive = mean(ratio_pct > 0 & gap_per_100k <= 0),
    probability_same_direction = mean(sign(ratio_pct) == sign(gap_per_100k)),
    posterior_correlation = cor(ratio_pct, gap_per_100k),
    .groups = "drop"
  )

saveRDS(posterior_draws, file.path(results_dir, "r1_posterior_draws.rds"))
write.csv(summary_table, file.path(results_dir, "r1_posterior_summary.csv"), row.names = FALSE)
write.csv(joint_table, file.path(results_dir, "r1_joint_probabilities.csv"), row.names = FALSE)

cat("\nPrimary AR(1), between-state summary\n")
print(summary_table |> filter(model == "AR(1) log-rate", design == "between"), n = Inf)
cat("\nPrimary AR(1), between-state joint probabilities\n")
print(joint_table |> filter(model == "AR(1) log-rate", design == "between"), n = Inf)
cat("\nSaved results to ", results_dir, "\n", sep = "")
