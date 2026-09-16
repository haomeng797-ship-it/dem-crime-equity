# Audit the fitted models retained for R1. This reads the submitted cache files
# but writes all diagnostic records to the local, ignored R1 workspace.

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
results_dir <- file.path(project_root, "submission", "R1_work", "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

model_files <- tibble::tribble(
  ~model,                     ~file,
  "AR(1) log-rate",          "submission/R1_work/models/primary_ar1_final.rds",
  "Log-rate, no AR(1)",      "submission/R1_work/models/sensitivity_no_ar_refit.rds",
  "Region + AR(1)",          "submission/R1_work/models/sensitivity_region_ar1_refit.rds",
  "Negative-binomial count", "submission/R1_work/models/sensitivity_count_refit.rds",
  "Recording-screen sample", "submission/R1_work/models/sensitivity_recording_screen_refit.rds"
)

safe_min <- function(x) min(x[is.finite(x)], na.rm = TRUE)
safe_max <- function(x) max(x[is.finite(x)], na.rm = TRUE)

year_bounds <- function(data) {
  if ("year" %in% names(data)) {
    return(c(min(data$year), max(data$year)))
  }
  if ("yearc" %in% names(data)) {
    years <- 2011 + 10 * data$yearc
    return(round(c(min(years), max(years))))
  }
  c(NA_integer_, NA_integer_)
}

audit_one <- function(model, file) {
  path <- file.path(project_root, file)
  message("Auditing ", model)
  fit <- readRDS(path)
  draws <- as_draws_array(fit)
  diagnostics <- summarise_draws(draws, rhat, ess_bulk, ess_tail)
  nuts <- nuts_params(fit)
  max_treedepth <- fit$fit@stan_args[[1]]$control$max_treedepth
  if (is.null(max_treedepth)) max_treedepth <- 10
  years <- year_bounds(fit$data)

  energies <- nuts |>
    filter(Parameter == "energy__") |>
    arrange(Chain, Iteration) |>
    group_by(Chain) |>
    summarise(
      ebfmi = mean(diff(Value)^2) / var(Value),
      .groups = "drop"
    )

  summary_row <- tibble(
    model = model,
    source_file = file,
    observations = nrow(fit$data),
    states = if ("state_abbr" %in% names(fit$data)) n_distinct(fit$data$state_abbr) else NA_integer_,
    first_year = years[[1]],
    last_year = years[[2]],
    chains = fit$fit@sim$chains,
    post_warmup_draws = sum(fit$fit@sim$n_save - fit$fit@sim$warmup2),
    max_rhat = safe_max(diagnostics$rhat),
    min_bulk_ess = safe_min(diagnostics$ess_bulk),
    min_tail_ess = safe_min(diagnostics$ess_tail),
    divergences = sum(nuts$Value[nuts$Parameter == "divergent__"]),
    max_treedepth = max_treedepth,
    treedepth_hits = sum(
      nuts$Value[nuts$Parameter == "treedepth__"] >= max_treedepth
    ),
    min_ebfmi = min(energies$ebfmi),
    mean_accept_stat = mean(nuts$Value[nuts$Parameter == "accept_stat__"]),
    backend = fit$backend,
    brms_version = as.character(fit$version$brms),
    rstan_version = as.character(fit$version$rstan)
  )

  parameter_summary <- summarise_draws(draws, mean, sd, median, ~quantile(.x, 0.025), ~quantile(.x, 0.975), rhat, ess_bulk, ess_tail) |>
    rename(lower_95 = `2.5%`, upper_95 = `97.5%`) |>
    mutate(model = model, .before = 1)

  formula_text <- paste(capture.output(print(fit$formula)), collapse = "\n")
  family_text <- paste(capture.output(print(fit$family)), collapse = "\n")

  list(
    summary = summary_row,
    parameters = parameter_summary,
    formulas = tibble(model = model, formula = formula_text, family = family_text)
  )
}

audits <- lapply(seq_len(nrow(model_files)), function(i) {
  audit_one(model_files$model[[i]], model_files$file[[i]])
})

diagnostic_table <- bind_rows(lapply(audits, `[[`, "summary"))
parameter_table <- bind_rows(lapply(audits, `[[`, "parameters"))
formula_table <- bind_rows(lapply(audits, `[[`, "formulas"))

write.csv(diagnostic_table, file.path(results_dir, "r1_model_diagnostics.csv"), row.names = FALSE)
write.csv(parameter_table, file.path(results_dir, "r1_parameter_diagnostics.csv"), row.names = FALSE)
write.csv(formula_table, file.path(results_dir, "r1_model_formulas.csv"), row.names = FALSE)

cat("\nModel diagnostics\n")
print(diagnostic_table, n = Inf, width = Inf)
