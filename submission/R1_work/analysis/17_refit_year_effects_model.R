# Refit the primary joint model with year fixed effects in place of the linear
# time trend (sensitivity model 5). The fit is cached only inside
# submission/R1_work/models, like the other refits.

suppressPackageStartupMessages({
  library(brms)
  library(dplyr)
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
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

data_path <- file.path(results_dir, "r1_rebuilt_analysis_data.csv")
if (!file.exists(data_path)) {
  stop("Run 00_rebuild_and_audit_data.R before fitting the models.")
}

# Same preparation as 04_refit_primary_model.R.
d <- read.csv(data_path) |>
  filter(
    !is.na(black_prison_pop_rate),
    !is.na(white_prison_pop_rate),
    black_prison_pop_rate > 0,
    white_prison_pop_rate > 0,
    !is.na(democracy)
  ) |>
  mutate(
    logB = log(black_prison_pop_rate),
    logW = log(white_prison_pop_rate)
  ) |>
  group_by(state_abbr) |>
  mutate(demM_raw = mean(democracy)) |>
  ungroup() |>
  mutate(
    demW_raw = democracy - demM_raw,
    yearc = (year - 2011) / 10,
    yearf = factor(year)
  )

state_means <- distinct(d, state_abbr, demM_raw)
d$demM <- (d$demM_raw - mean(state_means$demM_raw)) / sd(state_means$demM_raw)
d$demW <- d$demW_raw / sd(d$demW_raw)
d <- arrange(d, state_abbr, year)

# Year dummies replace the linear trend; everything else matches the primary model.
formula_year <-
  bf(
    logB ~ demW + demM + yearf +
      (1 | p | state_abbr) + ar(time = year, gr = state_abbr)
  ) +
  bf(
    logW ~ demW + demM + yearf +
      (1 | p | state_abbr) + ar(time = year, gr = state_abbr)
  ) +
  set_rescor(TRUE)

priors <- c(
  prior(normal(0, 1), class = b, resp = "logB"),
  prior(normal(0, 1), class = b, resp = "logW")
)

fit_path <- file.path(models_dir, "sensitivity_year_effects_refit.rds")
if (file.exists(fit_path)) {
  message("Using completed local fit: ", basename(fit_path))
  fit <- readRDS(fit_path)
} else {
  start_time <- Sys.time()
  fit <- brm(
    formula_year,
    data = d,
    prior = priors,
    chains = 4,
    iter = 6000,
    warmup = 1500,
    seed = 20261101,
    cores = 4,
    refresh = 250,
    control = list(adapt_delta = 0.95, max_treedepth = 12)
  )
  saveRDS(fit, fit_path)
  message(
    "Elapsed minutes: ",
    round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1)
  )
}

nuts <- nuts_params(fit)
cat("\nYear-effects refit\n")
print(tibble(
  observations = nrow(fit$data),
  states = n_distinct(fit$data$state_abbr),
  posterior_draws = ndraws(fit),
  max_rhat = max(rhat(fit), na.rm = TRUE),
  divergences = sum(nuts$Value[nuts$Parameter == "divergent__"]),
  treedepth_hits = sum(nuts$Value[nuts$Parameter == "treedepth__"] >= 12)
), width = Inf)
