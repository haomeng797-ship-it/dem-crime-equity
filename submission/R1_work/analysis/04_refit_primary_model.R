# Refit the primary R1 joint AR(1) model from the rebuilt raw-data merge. The
# fitted object is written only to the local, ignored R1 workspace.

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
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

data_path <- file.path(results_dir, "r1_rebuilt_analysis_data.csv")
if (!file.exists(data_path)) {
  stop("Run 00_rebuild_and_audit_data.R before fitting the model.")
}

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
    yearc = (year - 2011) / 10
  )

state_means <- d |>
  distinct(state_abbr, demM_raw)
d$demM <- (d$demM_raw - mean(state_means$demM_raw)) / sd(state_means$demM_raw)
d$demW <- d$demW_raw / sd(d$demW_raw)
d <- arrange(d, state_abbr, year)

formula_black <- bf(
  logB ~ demW + demM + yearc +
    (1 | p | state_abbr) + ar(time = year, gr = state_abbr)
)
formula_white <- bf(
  logW ~ demW + demM + yearc +
    (1 | p | state_abbr) + ar(time = year, gr = state_abbr)
)
priors <- c(
  prior(normal(0, 1), class = b, resp = "logB"),
  prior(normal(0, 1), class = b, resp = "logW")
)

fit_path <- file.path(models_dir, "primary_ar1_final.rds")
start_time <- Sys.time()

fit <- brm(
  formula_black + formula_white + set_rescor(TRUE),
  data = d,
  prior = priors,
  chains = 4,
  iter = 6000,
  warmup = 1500,
  seed = 20260908,
  cores = 4,
  refresh = 200,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

saveRDS(fit, fit_path)

elapsed_minutes <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
draws <- as_draws_df(fit)
nuts <- nuts_params(fit)
diagnostic_summary <- tibble(
  observations = nrow(fit$data),
  states = n_distinct(fit$data$state_abbr),
  posterior_draws = nrow(draws),
  max_rhat = max(rhat(fit), na.rm = TRUE),
  divergences = sum(nuts$Value[nuts$Parameter == "divergent__"]),
  treedepth_hits = sum(
    nuts$Value[nuts$Parameter == "treedepth__"] >= 12
  ),
  elapsed_minutes = elapsed_minutes
)

write.csv(
  diagnostic_summary,
  file.path(results_dir, "r1_primary_refit_diagnostics.csv"),
  row.names = FALSE
)
writeLines(
  capture.output(sessionInfo()),
  file.path(results_dir, "r1_refit_session_info.txt")
)

cat("\nFresh primary-model refit\n")
print(diagnostic_summary, width = Inf)
