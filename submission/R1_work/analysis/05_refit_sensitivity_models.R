# Refit the retained R1 sensitivity models from the locally rebuilt data. Each
# fit is cached only inside submission/R1_work/models.

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

prepare_rates <- function(data) {
  d <- data |>
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

  state_means <- distinct(d, state_abbr, demM_raw)
  d$demM <- (d$demM_raw - mean(state_means$demM_raw)) / sd(state_means$demM_raw)
  d$demW <- d$demW_raw / sd(d$demW_raw)
  arrange(d, state_abbr, year)
}

base_data <- read.csv(data_path)
d <- prepare_rates(base_data)

rate_priors <- c(
  prior(normal(0, 1), class = b, resp = "logB"),
  prior(normal(0, 1), class = b, resp = "logW")
)

fit_and_save <- function(path, formula, data, prior, seed, iter = 4000, warmup = 1000) {
  if (file.exists(path)) {
    message("Using completed local fit: ", basename(path))
    return(readRDS(path))
  }

  message("Fitting ", basename(path))
  fit <- brm(
    formula,
    data = data,
    prior = prior,
    chains = 4,
    iter = iter,
    warmup = warmup,
    seed = seed,
    cores = 4,
    refresh = 250,
    control = list(adapt_delta = 0.95, max_treedepth = 12)
  )
  saveRDS(fit, path)
  fit
}

# Sensitivity 1: remove residual AR(1) terms.
formula_no_ar <-
  bf(logB ~ demW + demM + yearc + (1 | p | state_abbr)) +
  bf(logW ~ demW + demM + yearc + (1 | p | state_abbr)) +
  set_rescor(TRUE)

fit_no_ar <- fit_and_save(
  file.path(models_dir, "sensitivity_no_ar_refit.rds"),
  formula_no_ar,
  d,
  rate_priors,
  seed = 20260904
)

# Sensitivity 2: retain AR(1) and add Census-region fixed effects.
formula_region_ar <-
  bf(
    logB ~ demW + demM + yearc + region +
      (1 | p | state_abbr) + ar(time = year, gr = state_abbr)
  ) +
  bf(
    logW ~ demW + demM + yearc + region +
      (1 | p | state_abbr) + ar(time = year, gr = state_abbr)
  ) +
  set_rescor(TRUE)

fit_region_ar <- fit_and_save(
  file.path(models_dir, "sensitivity_region_ar1_refit.rds"),
  formula_region_ar,
  d,
  rate_priors,
  seed = 20260905,
  iter = 6000,
  warmup = 1500
)

# Sensitivity 3: model race-specific counts with population offsets.
vera_counts <- read.csv(
  file.path(project_root, "data", "raw", "incarceration_trends_state.csv"),
  check.names = FALSE
) |>
  select(
    state_abbr,
    year,
    black_prison_pop,
    white_prison_pop,
    black_pop_15to64,
    white_pop_15to64
  )

d_count <- d |>
  left_join(vera_counts, by = c("state_abbr", "year")) |>
  filter(
    black_prison_pop > 0,
    white_prison_pop > 0,
    black_pop_15to64 > 0,
    white_pop_15to64 > 0
  ) |>
  mutate(
    Bc = as.integer(round(black_prison_pop)),
    Wc = as.integer(round(white_prison_pop)),
    offB = log(black_pop_15to64),
    offW = log(white_pop_15to64)
  )

formula_count <-
  bf(
    Bc ~ demW + demM + yearc + offset(offB) + (1 | p | state_abbr),
    family = negbinomial()
  ) +
  bf(
    Wc ~ demW + demM + yearc + offset(offW) + (1 | p | state_abbr),
    family = negbinomial()
  )

count_priors <- c(
  prior(normal(0, 1), class = b, resp = "Bc"),
  prior(normal(0, 1), class = b, resp = "Wc")
)

fit_count <- fit_and_save(
  file.path(models_dir, "sensitivity_count_refit.rds"),
  formula_count,
  d_count,
  count_priors,
  seed = 20260906
)

# Sensitivity 4: remove states flagged for likely Hispanic/White recording overlap.
vera_full <- read.csv(
  file.path(project_root, "data", "raw", "incarceration_trends_state.csv"),
  check.names = FALSE
)

recording_screen <- vera_full |>
  filter(year %in% 2016:2020) |>
  group_by(state_name) |>
  summarise(
    latinx_population_share = mean(latinx_pop_15to64 / total_pop_15to64, na.rm = TRUE),
    latinx_white_rate_ratio =
      mean(latinx_prison_pop / latinx_pop_15to64, na.rm = TRUE) /
      mean(white_prison_pop / white_pop_15to64, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    flagged = latinx_population_share > 0.04 &
      (!is.finite(latinx_white_rate_ratio) | latinx_white_rate_ratio < 0.6)
  )

write.csv(
  recording_screen,
  file.path(results_dir, "r1_recording_screen.csv"),
  row.names = FALSE
)

flagged_states <- recording_screen$state_name[recording_screen$flagged]
d_screen <- prepare_rates(filter(base_data, !state_name %in% flagged_states))

formula_screen <-
  bf(
    logB ~ demW + demM + yearc +
      (1 | p | state_abbr) + ar(time = year, gr = state_abbr)
  ) +
  bf(
    logW ~ demW + demM + yearc +
      (1 | p | state_abbr) + ar(time = year, gr = state_abbr)
  ) +
  set_rescor(TRUE)

fit_screen <- fit_and_save(
  file.path(models_dir, "sensitivity_recording_screen_refit.rds"),
  formula_screen,
  d_screen,
  rate_priors,
  seed = 20260907
)

cat("\nSensitivity refits complete.\n")
cat("Flagged states:", paste(sort(flagged_states), collapse = ", "), "\n")
