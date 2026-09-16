# Rebuild the submitted analysis data from the two raw sources without touching
# any tracked repository output, then document the analysis sample used in R1.

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
work_root <- file.path(project_root, "submission", "R1_work")
results_dir <- file.path(work_root, "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

raw_dir <- file.path(project_root, "data", "raw")

sdi <- read.csv(file.path(raw_dir, "SDI_2.0.csv"), check.names = FALSE) |>
  transmute(
    state_abbr = st,
    state_name = state,
    year,
    democracy = democracy_mcmc,
    democracy_sd = democracy_mcmc_sd
  )

vera <- read.csv(
  file.path(raw_dir, "incarceration_trends_state.csv"),
  check.names = FALSE
) |>
  select(
    state_abbr,
    region,
    division,
    year,
    total_prison_pop_rate,
    black_prison_pop_rate,
    white_prison_pop_rate
  ) |>
  mutate(bw_ratio = black_prison_pop_rate / white_prison_pop_rate)

rebuilt <- inner_join(sdi, vera, by = c("state_abbr", "year"))
submitted <- read.csv(file.path(project_root, "data", "state_dem_incarceration.csv"))

comparison <- all.equal(
  rebuilt[order(rebuilt$state_abbr, rebuilt$year), ],
  submitted[order(submitted$state_abbr, submitted$year), ],
  check.attributes = FALSE,
  tolerance = 1e-12
)

analysis_sample <- rebuilt |>
  filter(
    !is.na(democracy),
    is.finite(black_prison_pop_rate),
    is.finite(white_prison_pop_rate),
    black_prison_pop_rate > 0,
    white_prison_pop_rate > 0
  )

duplicate_keys <- rebuilt |>
  count(state_abbr, year) |>
  filter(n != 1)

audit <- tibble(
  check = c(
    "Rebuilt data match submitted CSV",
    "Merged rows",
    "Merged states",
    "Merged year range",
    "Duplicate state-year keys",
    "Rows missing either race-specific rate",
    "Rows with a zero race-specific rate",
    "Primary model rows",
    "Primary model states",
    "Primary model year range"
  ),
  result = c(
    if (isTRUE(comparison)) "yes (exact within 1e-12)" else paste(comparison, collapse = "; "),
    as.character(nrow(rebuilt)),
    as.character(n_distinct(rebuilt$state_abbr)),
    paste0(min(rebuilt$year), "-", max(rebuilt$year)),
    as.character(nrow(duplicate_keys)),
    as.character(sum(is.na(rebuilt$black_prison_pop_rate) | is.na(rebuilt$white_prison_pop_rate))),
    as.character(sum(
      rebuilt$black_prison_pop_rate == 0 | rebuilt$white_prison_pop_rate == 0,
      na.rm = TRUE
    )),
    as.character(nrow(analysis_sample)),
    as.character(n_distinct(analysis_sample$state_abbr)),
    paste0(min(analysis_sample$year), "-", max(analysis_sample$year))
  )
)

write.csv(rebuilt, file.path(results_dir, "r1_rebuilt_analysis_data.csv"), row.names = FALSE)
saveRDS(rebuilt, file.path(results_dir, "r1_rebuilt_analysis_data.rds"))
write.csv(audit, file.path(results_dir, "r1_data_audit.csv"), row.names = FALSE)

cat("Data audit\n")
print(audit, n = Inf)

if (!isTRUE(comparison)) {
  stop("The locally rebuilt dataset does not match the submitted analysis CSV.")
}
