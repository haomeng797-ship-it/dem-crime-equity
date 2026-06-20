# 01_load_merge.R
# Build the state-year analysis dataset: state democracy x racial incarceration disparity.
# Sources:
#   - State Democracy Index 2.0 (Grumbach & Bitton, UC Berkeley Democracy Policy Lab), 2000-2023.
#       democracy_mcmc = latent democratic-performance score (higher = more democratic).
#   - Vera Institute, Incarceration Trends (state file): race-specific prison rates per 100k (ages 15-64).
# Equity outcome: Black-to-White prison-rate ratio (bw_ratio); U.S. average historically ~5x.

suppressPackageStartupMessages({ library(dplyr) })

proj <- Sys.getenv("DEM_CRIME_ROOT", unset = getwd())  # project root: run from repo root, or set DEM_CRIME_ROOT
raw  <- file.path(proj, "data", "raw")

# --- State Democracy Index 2.0 ---
sdi <- read.csv(file.path(raw, "SDI_2.0.csv"), check.names = FALSE) |>
  transmute(state_abbr = st, state_name = state, year,
            democracy = democracy_mcmc, democracy_sd = democracy_mcmc_sd)

# --- Vera incarceration (state level) ---
vera <- read.csv(file.path(raw, "incarceration_trends_state.csv"), check.names = FALSE) |>
  select(state_abbr, region, division, year,
         total_prison_pop_rate, black_prison_pop_rate, white_prison_pop_rate) |>
  mutate(bw_ratio = black_prison_pop_rate / white_prison_pop_rate)

# --- merge on state x year (overlap 2000-2022) ---
df <- inner_join(sdi, vera, by = c("state_abbr", "year"))

cat("merged rows:", nrow(df), "\n")
cat("year range:", min(df$year), "-", max(df$year), "\n")
cat("states:", n_distinct(df$state_abbr), "\n")
cat("rows with finite bw_ratio:", sum(is.finite(df$bw_ratio)), "\n")
cat(sprintf("Black/White prison-rate ratio - median %.2f, IQR %.2f to %.2f\n",
            median(df$bw_ratio[is.finite(df$bw_ratio)]),
            quantile(df$bw_ratio[is.finite(df$bw_ratio)], .25),
            quantile(df$bw_ratio[is.finite(df$bw_ratio)], .75)))

saveRDS(df, file.path(proj, "data", "state_dem_incarceration.rds"))
write.csv(df, file.path(proj, "data", "state_dem_incarceration.csv"), row.names = FALSE)
cat("saved data/state_dem_incarceration.rds + .csv\n")
