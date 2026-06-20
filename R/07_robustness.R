# 07_robustness.R
# Robustness checks for the Florida Amendment 4 synthetic control (see 06_causal_amendment4.R).
# Three checks, all reinforcing the "robust null" reading:
#   (1) extend the post-treatment window from 2020 to include the 2022 midterm;
#   (2) in-time placebo: reassign treatment to 2016 (before the reform) and check the
#       genuinely pre-treatment gaps are near zero;
#   (3) leave-one-out: drop each weighted donor in turn and re-fit, to show no single
#       donor drives the estimate.
# Outcome: VAP turnout rate. Donor pool excludes states that also changed felon-voting
# laws in 2016-2022 (SUTVA). Run after 06; uses the same data and specification.

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(tidysynth) })

proj <- Sys.getenv("DEM_CRIME_ROOT", unset = getwd())  # project root: run from repo root, or set DEM_CRIME_ROOT
raw  <- read.csv(file.path(proj, "data", "raw", "ufl_turnout_1980_2022.csv"),
                 check.names = FALSE, stringsAsFactors = FALSE)
num     <- function(x) as.numeric(gsub("[,%]", "", trimws(x)))
firstok <- function(x){ x <- x[is.finite(x)]; if (length(x)) x[1] else NA_real_ }

d <- raw |>
  mutate(state = trimws(gsub("[*]", "", STATE)), year = as.integer(YEAR),
         vap = num(VAP), vap_turnout = num(VAP_TURNOUT_RATE),
         felons = num(INELIGIBLE_FELONS_TOTAL)) |>
  filter(year >= 2000, year %% 2 == 0,
         !state %in% c("United States", "District of Columbia")) |>
  group_by(state, year) |>
  summarise(vap = firstok(vap), vap_turnout = firstok(vap_turnout),
            felons = firstok(felons), .groups = "drop")

yrs  <- sort(unique(d$year))
comp <- d |> group_by(state) |> summarise(ok = sum(is.finite(vap_turnout)), .groups = "drop")
balanced  <- comp$state[comp$ok == length(yrs)]
reformers <- c("Nevada","New Jersey","Colorado","New York","Washington","Virginia",
               "Kentucky","Iowa","California","Connecticut","Louisiana","New Mexico")
donors <- setdiff(balanced, reformers)
panel  <- d |> filter(state %in% union("Florida", donors))
cat("years:", paste(range(yrs), collapse = "-"), "| Florida +", length(donors), "clean donors\n\n")

# baseline specification (matches 06): treatment first hits 2020
base_sc <- function(pnl, placebos = TRUE){
  pnl |>
    synthetic_control(outcome = vap_turnout, unit = state, time = year,
                      i_unit = "Florida", i_time = 2020, generate_placebos = placebos) |>
    generate_predictor(time_window = 2000:2018, turnout_pre = mean(vap_turnout, na.rm = TRUE)) |>
    generate_predictor(time_window = 2016, turnout_2016 = vap_turnout) |>
    generate_predictor(time_window = 2012, turnout_2012 = vap_turnout) |>
    generate_predictor(time_window = 2008, turnout_2008 = vap_turnout) |>
    generate_weights(optimization_window = 2000:2018) |>
    generate_control()
}

# in-time placebo: pretend the reform happened in 2016, fit on pre-2016 only
intime_sc <- function(pnl){
  pnl |>
    synthetic_control(outcome = vap_turnout, unit = state, time = year,
                      i_unit = "Florida", i_time = 2016, generate_placebos = FALSE) |>
    generate_predictor(time_window = 2000:2014, turnout_pre = mean(vap_turnout, na.rm = TRUE)) |>
    generate_predictor(time_window = 2014, turnout_2014 = vap_turnout) |>
    generate_predictor(time_window = 2010, turnout_2010 = vap_turnout) |>
    generate_predictor(time_window = 2008, turnout_2008 = vap_turnout) |>
    generate_weights(optimization_window = 2000:2014) |>
    generate_control()
}

gapsof <- function(sc) sc |> grab_synthetic_control() |> mutate(gap = real_y - synth_y)

# ---- Check 1: base model + extended post-period (2020 and 2022) ----
cat("========== CHECK 1: base model, post-period 2020 AND 2022 ==========\n")
base <- base_sc(panel, placebos = TRUE)
gb <- gapsof(base)
print(gb |> filter(time_unit >= 2014) |>
        transmute(year = time_unit, real = round(real_y, 1),
                  synth = round(synth_y, 1), gap = round(gap, 2)), row.names = FALSE)
sig <- base |> grab_significance() |> filter(unit_name == "Florida")
cat("\npre-period RMSPE:", round(sqrt(sig$pre_mspe), 3),
    "| placebo rank:", sig$rank, "of", nrow(base |> grab_significance()),
    "| Fisher p:", round(sig$fishers_exact_pvalue, 3),
    "| mspe_ratio:", round(sig$mspe_ratio, 2), "\n\n")

# ---- Check 2: in-time placebo (fake treatment year = 2016) ----
cat("========== CHECK 2: in-time placebo, fake treatment year = 2016 ==========\n")
it  <- intime_sc(panel)
git <- gapsof(it)
print(git |> filter(time_unit >= 2012) |>
        transmute(year = time_unit, real = round(real_y, 1),
                  synth = round(synth_y, 1), gap = round(gap, 2)), row.names = FALSE)
cat("\nin-time pre-2016 RMSPE:",
    round(sqrt(mean((git$gap[git$time_unit <= 2014])^2)), 3), "\n\n")

# ---- Check 3: leave-one-out donors ----
cat("========== CHECK 3: leave-one-out donors ==========\n")
wts <- base |> grab_unit_weights() |> arrange(desc(weight))
print(head(wts, 8), row.names = FALSE)
topd <- wts$unit[wts$weight > 0.01]
loo <- do.call(rbind, lapply(topd, function(drop){
  tryCatch({
    m  <- base_sc(panel |> filter(state != drop), placebos = FALSE)
    gg <- gapsof(m)
    data.frame(dropped = drop,
               gap2020 = round(gg$gap[gg$time_unit == 2020], 2),
               gap2022 = round(gg$gap[gg$time_unit == 2022], 2))
  }, error = function(e) data.frame(dropped = drop, gap2020 = NA, gap2022 = NA))
}))
print(loo, row.names = FALSE)
cat("\nbaseline gap -> 2020:", round(gb$gap[gb$time_unit == 2020], 2),
    " 2022:", round(gb$gap[gb$time_unit == 2022], 2), "\n")
cat("leave-one-out 2020 range: [", round(min(loo$gap2020, na.rm = TRUE), 2), ",",
    round(max(loo$gap2020, na.rm = TRUE), 2), "]\n")
cat("leave-one-out 2022 range: [", round(min(loo$gap2022, na.rm = TRUE), 2), ",",
    round(max(loo$gap2022, na.rm = TRUE), 2), "]\n")
