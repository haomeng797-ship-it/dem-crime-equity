# 09_bayes_robustness.R -- two robustness checks for the joint model in R/08, the two things a
# careful reader would push on:
#   (1) add region as its own level (states nested in regions), in case region is doing the work;
#   (2) replace the Gaussian-on-log-rate likelihood with a negative-binomial count model on the
#       raw imprisonment counts, with log population (ages 15-64) as an offset.
# Both should reproduce the main result if it is real: a positive between-state ratio effect driven
# by the falling white rate, and P(ratio > 0 AND White < 0) near 1. Cached fits, like R/08.

suppressPackageStartupMessages({ library(brms); library(dplyr); library(posterior) })
proj <- Sys.getenv("DEM_CRIME_ROOT", unset = getwd())

m <- read.csv(file.path(proj, "data", "state_dem_incarceration.csv"))
v <- read.csv(file.path(proj, "data", "raw", "incarceration_trends_state.csv"))[
  , c("state_abbr", "year", "black_prison_pop", "white_prison_pop",
      "black_pop_15to64", "white_pop_15to64")]

d <- m |> left_join(v, by = c("state_abbr", "year")) |>
  filter(!is.na(democracy), !is.na(region),
         black_prison_pop_rate > 0, white_prison_pop_rate > 0,
         black_prison_pop > 0, white_prison_pop > 0,
         black_pop_15to64 > 0, white_pop_15to64 > 0) |>
  mutate(logB = log(black_prison_pop_rate), logW = log(white_prison_pop_rate),
         Bc = as.integer(round(black_prison_pop)), Wc = as.integer(round(white_prison_pop)),
         offB = log(black_pop_15to64), offW = log(white_pop_15to64)) |>
  group_by(state_abbr) |> mutate(demM_raw = mean(democracy)) |> ungroup() |>
  mutate(demW_raw = democracy - demM_raw, yearc = (year - 2011) / 10)
sm <- d |> distinct(state_abbr, demM_raw)
d$demM <- (d$demM_raw - mean(sm$demM_raw)) / sd(sm$demM_raw)
d$demW <- d$demW_raw / sd(d$demW_raw)
cat("rows:", nrow(d), " states:", length(unique(d$state_abbr)),
    " regions:", length(unique(d$region)), "\n")

ci <- function(x) sprintf("%+.3f [%+.3f, %+.3f]", median(x), quantile(x, .025), quantile(x, .975))
report <- function(rb, bb, wb, label) {
  cat("\n[", label, "]  between-state, per +1 SD democracy\n")
  cat("   ratio:", ci(rb), "  Black:", ci(bb), "  White:", ci(wb), "\n")
  cat("   P(ratio>0 & White<0) =", round(mean(rb > 0 & wb < 0), 3),
      "   P(ratio>0 & Black<0) =", round(mean(rb > 0 & bb < 0), 3), "\n")
}

# --- MAIN (cached from R/08) for side-by-side ---
mf <- file.path(proj, "bayes_fit.rds")
if (file.exists(mf)) { dr <- as_draws_df(readRDS(mf))
  report(dr$b_logB_demM - dr$b_logW_demM, dr$b_logB_demM, dr$b_logW_demM,
         "MAIN  Gaussian on log-rate (R/08)") }

# --- Robustness 1: adjust for region as a FIXED effect.
# Only 4 census regions, too few to estimate a random-effect variance (a random level here gives a
# funnel: divergences, low ESS). With <5 groups the standard move is fixed effects (Gelman & Hill).
# This asks the cleaner question: within regions, does the between-state pattern survive?
fp1 <- file.path(proj, "bayes_fit_regionfe.rds")
if (file.exists(fp1)) fit1 <- readRDS(fp1) else {
  fit1 <- brm(bf(logB ~ demW + demM + yearc + region + (1 | p | state_abbr)) +
              bf(logW ~ demW + demM + yearc + region + (1 | p | state_abbr)) +
              set_rescor(TRUE), data = d,
              prior = c(prior(normal(0, 1), class = b, resp = "logB"),
                        prior(normal(0, 1), class = b, resp = "logW")),
              chains = 4, iter = 2000, warmup = 1000, seed = 20260620,
              cores = 4, refresh = 0, control = list(adapt_delta = 0.95))
  saveRDS(fit1, fp1) }
d1 <- as_draws_df(fit1)
report(d1$b_logB_demM - d1$b_logW_demM, d1$b_logB_demM, d1$b_logW_demM,
       "ROBUSTNESS 1  + region fixed effect")
cat("   max Rhat:", round(max(rhat(fit1), na.rm = TRUE), 3), "\n")

# --- Robustness 2: negative-binomial count model with population offset ---
fp2 <- file.path(proj, "bayes_fit_count.rds")
if (file.exists(fp2)) fit2 <- readRDS(fp2) else {
  fit2 <- brm(bf(Bc ~ demW + demM + yearc + offset(offB) + (1 | p | state_abbr), family = negbinomial()) +
              bf(Wc ~ demW + demM + yearc + offset(offW) + (1 | p | state_abbr), family = negbinomial()),
              data = d, chains = 4, iter = 2000, warmup = 1000, seed = 20260620,
              cores = 4, refresh = 0, control = list(adapt_delta = 0.95))
  saveRDS(fit2, fp2) }
d2 <- as_draws_df(fit2)
report(d2$b_Bc_demM - d2$b_Wc_demM, d2$b_Bc_demM, d2$b_Wc_demM,
       "ROBUSTNESS 2  negbin count model + log-population offset")
cat("   max Rhat:", round(max(rhat(fit2), na.rm = TRUE), 3), "\n")
cat("\n===== ROBUSTNESS DONE =====\n")
