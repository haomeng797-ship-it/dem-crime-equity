# 14_hispanic_robustness.R -- can Hispanic misclassification explain the reversal?
# Some states record Hispanic prisoners as white, inflating their white rates; those states
# skew southern and less democratic, so the practice could mimic the denominator finding.
# Transparent flag: Latino share of the 15-64 population above 4 percent, yet a Latino prison
# rate that is missing or below 60 percent of the state's white rate (nationally the Latino
# rate exceeds the white rate, so values that low signal folding Hispanics into "white").
# Refit the AR(1) joint model of R/10 on the states that pass the screen.
suppressPackageStartupMessages({ library(brms); library(dplyr); library(posterior) })
proj <- Sys.getenv("DEM_CRIME_ROOT", unset = getwd())

v <- read.csv(file.path(proj, "data", "raw", "incarceration_trends_state.csv")) |>
  filter(year %in% 2016:2020) |> group_by(state_name) |>
  summarise(lat_share = mean(latinx_pop_15to64 / total_pop_15to64, na.rm = TRUE),
            lw = mean(latinx_prison_pop / latinx_pop_15to64, na.rm = TRUE) /
                 mean(white_prison_pop / white_pop_15to64, na.rm = TRUE), .groups = "drop")
suspect <- v$state_name[v$lat_share > 0.04 & (!is.finite(v$lw) | v$lw < 0.6)]
cat("suspect states (", length(suspect), "):", paste(sort(suspect), collapse = ", "), "\n\n")

d <- read.csv(file.path(proj, "data", "state_dem_incarceration.csv")) |>
  filter(!state_name %in% suspect,
         !is.na(black_prison_pop_rate), !is.na(white_prison_pop_rate),
         black_prison_pop_rate > 0, white_prison_pop_rate > 0, !is.na(democracy)) |>
  mutate(logB = log(black_prison_pop_rate), logW = log(white_prison_pop_rate)) |>
  group_by(state_abbr) |> mutate(demM_raw = mean(democracy)) |> ungroup() |>
  mutate(demW_raw = democracy - demM_raw, yearc = (year - 2011)/10)
sm <- d |> distinct(state_abbr, demM_raw)
d$demM <- (d$demM_raw - mean(sm$demM_raw))/sd(sm$demM_raw); d$demW <- d$demW_raw/sd(d$demW_raw)
d <- d |> arrange(state_abbr, year)
cat("states in subsample:", n_distinct(d$state_abbr), "| rows:", nrow(d), "\n")

bfB <- bf(logB ~ demW + demM + yearc + (1|p|state_abbr) + ar(time = year, gr = state_abbr))
bfW <- bf(logW ~ demW + demM + yearc + (1|p|state_abbr) + ar(time = year, gr = state_abbr))
pr  <- c(prior(normal(0,1), class=b, resp="logB"), prior(normal(0,1), class=b, resp="logW"))
fp <- file.path(proj, "bayes_fit_ar1_nohisp.rds")
if (file.exists(fp)) { fit <- readRDS(fp) } else {
  fit <- brm(bfB + bfW + set_rescor(TRUE), data = d, prior = pr, chains = 4, iter = 4000, warmup = 1000,
             seed = 20260620, cores = 4, refresh = 0,
             control = list(adapt_delta = 0.95, max_treedepth = 12))
  saveRDS(fit, fp)
}
cat("max Rhat:", round(max(rhat(fit), na.rm=TRUE),4),
    " | divergences:", sum(subset(nuts_params(fit), Parameter=='divergent__')$Value), "\n")
dr <- as_draws_df(fit)
Bb<-dr$b_logB_demM; Wb<-dr$b_logW_demM; rb<-Bb-Wb
q<-function(x)sprintf("%+.3f [%+.3f, %+.3f]",median(x),quantile(x,.025),quantile(x,.975))
cat("\n=== AR(1), BETWEEN-state per +1 SD democracy, misclassification states dropped ===\n")
cat("  ratio:",q(rb)," Black:",q(Bb)," White:",q(Wb),"\n")
cat("  P(ratio>0 & White<0) =", round(mean(rb>0 & Wb<0),3), "\n")
