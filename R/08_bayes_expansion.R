# 08_bayes_expansion.R -- Bayesian continuous-model-expansion of the Demonstration 1 & 2 multiverse.
#
# Instead of treating the four "measures" of racial inequality as four separate analyses (a discrete
# multiverse), we fit ONE joint multilevel model of log Black and log White imprisonment on democracy,
# split Mundlak-style into within- and between-state parts. The four measures are then read off as
# posterior contrasts of a single posterior: the Black/White ratio effect is just (log-Black effect -
# log-White effect). The sign "flip" across measures becomes the geometry of one joint effect, and the
# thesis -- the measure makes the finding -- can be stated as a single posterior probability.
#
# This is the principled back-end (Gelman's continuous model expansion) to the paper's front-end
# multiverse. Sourced style/look from R/00_theme.R; figure saved via save_fig() like 02-06.

suppressPackageStartupMessages({
  library(brms); library(dplyr); library(posterior); library(ggplot2); library(tidyr); library(ggridges)
})
proj <- Sys.getenv("DEM_CRIME_ROOT", unset = getwd())  # project root: run from repo root, or set DEM_CRIME_ROOT
source(file.path(proj, "R", "00_theme.R"))

# ---- data: log rates + Mundlak within/between split of democracy -----------------------------------
d <- read.csv(file.path(proj, "data", "state_dem_incarceration.csv")) |>
  filter(!is.na(black_prison_pop_rate), !is.na(white_prison_pop_rate),
         black_prison_pop_rate > 0, white_prison_pop_rate > 0, !is.na(democracy)) |>
  mutate(logB = log(black_prison_pop_rate), logW = log(white_prison_pop_rate)) |>
  group_by(state_abbr) |> mutate(demM_raw = mean(democracy)) |> ungroup() |>
  mutate(demW_raw = democracy - demM_raw, yearc = (year - 2011) / 10)
sm <- d |> distinct(state_abbr, demM_raw)
d$demM <- (d$demM_raw - mean(sm$demM_raw)) / sd(sm$demM_raw)   # between-state, per 1 SD across states
d$demW <- d$demW_raw / sd(d$demW_raw)                          # within-state,  per 1 SD over time

# ---- one joint multilevel model of (logB, logW) ----------------------------------------------------
# cached: refit only if bayes_fit.rds is absent (MCMC ~1-2 min)
fit_path <- file.path(proj, "bayes_fit.rds")
if (file.exists(fit_path)) {
  fit <- readRDS(fit_path)
} else {
  bfB <- bf(logB ~ demW + demM + yearc + (1 | p | state_abbr))
  bfW <- bf(logW ~ demW + demM + yearc + (1 | p | state_abbr))
  pr  <- c(prior(normal(0, 1), class = b, resp = "logB"),
           prior(normal(0, 1), class = b, resp = "logW"))
  fit <- brm(bfB + bfW + set_rescor(TRUE), data = d, prior = pr,
             chains = 4, iter = 4000, warmup = 1000, seed = 20260620,
             cores = 4, refresh = 0, control = list(adapt_delta = 0.95))
  saveRDS(fit, fit_path)
}
cat(sprintf("convergence: max Rhat %.4f | min Bulk-ESS %d | divergences %d\n",
            max(rhat(fit), na.rm = TRUE),
            round(min(summarise_draws(as_draws_df(fit), ess_bulk)$ess_bulk, na.rm = TRUE)),
            sum(subset(nuts_params(fit), Parameter == "divergent__")$Value)))

# ---- the four "measures" as contrasts of one posterior --------------------------------------------
dr <- as_draws_df(fit)
Bb <- dr$b_logB_demM; Wb <- dr$b_logW_demM; rb <- Bb - Wb     # between-state (Demonstration 1)
Bw <- dr$b_logB_demW; Ww <- dr$b_logW_demW; rw <- Bw - Ww     # within-state  (Demonstration 2)
q <- function(x) sprintf("%+.3f [%+.3f, %+.3f]", median(x), quantile(x, .025), quantile(x, .975))
cat("\nBETWEEN-state, per +1 SD between-democracy:\n")
cat("  Black/White ratio:", q(rb), " absolute Black:", q(Bb), " absolute White:", q(Wb), "\n")
cat("WITHIN-state, per +1 SD within-democracy:\n")
cat("  Black/White ratio:", q(rw), " absolute Black:", q(Bw), " absolute White:", q(Ww), "\n")
cat("\nthe measure makes the finding, as one posterior probability:\n")
cat("  P(ratio effect > 0 AND White effect < 0) =", round(mean(rb > 0 & Wb < 0), 3),
    "  (the flip is the white denominator)\n")
cat("  P(ratio effect > 0 AND Black effect < 0) =", round(mean(rb > 0 & Bb < 0), 3),
    "  (weaker: the Black rate is ~flat)\n")

# ---- figure: four measures are four contrasts of one posterior ------------------------------------
pd <- data.frame(`Black/White ratio` = rb, `absolute Black` = Bb, `absolute White` = Wb,
                 check.names = FALSE) |>
  pivot_longer(everything(), names_to = "measure", values_to = "effect")
pd$measure <- factor(pd$measure, levels = c("absolute White", "absolute Black", "Black/White ratio"))

p <- ggplot(pd, aes(effect, measure, fill = measure)) +
  geom_density_ridges(scale = 1.5, alpha = .9, color = "white", linewidth = .3, rel_min_height = .004) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey35", linewidth = .4) +
  # low-saturation blue / grey / red triad, near-equal value, distinguishable by hue only
  scale_fill_manual(values = c("absolute White"    = "#6F8AA1",
                               "absolute Black"     = "#8C8C8E",
                               "Black/White ratio"  = "#AC827A"), guide = "none") +
  scale_x_continuous(limits = c(-.4, .4)) +
  labs(title = "Four 'measures' are four contrasts of one posterior",
       subtitle = paste("Effect of +1 SD between-state democracy. The ratio is positive only because white",
                        "imprisonment (blue) falls faster than Black: the denominator does the work.",
                        sep = "\n"),
       x = "effect on log rate per +1 SD democracy (between-state)", y = NULL) +
  theme_poli(base_size = 13)

save_fig(p, file.path(proj, "figures", "06_bayes_contrasts.png"), width = 8, height = 4.3)
