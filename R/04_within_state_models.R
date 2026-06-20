# 04_within_state_models.R
# Step 3: within-state over time. Does a state's OWN change in democracy track its
# change in racial incarceration outcomes, net of fixed state differences & common year shocks?
# Method: linear mixed models (lme4) with a within/between (Mundlak) decomposition:
#   democracy -> dem_between (state mean; cross-sectional) + dem_within (deviation; longitudinal).
# Outcomes (logged; rates are right-skewed): B/W ratio, Black rate, White rate.
# Associational + FE-adjusted -- NOT a clean natural experiment (that is design #2).

suppressPackageStartupMessages({ library(dplyr); library(lme4); library(ggplot2) })
source("R/00_theme.R")

proj <- Sys.getenv("DEM_CRIME_ROOT", unset = getwd())  # project root: run from repo root, or set DEM_CRIME_ROOT
df <- readRDS(file.path(proj, "data", "state_dem_incarceration.rds"))

panel <- df |>
  filter(is.finite(bw_ratio), black_prison_pop_rate > 0, white_prison_pop_rate > 0,
         is.finite(democracy)) |>
  group_by(state_abbr) |>
  mutate(dem_between = mean(democracy),
         dem_within  = democracy - dem_between,
         n_state     = n()) |>
  ungroup() |>
  filter(n_state >= 10) |>          # keep states with a real time series
  mutate(year_c = year - 2011)      # center year near the midpoint

cat("panel: rows", nrow(panel), " states", n_distinct(panel$state_abbr),
    " years", min(panel$year), "-", max(panel$year), "\n\n")

outcomes <- c("log(Black/White ratio)" = "log(bw_ratio)",
              "log(Black rate)"        = "log(black_prison_pop_rate)",
              "log(White rate)"        = "log(white_prison_pop_rate)")

rows <- list()
for (nm in names(outcomes)) {
  f <- as.formula(paste0(outcomes[[nm]],
        " ~ dem_within + dem_between + year_c + (1 | state_abbr)"))
  m  <- lmer(f, data = panel, REML = TRUE)
  co <- summary(m)$coefficients
  cat("==", nm, "==\n")
  print(round(co[c("dem_within", "dem_between", "year_c"), c("Estimate", "Std. Error", "t value")], 4))
  cat("\n")
  for (term in c("dem_within", "dem_between")) {
    est <- co[term, "Estimate"]; se <- co[term, "Std. Error"]
    rows[[length(rows) + 1]] <- data.frame(outcome = nm, term = term,
      est = est, lo = est - 1.96 * se, hi = est + 1.96 * se)
  }
}
res <- do.call(rbind, rows) |>
  mutate(term = recode(term,
            dem_within  = "Within-state (over time)",
            dem_between = "Between-state (cross-sectional)"),
         outcome = factor(outcome, levels = names(outcomes)))

p <- ggplot(res, aes(est, outcome, color = term)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey60") +
  geom_pointrange(aes(xmin = lo, xmax = hi),
                  position = position_dodge(width = .55), linewidth = .8, size = .6) +
  scale_color_manual(values = wb_pal) +
  labs(
    title    = "Within a state over time, does democracy track racial incarceration?",
    subtitle = "lme4 democracy coefficients, within- vs between-state.  + = higher democracy goes with a higher (log) outcome.",
    x = "Effect of State Democracy Index (log change per 1 SDI unit)",
    y = NULL, color = NULL,
    caption = "Mundlak within/between decomposition; random intercept by state; year-trend adjusted.\nSources: SDI 2.0 (Grumbach & Bitton); Vera Incarceration Trends. Associational, FE-adjusted - not causal."
  ) +
  theme_poli(base_size = 12)

save_fig(p, file.path(proj, "figures", "03_within_between.png"), width = 9, height = 5)
cat("saved figures/03_within_between.png\n")
