# 06_causal_amendment4.R
# Step 5 (design #2): CAUSAL. Florida Amendment 4 (2018) felon re-enfranchisement as a
# natural experiment. One treated unit -> synthetic control + placebo (permutation) inference.
# Outcome: VAP turnout rate (denominator = voting-age population; not changed by who is
#   enfranchised -> avoids the mechanical VEP-denominator confound, which here is tiny anyway).
# Donor pool EXCLUDES states that also changed felon-voting laws in 2016-2022 (SUTVA).
# Caveats: SB7066 (2019) reimposed "pay fines first" -> likely mutes any effect; 2020 = COVID.

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2); library(tidysynth) })
source("R/00_theme.R")

proj <- path.expand("~/Documents/dem-crime-equity")
raw  <- read.csv(file.path(proj, "data", "raw", "ufl_turnout_1980_2022.csv"),
                 check.names = FALSE, stringsAsFactors = FALSE)

num    <- function(x) as.numeric(gsub("[,%]", "", trimws(x)))
firstok <- function(x) { x <- x[is.finite(x)]; if (length(x)) x[1] else NA_real_ }

d <- raw |>
  mutate(state = trimws(gsub("[*]", "", STATE)), year = as.integer(YEAR),
         vap = num(VAP), vap_turnout = num(VAP_TURNOUT_RATE),
         felons = num(INELIGIBLE_FELONS_TOTAL)) |>
  filter(year >= 2000, year %% 2 == 0,
         !state %in% c("United States", "District of Columbia")) |>
  group_by(state, year) |>
  summarise(vap = firstok(vap), vap_turnout = firstok(vap_turnout),
            felons = firstok(felons), .groups = "drop") |>
  mutate(felon_rate = 100 * felons / vap)

# balanced panel
yrs  <- sort(unique(d$year))
comp <- d |> group_by(state) |> summarise(ok = sum(is.finite(vap_turnout)), .groups = "drop")
balanced <- comp$state[comp$ok == length(yrs)]

# drop donors that ALSO reformed felon voting in 2016-2022 (keep Florida = treated)
reformers <- c("Nevada","New Jersey","Colorado","New York","Washington","Virginia",
               "Kentucky","Iowa","California","Connecticut","Louisiana","New Mexico")
donors <- setdiff(balanced, reformers)
panel  <- d |> filter(state %in% union("Florida", donors))
cat("balanced states:", length(balanced),
    "| after dropping reformers:", n_distinct(panel$state),
    "(Florida + ", n_distinct(panel$state) - 1, "clean donors )\n\n")

# --- first stage: Florida's disenfranchised-felon rate (did Amendment 4 bite?) ---
cat("Florida 2012-2022  (felon_rate = % of VAP disenfranchised by felony):\n")
print(d |> filter(state == "Florida", year >= 2012) |>
        transmute(year, vap_turnout, felons, felon_rate = round(felon_rate, 2)), row.names = FALSE)

# --- synthetic control for FL VAP turnout; treatment first hits 2020 ---
sc <- panel |>
  synthetic_control(outcome = vap_turnout, unit = state, time = year,
                    i_unit = "Florida", i_time = 2020, generate_placebos = TRUE) |>
  generate_predictor(time_window = 2000:2018, turnout_pre = mean(vap_turnout, na.rm = TRUE)) |>
  generate_predictor(time_window = 2016, turnout_2016 = vap_turnout) |>
  generate_predictor(time_window = 2012, turnout_2012 = vap_turnout) |>
  generate_predictor(time_window = 2008, turnout_2008 = vap_turnout) |>
  generate_weights(optimization_window = 2000:2018) |>
  generate_control()

gaps <- sc |> grab_synthetic_control() |> mutate(gap = real_y - synth_y)
cat("\nFL vs synthetic FL (VAP turnout, percentage points):\n")
print(gaps |> filter(time_unit >= 2014) |>
        transmute(year = time_unit, real = round(real_y, 1),
                  synth = round(synth_y, 1), gap = round(gap, 2)), row.names = FALSE)

cat("\ntop donor weights building synthetic Florida:\n")
print(sc |> grab_unit_weights() |> arrange(desc(weight)) |> head(6), row.names = FALSE)

cat("\nplacebo-based significance (Florida):\n")
print(sc |> grab_significance() |> filter(unit_name == "Florida") |>
        select(unit_name, type, pre_mspe, post_mspe, mspe_ratio, rank, fishers_exact_pvalue))

# --- plots (rebuilt from the grabbed series for full control over line weight + colour) ---
FL_RED <- "#8B2E36"   # subdued, low-key deep red for the treated unit (Florida)

# (1) observed vs. synthetic Florida -- thin lines, deep-red observed, grey dashed synthetic
trend_df <- sc |> grab_synthetic_control() |>
  transmute(year = time_unit, Observed = real_y, Synthetic = synth_y) |>
  tidyr::pivot_longer(c(Observed, Synthetic), names_to = "series", values_to = "turnout")

p1 <- ggplot(trend_df, aes(year, turnout, color = series, linetype = series)) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "grey55", linewidth = 0.3) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.3) +
  scale_color_manual(values = c("Observed" = FL_RED, "Synthetic" = "#9C968C")) +
  scale_linetype_manual(values = c("Observed" = "solid", "Synthetic" = "dashed")) +
  labs(title = "Florida vs. synthetic Florida: VAP turnout, 2000-2022",
       subtitle = "Amendment 4 effective 2019 (first election 2020); SB7066 reimposed fines-first mid-2019",
       x = "year", y = "VAP turnout (%)", color = NULL, linetype = NULL) +
  theme_poli()
save_fig(p1, file.path(proj, "figures", "05_synth_trends.png"), width = 8, height = 5)

# (2) placebo gaps -- replicate tidysynth's prune (RMSPE <= 2x treated), then draw by hand so
# the donor lines get slight hue variation (a narrow grey ramp) instead of fusing into one band.
pl  <- sc |> grab_synthetic_control(placebo = TRUE) |> mutate(diff = real_y - synth_y)
sig <- sc |> grab_significance()
thr <- sig |> filter(type == "Treated") |> pull(pre_mspe) |> sqrt()
keep <- sig |> filter(sqrt(pre_mspe) <= 2 * thr) |> pull(unit_name)
pl  <- pl |> filter(.id %in% keep)
ctrl <- pl |> filter(.placebo == 1); fl <- pl |> filter(.placebo == 0)

ctrl_ids  <- sort(unique(ctrl$.id))
ctrl_cols <- setNames(
  grDevices::colorRampPalette(c("#8E94A0", "#ABA59B", "#C7C0B4"))(length(ctrl_ids)), ctrl_ids)
line_cols <- c(ctrl_cols, Florida = FL_RED)

p2 <- ggplot(mapping = aes(time_unit, diff, group = .id, color = .id)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.3) +
  geom_vline(xintercept = 2020, linetype = "dotted", color = "grey55", linewidth = 0.3) +
  geom_line(data = ctrl, linewidth = 0.3, alpha = 0.75) +
  geom_line(data = fl,   linewidth = 0.9) +
  scale_color_manual(values = line_cols, breaks = c("Florida", ctrl_ids[1]),
                     labels = c("Florida", "control units"), name = NULL) +
  guides(color = guide_legend(override.aes = list(linewidth = c(0.9, 0.5), alpha = 1))) +
  labs(title = "Placebo test: Florida's turnout gap vs. clean donor states",
       x = "year", y = "turnout gap (percentage points)") +
  theme_poli()
save_fig(p2, file.path(proj, "figures", "05_synth_placebos.png"), width = 8, height = 5)

cat("\nsaved figures/05_synth_trends.png and 05_synth_placebos.png\n")
