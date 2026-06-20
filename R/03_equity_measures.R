# 03_equity_measures.R
# Step 2: "racial inequity" is not one number. Hold the democracy axis fixed and
# compare FOUR operationalizations against it:
#   (a) Black/White prison-rate ratio   (b) absolute Black rate
#   (c) absolute White rate             (d) gap = Black - White (per 100k)
# Goal: show the conclusion can FLIP with the measure, and expose the mechanism
# behind the step-1 "ratio paradox" (the White-rate panel).
# DESCRIPTIVE / cross-sectional only.

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2) })
source("R/00_theme.R")

proj <- Sys.getenv("DEM_CRIME_ROOT", unset = getwd())  # project root: run from repo root, or set DEM_CRIME_ROOT
df <- readRDS(file.path(proj, "data", "state_dem_incarceration.rds"))

win <- 2016:2020
st <- df |>
  filter(year %in% win,
         is.finite(black_prison_pop_rate), is.finite(white_prison_pop_rate),
         is.finite(democracy)) |>
  group_by(state_abbr, region) |>
  summarise(democracy  = mean(democracy),
            black_rate = mean(black_prison_pop_rate),
            white_rate = mean(white_prison_pop_rate),
            .groups = "drop") |>
  mutate(bw_ratio = black_rate / white_rate,
         bw_gap   = black_rate - white_rate)

long <- st |>
  pivot_longer(c(bw_ratio, black_rate, white_rate, bw_gap),
               names_to = "measure", values_to = "value") |>
  mutate(measure = factor(measure,
           levels = c("bw_ratio", "black_rate", "white_rate", "bw_gap"),
           labels = c("Black / White ratio", "Black rate (per 100k)",
                      "White rate (per 100k)", "Gap: Black - White (per 100k)")))

corr <- long |>
  group_by(measure) |>
  summarise(r = cor(democracy, value), .groups = "drop") |>
  mutate(label = sprintf("r = %+.2f", r))

cat("states:", nrow(st), "\n")
cat("correlation of State Democracy Index with each equity measure:\n")
print(as.data.frame(corr[, c("measure", "r")]), row.names = FALSE)

p <- ggplot(long, aes(democracy, value)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey35", fill = "grey85", linewidth = .6) +
  geom_point(aes(fill = region), shape = 21, color = "white", stroke = 0.3, size = 2.4) +
  scale_fill_manual(values = region_pal) +
  geom_text(data = corr, aes(x = -Inf, y = Inf, label = label), family = FONT,
            hjust = -0.15, vjust = 1.5, inherit.aes = FALSE, size = 4.2, fontface = "bold") +
  facet_wrap(~measure, scales = "free_y") +
  labs(
    title    = "One democracy axis, four ways to measure 'racial inequity' - the sign flips",
    subtitle = "Each point = one U.S. state, averaged 2016-2020.  x = State Democracy Index 2.0 (higher = more democratic).",
    x = "State Democracy Index 2.0", y = NULL, fill = "Region",
    caption  = "Sources: Grumbach & Bitton SDI 2.0; Vera Institute Incarceration Trends.  Descriptive / cross-sectional only."
  ) +
  theme_poli(base_size = 12) +
  theme(legend.position = "right")

save_fig(p, file.path(proj, "figures", "02_equity_measures.png"), width = 10, height = 7)
cat("saved figures/02_equity_measures.png\n")
