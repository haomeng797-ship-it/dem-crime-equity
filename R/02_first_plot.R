# 02_first_plot.R
# Teaser: across U.S. states, does weaker democracy go with a wider racial incarceration gap?
# Cross-sectional view: average each state over a recent window to cut noise/missingness.
# DESCRIPTIVE / correlational only -- not causal.

suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })
source("R/00_theme.R")

proj <- path.expand("~/Documents/dem-crime-equity")
df <- readRDS(file.path(proj, "data", "state_dem_incarceration.rds"))

win <- 2016:2020
st <- df |>
  filter(year %in% win, is.finite(bw_ratio), is.finite(democracy)) |>
  group_by(state_abbr, region) |>
  summarise(democracy = mean(democracy, na.rm = TRUE),
            bw_ratio  = mean(bw_ratio,  na.rm = TRUE),
            .groups = "drop")

cat("states plotted:", nrow(st), "\n")
cat(sprintf("correlation (democracy vs B/W ratio): r = %.2f\n",
            cor(st$democracy, st$bw_ratio)))

p <- ggplot(st, aes(democracy, bw_ratio)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey35", fill = "grey85", linewidth = .6) +
  geom_point(aes(color = region), size = 2.6) +
  scale_color_manual(values = region_pal) +
  labs(
    title    = "Weaker state democracy, wider racial incarceration gap?",
    subtitle = "Each point = one U.S. state, averaged 2016-2020.  y = Black-to-White prison-rate ratio.",
    x = "State Democracy Index 2.0  (higher = more democratic)",
    y = "Black / White prison-rate ratio",
    color = "Region",
    caption = paste0("Sources: Grumbach & Bitton, State Democracy Index 2.0 (UC Berkeley); ",
                     "Vera Institute, Incarceration Trends.\nDescriptive association only - not causal.")
  ) +
  theme_poli(base_size = 13) +
  theme(legend.position = "right")

# label states (use ggrepel if available, else plain text)
if (requireNamespace("ggrepel", quietly = TRUE)) {
  p <- p + ggrepel::geom_text_repel(aes(label = state_abbr), size = 3, max.overlaps = 20, seed = 1)
} else {
  p <- p + geom_text(aes(label = state_abbr), size = 2.6, vjust = -0.7, check_overlap = TRUE)
}

save_fig(p, file.path(proj, "figures", "01_dem_vs_disparity.png"), width = 8.5, height = 5.5)
cat("saved figures/01_dem_vs_disparity.png\n")
