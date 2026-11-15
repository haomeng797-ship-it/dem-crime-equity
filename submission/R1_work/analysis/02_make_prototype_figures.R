# Draw Figure 1 from the merged data. Figures 2 and 3 come from 12_redesign_figure2.R and
# 13_style_figure3.R; Figure D1 comes from 06_posterior_predictive_checks.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(posterior)
  library(scales)
  library(tidyr)
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
figures_dir <- file.path(work_root, "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)


source(file.path(script_dir, "figure_style.R"))

# State averages used in the measurement figures.
state_means <- read.csv(file.path(project_root, "data", "state_dem_incarceration.csv")) |>
  filter(year >= 2016, year <= 2020) |>
  group_by(state_name, state_abbr) |>
  summarise(
    democracy = mean(democracy, na.rm = TRUE),
    black_rate = mean(black_prison_pop_rate, na.rm = TRUE),
    white_rate = mean(white_prison_pop_rate, na.rm = TRUE),
    gap = mean(black_prison_pop_rate - white_prison_pop_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    ratio = black_rate / white_rate,
    rank_ratio = min_rank(desc(ratio)),
    rank_gap = min_rank(desc(gap)),
    rank_difference = rank_gap - rank_ratio
  )

# Figure 1: ratio and gap are different geometries of the same two rates.
stopifnot(nrow(state_means) == 50L, !anyDuplicated(state_means$state_abbr),
  all(is.finite(as.matrix(state_means[c("black_rate", "white_rate", "ratio", "gap")]))),
  all(state_means$white_rate >= 50 & state_means$white_rate <= 850),
  all(state_means$black_rate >= 450 & state_means$black_rate <= 4100))

# Journal-style layout (2026-11-15): axes start at the origin so the equal-ratio rays
# visibly meet there; guide labels run along their lines; both example pairs are named
# in both panels, with the pair that matches the panel's scale in color.
x_max <- 880
y_max <- 4100
panel_aspect <- 0.88
screen_angle <- function(slope) atan(slope * (x_max / y_max) * panel_aspect) * 180 / pi
label_size <- 8 / ggplot2::.pt

ratio_guides <- tibble(
  slope = c(3, 5, 7, 9),
  x = c(800, 742, 548, 428),
  y = slope * x,
  angle = screen_angle(slope),
  label = paste("Ratio", slope)
)
gap_guides <- tibble(
  intercept = c(0, 500, 1000, 1500, 2000, 2500),
  x = rep(868, 6),
  y = x + intercept,
  angle = screen_angle(1),
  label = c("Gap 0", paste("Gap", format(intercept[-1], big.mark = ",", trim = TRUE)))
)

examples <- state_means |>
  filter(state_abbr %in% c("MD", "SD", "MA", "NH")) |>
  mutate(
    pair = if_else(state_abbr %in% c("MD", "SD"), "ratio", "gap"),
    # Leaders are vertical so they read as callouts and cannot be mistaken for a line
    # joining states. Heights are chosen by eye to end in empty space in each panel.
    label_x = white_rate,
    label_y_ratio = c(MA = 105, MD = 430, NH = 500, SD = 2750)[state_abbr],
    label_y_gap = c(MA = 870, MD = 430, NH = 500, SD = 2750)[state_abbr]
  )
stopifnot(nrow(examples) == 4L)

geometry_panel <- function(focus, guides, guide_slope, title, show_y) {
  focus_color <- if (focus == "ratio") color_ratio else color_gap
  shown <- mutate(examples, in_focus = pair == focus,
    label_y = if (focus == "ratio") label_y_ratio else label_y_gap)
  ggplot(state_means, aes(white_rate, black_rate)) +
    geom_abline(data = guides, aes(slope = {{ guide_slope }},
      intercept = if ("intercept" %in% names(guides)) intercept else 0),
      inherit.aes = FALSE, color = "#B4BABF", linewidth = 0.4) +
    geom_point(shape = 16, size = 1.15, color = "#3A3A3A", alpha = 0.8) +
    geom_text(data = guides, aes(x = x, y = y, label = label, angle = angle),
      inherit.aes = FALSE, hjust = 1, vjust = -0.45, size = label_size * 0.94,
      family = font_regular, color = "#4A4F54") +
    geom_segment(data = shown, aes(xend = label_x, yend = label_y,
      color = in_focus), linewidth = 0.25) +
    geom_point(data = shown, aes(color = in_focus, size = in_focus), shape = 16) +
    geom_label(data = shown, aes(x = label_x, y = label_y, label = state_abbr,
      color = in_focus), family = font_regular, size = label_size, fill = "white",
      linewidth = 0, label.padding = grid::unit(0.1, "lines"),
      label.r = grid::unit(0, "lines")) +
    scale_color_manual(values = c(`TRUE` = focus_color, `FALSE` = "#8C9399"), guide = "none") +
    scale_size_manual(values = c(`TRUE` = 2.3, `FALSE` = 1.7), guide = "none") +
    scale_x_continuous(limits = c(0, x_max), breaks = seq(0, 800, by = 200),
      labels = label_comma(), expand = expansion(mult = 0)) +
    scale_y_continuous(limits = c(0, y_max), breaks = seq(0, 4000, by = 1000),
      labels = label_comma(), expand = expansion(mult = 0)) +
    labs(title = title, x = "White imprisonment rate per 100,000",
      y = if (show_y) "Black imprisonment rate per 100,000" else NULL) +
    theme_r1() +
    theme(
      aspect.ratio = panel_aspect,
      panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      axis.line = element_line(color = "#303A41", linewidth = 0.2),
      axis.text.y = if (show_y) element_text() else element_blank(),
      axis.ticks.y = if (show_y) element_line() else element_blank(),
      legend.position = "none"
    )
}

p_geometry_ratio <- geometry_panel("ratio", ratio_guides, slope,
  "A  Relative disparity", TRUE)
p_geometry_gap <- geometry_panel("gap", mutate(gap_guides, slope = 1), slope,
  "B  Absolute disparity", FALSE)

figure_1 <- p_geometry_ratio + p_geometry_gap
save_figure(figure_1, "figure_1_estimand_geometry", 7.25, 3.75)
message("Figure 1 saved to ", figures_dir)
