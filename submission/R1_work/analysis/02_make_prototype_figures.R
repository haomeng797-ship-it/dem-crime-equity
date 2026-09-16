# Create the first R1 figure prototypes from the submitted data and fitted models.

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

ratio_guides <- tibble(
  slope = c(3, 5, 7, 9),
  x = c(840, 700, 500, 385),
  y = slope * x,
  label = paste("Ratio", slope)
)

gap_guides <- tibble(
  intercept = c(0, 500, 1000, 1500, 2000, 2500),
  x = rep(850, 6),
  y = x + intercept,
  label = c("Gap 0", paste("Gap", format(intercept[-1], big.mark = ",")))
)

# Keep every gap label clear of the state points and of the other labels.
# The rendered text occupies about x in [anchor - 131, anchor] and y in
# [anchor - 10, anchor + 133] in data units, measured on the 400-dpi export;
# the test box adds the point radius (about 7 by 35 data units) as margin.
gap_label_clear <- vapply(seq_len(nrow(gap_guides)), function(i) {
  !any(state_means$white_rate >= gap_guides$x[i] - 140 &
    state_means$white_rate <= gap_guides$x[i] &
    state_means$black_rate >= gap_guides$y[i] - 45 &
    state_means$black_rate <= gap_guides$y[i] + 175)
}, logical(1))
stopifnot(all(gap_label_clear), all(diff(sort(gap_guides$y)) > 220))

geometry_base <- function() {
  ggplot(state_means, aes(white_rate, black_rate)) +
    geom_point(
      shape = 21,
      size = 1.75,
      stroke = 0.3,
      color = "white",
      fill = color_ink,
      alpha = 0.72
    ) +
    scale_x_continuous(
      limits = c(50, 850),
      breaks = seq(200, 800, by = 200),
      labels = label_comma()
    ) +
    scale_y_continuous(
      limits = c(450, 4100),
      breaks = seq(1000, 4000, by = 1000),
      labels = label_comma()
    ) +
    labs(
      x = "White imprisonment rate per 100,000",
      y = "Black imprisonment rate per 100,000"
    ) +
    theme_r1() +
    theme(
      panel.grid.major = element_line(color = color_grid, linewidth = 0.25),
      legend.position = "none"
    )
}

ratio_examples <- filter(state_means, state_abbr %in% c("MD", "SD"))
gap_examples <- filter(state_means, state_abbr %in% c("MA", "NH"))

p_geometry_ratio <- geometry_base() +
  geom_abline(
    data = ratio_guides,
    aes(slope = slope, intercept = 0),
    inherit.aes = FALSE,
    color = "#ABB2B7",
    linewidth = 0.55
  ) +
  geom_text(
    data = ratio_guides,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = -0.35,
    size = 2.35,
    family = font_regular,
    color = color_ink
  ) +
  geom_point(
    data = ratio_examples,
    shape = 21,
    size = 4.2,
    stroke = 0.65,
    color = "white",
    fill = color_ratio
  ) +
  geom_text(
    data = ratio_examples,
    aes(label = state_abbr),
    color = "white",
    family = font_emphasis,
    fontface = "plain",
    size = 2.25
  ) +
  labs(
    title = "A  Relative disparity",
    subtitle = "Equal ratios lie on rays from the origin"
  )

p_geometry_gap <- geometry_base() +
  geom_abline(
    data = gap_guides,
    aes(slope = 1, intercept = intercept),
    inherit.aes = FALSE,
    color = "#ABB2B7",
    linewidth = 0.55
  ) +
  geom_text(
    data = gap_guides,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 1.02,
    vjust = -0.35,
    size = 2.35,
    family = font_regular,
    color = color_ink
  ) +
  geom_point(
    data = gap_examples,
    shape = 21,
    size = 4.2,
    stroke = 0.65,
    color = "white",
    fill = color_gap
  ) +
  geom_text(
    data = gap_examples,
    aes(label = state_abbr),
    color = "white",
    family = font_emphasis,
    fontface = "plain",
    size = 2.25
  ) +
  labs(
    title = "B  Absolute disparity",
    subtitle = "Equal gaps lie on parallel lines",
    y = NULL
  )

figure_1 <- p_geometry_ratio + p_geometry_gap
save_figure(figure_1, "figure_1_estimand_geometry", 7.25, 3.75)

rank_labels <- state_means |>
  filter(state_abbr %in% c("NJ", "NY", "MA", "OK", "AR", "AZ")) |>
  mutate(
    label_x = case_when(
      state_abbr == "NJ" ~ 5,
      state_abbr == "NY" ~ 8,
      state_abbr == "MA" ~ 20,
      state_abbr == "OK" ~ 30,
      state_abbr == "AR" ~ 44,
      state_abbr == "AZ" ~ 35
    ),
    label_y = case_when(
      state_abbr == "NJ" ~ 30,
      state_abbr == "NY" ~ 46,
      state_abbr == "MA" ~ 48,
      state_abbr == "OK" ~ 4,
      state_abbr == "AR" ~ 17,
      state_abbr == "AZ" ~ 8
    )
  )

p_rank <- ggplot(state_means, aes(rank_ratio, rank_gap)) +
  annotate(
    "rect",
    xmin = 1,
    xmax = 50,
    ymin = 1,
    ymax = 50,
    fill = "white"
  ) +
  geom_abline(intercept = 0, slope = 1, color = color_mid, linewidth = 0.45, linetype = "22") +
  geom_point(
    aes(
      fill = case_when(
        rank_difference >= 10 ~ "ratio",
        rank_difference <= -10 ~ "gap",
        TRUE ~ "near"
      )
    ),
    shape = 21,
    size = 2.2,
    stroke = 0.35,
    color = "white",
    alpha = 0.88
  ) +
  geom_segment(
    data = rank_labels,
    aes(xend = label_x, yend = label_y),
    color = color_mid,
    linewidth = 0.3
  ) +
  geom_text(
    data = rank_labels,
    aes(x = label_x, y = label_y, label = state_abbr),
    family = font_emphasis,
    fontface = "plain",
    size = 2.5,
    color = color_ink
  ) +
  scale_fill_manual(
    values = c("ratio" = color_ratio, "gap" = color_gap, "near" = color_mid),
    guide = "none"
  ) +
  scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50), limits = c(0, 51)) +
  scale_y_continuous(breaks = c(1, 10, 20, 30, 40, 50), limits = c(0, 51)) +
  coord_equal() +
  labs(
    title = "C  State rankings diverge",
    subtitle = "Color marks a rank difference of 10 places or more",
    x = "Rank by ratio (1 = highest)",
    y = "Rank by gap (1 = highest)"
  ) +
  theme_r1() +
  theme(panel.grid.minor = element_blank())

figure_1_with_ranks <- (p_geometry_ratio + p_geometry_gap) / p_rank +
  plot_layout(heights = c(1, 1.05))
save_figure(figure_1_with_ranks, "figure_1_estimand_geometry_and_ranks", 7.25, 7.15)

# Supplementary Figure S1: descriptive associations with state democracy.

scatter_panel <- function(data, outcome, panel_title, panel_subtitle, y_label, color,
                          y_labels = label_number()) {
  correlation <- cor(data$democracy, data[[outcome]], use = "complete.obs")
  ggplot(data, aes(x = democracy, y = .data[[outcome]])) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      color = color,
      fill = color,
      linewidth = 0.65,
      alpha = 0.12
    ) +
    geom_point(
      shape = 21,
      size = 2.05,
      stroke = 0.35,
      color = "white",
      fill = color_ink,
      alpha = 0.78
    ) +
    annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = sprintf("r = %+.2f", correlation),
      hjust = -0.1,
      vjust = 1.35,
      size = 2.55,
      family = font_regular,
      color = color_ink
    ) +
    scale_x_continuous(breaks = c(-3, -2, -1, 0, 1)) +
    scale_y_continuous(labels = y_labels, expand = expansion(mult = c(0.08, 0.12))) +
    labs(
      title = panel_title,
      subtitle = panel_subtitle,
      x = "State Democracy Index",
      y = y_label
    ) +
    theme_r1()
}

p_ratio <- scatter_panel(
  state_means,
  "ratio",
  "A  Relative disparity",
  "Black/White rate ratio",
  "Rate ratio",
  color_ratio,
  label_number(accuracy = 1)
) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

p_gap <- scatter_panel(
  state_means,
  "gap",
  "B  Absolute disparity",
  "Black minus White rate",
  "Difference per 100,000",
  color_white,
  label_comma()
) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

p_black <- scatter_panel(
  state_means,
  "black_rate",
  "C  Group-specific burden",
  "Black imprisonment rate",
  "Rate per 100,000",
  color_ratio,
  label_comma()
)

p_white <- scatter_panel(
  state_means,
  "white_rate",
  "D  Group-specific burden",
  "White imprisonment rate",
  "Rate per 100,000",
  color_white,
  label_comma()
)

figure_s1 <- (p_ratio | p_gap) / (p_black | p_white) +
  plot_layout(guides = "collect")

save_figure(figure_s1, "figure_s1_descriptive_associations", 7.25, 6.15)

# Figure 2: joint posterior for the two disparity estimands and their rate components.
all_draws <- readRDS(file.path(results_dir, "r1_posterior_draws.rds"))
main_draws <- all_draws |>
  filter(model == "AR(1) log-rate", design == "between")

prob_both_increase <- mean(main_draws$ratio_pct > 0 & main_draws$gap_per_100k > 0)
prob_ratio_only <- mean(main_draws$ratio_pct > 0 & main_draws$gap_per_100k <= 0)

p_joint <- ggplot(main_draws, aes(ratio_pct, gap_per_100k)) +
  geom_hline(yintercept = 0, color = color_mid, linewidth = 0.4, linetype = "22") +
  geom_vline(xintercept = 0, color = color_mid, linewidth = 0.4, linetype = "22") +
  geom_point(color = color_ink, size = 0.65, alpha = 0.035) +
  stat_density_2d(color = color_ink, bins = 5, linewidth = 0.38) +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = sprintf("Ratio increases\nGap increases\n%.1f%%", 100 * prob_both_increase),
    hjust = 1.08,
    vjust = 1.25,
    size = 2.55,
    family = font_regular,
    color = color_gap,
    lineheight = 0.95
  ) +
  annotate(
    "text",
    x = Inf,
    y = -Inf,
    label = sprintf("Ratio increases\nGap decreases\n%.1f%%", 100 * prob_ratio_only),
    hjust = 1.08,
    vjust = -0.3,
    size = 2.55,
    family = font_regular,
    color = color_ratio,
    lineheight = 0.95
  ) +
  scale_x_continuous(labels = label_number(suffix = "%", accuracy = 1)) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(
    title = "A  Joint posterior for the disparity measures",
    subtitle = "One-SD higher long-run democracy, between states",
    x = "Change in Black/White ratio",
    y = "Change in Black-White gap per 100,000"
  ) +
  theme_r1()

rate_long <- main_draws |>
  select(.draw, black_rate_pct, white_rate_pct) |>
  pivot_longer(
    cols = c(black_rate_pct, white_rate_pct),
    names_to = "rate",
    values_to = "estimate"
  ) |>
  mutate(
    rate = factor(
      rate,
      levels = c("white_rate_pct", "black_rate_pct"),
      labels = c("White rate", "Black rate")
    )
  )

rate_summary <- rate_long |>
  group_by(rate) |>
  summarise(
    median = median(estimate),
    lower_50 = quantile(estimate, 0.25),
    upper_50 = quantile(estimate, 0.75),
    lower_95 = quantile(estimate, 0.025),
    upper_95 = quantile(estimate, 0.975),
    .groups = "drop"
  )

p_rates <- ggplot(rate_summary, aes(median, rate, color = rate)) +
  geom_vline(xintercept = 0, color = color_mid, linewidth = 0.4, linetype = "22") +
  geom_segment(aes(x = lower_95, xend = upper_95, yend = rate), linewidth = 0.65) +
  geom_segment(aes(x = lower_50, xend = upper_50, yend = rate), linewidth = 2.1) +
  geom_point(size = 2.2) +
  scale_color_manual(values = c("Black rate" = color_black, "White rate" = color_white), guide = "none") +
  scale_x_continuous(labels = label_number(suffix = "%", accuracy = 1)) +
  labs(
    title = "B  Underlying group-specific rates",
    subtitle = "Median, 50% interval, and 95% interval",
    x = "Rate change",
    y = NULL
  ) +
  theme_r1() +
  theme(panel.grid.major.y = element_blank())

figure_2 <- p_joint + p_rates + plot_layout(widths = c(1.65, 1))
save_figure(figure_2, "figure_2_joint_posterior", 7.25, 3.55)

# Figure 3: robustness of the two disparity estimands across fitted models.
model_levels <- c(
  "AR(1) log-rate",
  "Log-rate, no AR(1)",
  "Region + AR(1)",
  "Negative-binomial count",
  "Recording-screen sample"
)

robustness <- all_draws |>
  filter(design == "between") |>
  mutate(model = factor(model, levels = rev(model_levels))) |>
  select(.draw, model, ratio_pct, gap_per_100k) |>
  pivot_longer(c(ratio_pct, gap_per_100k), names_to = "estimand", values_to = "estimate") |>
  group_by(model, estimand) |>
  summarise(
    median = median(estimate),
    lower_50 = quantile(estimate, 0.25),
    upper_50 = quantile(estimate, 0.75),
    lower_95 = quantile(estimate, 0.025),
    upper_95 = quantile(estimate, 0.975),
    .groups = "drop"
  )

interval_panel <- function(data, estimand_name, panel_title, x_label, color, show_y = TRUE) {
  ggplot(filter(data, estimand == estimand_name), aes(median, model)) +
    geom_vline(xintercept = 0, color = color_mid, linewidth = 0.4, linetype = "22") +
    geom_segment(aes(x = lower_95, xend = upper_95, yend = model), color = color, linewidth = 0.55) +
    geom_segment(aes(x = lower_50, xend = upper_50, yend = model), color = color, linewidth = 1.9) +
    geom_point(shape = 21, size = 2.1, stroke = 0.35, color = "white", fill = color) +
    labs(title = panel_title, x = x_label, y = NULL) +
    theme_r1() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = if (show_y) element_text() else element_blank(),
      axis.ticks.y = if (show_y) element_line(color = color_mid, linewidth = 0.3) else element_blank()
    )
}

p_robust_ratio <- interval_panel(
  robustness,
  "ratio_pct",
  "A  Relative disparity",
  "Change in Black/White ratio (%)",
  color_ratio,
  TRUE
)

p_robust_gap <- interval_panel(
  robustness,
  "gap_per_100k",
  "B  Absolute disparity",
  "Change in gap per 100,000",
  color_gap,
  FALSE
)

figure_3 <- p_robust_ratio + p_robust_gap + plot_layout(widths = c(1.15, 1))
save_figure(figure_3, "figure_3_robustness", 7.25, 3.45)

cat("Saved figure prototypes to ", figures_dir, "\n", sep = "")
