# Posterior predictive checks for the final primary AR(1) model.

suppressPackageStartupMessages({
  library(brms)
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
models_dir <- file.path(work_root, "models")
results_dir <- file.path(work_root, "results")
figures_dir <- file.path(work_root, "figures")


source(file.path(script_dir, "figure_style.R"))
style_only <- "--style-only" %in% commandArgs(trailingOnly = TRUE)
fit <- readRDS(file.path(models_dir, "primary_ar1_final.rds"))
set.seed(20260909)
ndraws <- 400
state_indices <- split(seq_len(nrow(fit$data)), fit$data$state_abbr)
posterior_draws <- as_draws_df(fit)
draw_ids <- sample(seq_len(nrow(posterior_draws)), ndraws)

# brms implements these AR terms through lagged observed residuals. Use its
# conditional posterior predictions for distributional checks, then inspect
# whether the AR term removes lag-1 dependence from the one-step residuals.
if (style_only) {
  cached <- readRDS(file.path(results_dir, "r1_posterior_predictive_draws.rds"))
  yrep_black <- cached$black
  yrep_white <- cached$white
} else {
  yrep_black <- posterior_predict(fit, resp = "logB", draw_ids = draw_ids)
  yrep_white <- posterior_predict(fit, resp = "logW", draw_ids = draw_ids)
}

mu_black_no_ar <- posterior_linpred(
  fit,
  resp = "logB",
  draw_ids = draw_ids,
  re_formula = NULL,
  incl_autocor = FALSE
)
mu_black_ar <- posterior_linpred(
  fit,
  resp = "logB",
  draw_ids = draw_ids,
  re_formula = NULL,
  incl_autocor = TRUE
)
mu_white_no_ar <- posterior_linpred(
  fit,
  resp = "logW",
  draw_ids = draw_ids,
  re_formula = NULL,
  incl_autocor = FALSE
)
mu_white_ar <- posterior_linpred(
  fit,
  resp = "logW",
  draw_ids = draw_ids,
  re_formula = NULL,
  incl_autocor = TRUE
)

mean_lag1 <- function(y) {
  state_correlations <- vapply(state_indices, function(index) {
    values <- y[index]
    cor(values[-length(values)], values[-1])
  }, numeric(1))
  mean(state_correlations, na.rm = TRUE)
}

statistics_for <- function(y) {
  c(
    mean = mean(y),
    sd = sd(y),
    q05 = unname(quantile(y, 0.05)),
    q95 = unname(quantile(y, 0.95))
  )
}

summarise_ppc <- function(observed, replicated, response) {
  observed_statistics <- statistics_for(observed)
  replicated_statistics <- t(apply(replicated, 1, statistics_for))
  tibble(
    response = response,
    statistic = names(observed_statistics),
    observed = as.numeric(observed_statistics),
    predictive_median = apply(replicated_statistics, 2, median),
    predictive_lower_95 = apply(replicated_statistics, 2, quantile, 0.025),
    predictive_upper_95 = apply(replicated_statistics, 2, quantile, 0.975),
    posterior_predictive_p = vapply(seq_along(observed_statistics), function(i) {
      mean(replicated_statistics[, i] >= observed_statistics[[i]])
    }, numeric(1))
  )
}

ppc_table <- bind_rows(
  summarise_ppc(fit$data$logB, yrep_black, "Black log rate"),
  summarise_ppc(fit$data$logW, yrep_white, "White log rate")
)
if (!style_only) {
  write.csv(ppc_table, file.path(results_dir, "r1_posterior_predictive_checks.csv"), row.names = FALSE)
}

lag1_draws <- function(observed, mu_no_ar, mu_ar, response) {
  bind_rows(
    tibble(
      response = response,
      adjustment = "Before AR(1) term",
      draw = seq_len(nrow(mu_no_ar)),
      lag1 = apply(mu_no_ar, 1, function(mu) mean_lag1(observed - mu))
    ),
    tibble(
      response = response,
      adjustment = "After AR(1) term",
      draw = seq_len(nrow(mu_ar)),
      lag1 = apply(mu_ar, 1, function(mu) mean_lag1(observed - mu))
    )
  ) |>
    mutate(
      adjustment = factor(
        adjustment,
        levels = c("After AR(1) term", "Before AR(1) term")
      )
    )
}

lag1_frame <- bind_rows(
  lag1_draws(fit$data$logB, mu_black_no_ar, mu_black_ar, "Black rate"),
  lag1_draws(fit$data$logW, mu_white_no_ar, mu_white_ar, "White rate")
)

lag1_table <- lag1_frame |>
  group_by(response, adjustment) |>
  summarise(
    median = median(lag1),
    lower_95 = quantile(lag1, 0.025),
    upper_95 = quantile(lag1, 0.975),
    .groups = "drop"
  )
if (style_only) {
  saved_lag <- read.csv(file.path(results_dir, "r1_residual_autocorrelation_checks.csv"))
  saved_ppc <- read.csv(file.path(results_dir, "r1_posterior_predictive_checks.csv"))
  stopifnot(isTRUE(all.equal(as.matrix(lag1_table[c("median", "lower_95", "upper_95")]),
    as.matrix(saved_lag[c("median", "lower_95", "upper_95")]), tolerance = 1e-10)),
    isTRUE(all.equal(as.matrix(ppc_table[vapply(ppc_table, is.numeric, logical(1))]),
      as.matrix(saved_ppc[vapply(saved_ppc, is.numeric, logical(1))]), tolerance = 1e-10)))
} else {
  write.csv(lag1_table, file.path(results_dir, "r1_residual_autocorrelation_checks.csv"), row.names = FALSE)
}

density_data <- function(observed, replicated, response) {
  selected <- seq(1, nrow(replicated), length.out = 50) |> round() |> unique()
  simulated <- bind_rows(lapply(seq_along(selected), function(i) {
    estimate <- density(replicated[selected[[i]], ], n = 256)
    tibble(
      response = response,
      draw = i,
      x = estimate$x,
      density = estimate$y,
      type = "Posterior predictive"
    )
  }))
  observed_density <- density(observed, n = 256)
  observed_frame <- tibble(
    response = response,
    draw = NA_integer_,
    x = observed_density$x,
    density = observed_density$y,
    type = "Observed"
  )
  bind_rows(simulated, observed_frame)
}

plot_density <- function(observed, replicated, response, title, color) {
  density_frame <- density_data(observed, replicated, response)
  ggplot() +
    geom_line(
      data = filter(density_frame, type == "Posterior predictive"),
      aes(x, density, group = draw),
      color = color_mid,
      alpha = 0.18,
      linewidth = 0.3
    ) +
    geom_line(
      data = filter(density_frame, type == "Observed"),
      aes(x, density),
      color = color,
      linewidth = 0.65
    ) +
    labs(title = title, x = "Log imprisonment rate", y = "Density") +
    theme_r1() +
    theme(panel.grid = element_blank(), panel.grid.major = element_blank())
}

plot_lag1 <- function(data, response_name, title, color) {
  summary_data <- data |>
    filter(.data$response == .env$response_name) |>
    group_by(adjustment) |>
    summarise(
      median = median(lag1),
      lower_50 = quantile(lag1, 0.25),
      upper_50 = quantile(lag1, 0.75),
      lower_95 = quantile(lag1, 0.025),
      upper_95 = quantile(lag1, 0.975),
      .groups = "drop"
    )

  expected <- lag1_table |> filter(.data$response == .env$response_name)
  stopifnot(nrow(expected) == 2,
    identical(as.character(summary_data$adjustment), as.character(expected$adjustment)),
    max(abs(summary_data$median - expected$median)) < 1e-12,
    max(abs(summary_data$lower_95 - expected$lower_95)) < 1e-12,
    max(abs(summary_data$upper_95 - expected$upper_95)) < 1e-12,
    all(summary_data$lower_95 <= summary_data$lower_50),
    all(summary_data$lower_50 <= summary_data$median),
    all(summary_data$median <= summary_data$upper_50),
    all(summary_data$upper_50 <= summary_data$upper_95),
    all(summary_data$lower_95 >= -0.05), all(summary_data$upper_95 <= 0.85))

  ggplot(summary_data, aes(median, adjustment)) +
    geom_vline(xintercept = 0, color = color_mid, linewidth = 0.4, linetype = "22") +
    geom_segment(
      aes(x = lower_95, xend = upper_95, yend = adjustment),
      color = color,
      linewidth = 0.5
    ) +
    geom_segment(
      aes(x = lower_50, xend = upper_50, yend = adjustment),
      color = color,
      linewidth = 1.0
    ) +
    geom_segment(aes(x = median, xend = median,
                     y = as.numeric(adjustment) - 0.06,
                     yend = as.numeric(adjustment) + 0.06),
                 color = color, linewidth = 0.4) +
    scale_x_continuous(limits = c(-0.05, 0.85), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
    labs(title = title, x = "Mean residual lag-1 correlation", y = NULL) +
    theme_r1() +
    theme(panel.grid.major.y = element_blank())
}

p_density_black <- plot_density(
  fit$data$logB,
  yrep_black,
  "Black log rate",
  "A  Marginal distribution: Black rate",
  color_diagnostic_black
)
p_density_white <- plot_density(
  fit$data$logW,
  yrep_white,
  "White log rate",
  "B  Marginal distribution: White rate",
  color_diagnostic_white
)
p_lag_black <- plot_lag1(
  lag1_frame,
  "Black rate",
  "C  Residual dependence: Black rate",
  color_diagnostic_black
)
p_lag_white <- plot_lag1(
  lag1_frame,
  "White rate",
  "D  Residual dependence: White rate",
  color_diagnostic_white
)

for (panel in list(p_lag_black, p_lag_white)) {
  layers <- ggplot_build(panel)$data
  stopifnot(nrow(layers[[2]]) == 2, nrow(layers[[3]]) == 2, nrow(layers[[4]]) == 2,
    all(layers[[2]]$xend > layers[[2]]$x), all(layers[[3]]$xend > layers[[3]]$x),
    all(layers[[4]]$xend == layers[[4]]$x))
}
figure_s2 <- ((p_density_black | p_density_white) / (p_lag_black | p_lag_white)) +
  plot_annotation(
    caption = "Vertical ticks: medians. Thick/thin segments: 50%/95% credible intervals.",
    theme = theme(plot.caption = element_text(family = font_regular, color = color_ink,
      size = 7.7, hjust = 0, margin = margin(t = 6)), plot.margin = margin(3, 5, 5, 5)))
if (!"--legacy-layout" %in% commandArgs(trailingOnly = TRUE)) {
  lag_summary <- lag1_frame |>
    group_by(response, adjustment) |>
    summarise(median = median(lag1),
      lower_50 = quantile(lag1, 0.25), upper_50 = quantile(lag1, 0.75),
      lower_95 = quantile(lag1, 0.025), upper_95 = quantile(lag1, 0.975),
      .groups = "drop") |>
    mutate(y = case_when(
      response == "Black rate" & adjustment == "Before AR(1) term" ~ 4,
      response == "Black rate" ~ 3,
      adjustment == "Before AR(1) term" ~ 2,
      TRUE ~ 1
    ))
  stopifnot(identical(lag_summary$response, lag1_table$response),
    identical(lag_summary$adjustment, lag1_table$adjustment),
    max(abs(lag_summary$median - lag1_table$median)) < 1e-12,
    max(abs(lag_summary$lower_95 - lag1_table$lower_95)) < 1e-12,
    max(abs(lag_summary$upper_95 - lag1_table$upper_95)) < 1e-12,
    all(lag_summary$lower_95 <= lag_summary$lower_50),
    all(lag_summary$lower_50 <= lag_summary$median),
    all(lag_summary$median <= lag_summary$upper_50),
    all(lag_summary$upper_50 <= lag_summary$upper_95))

  # One dumbbell row per rate: the lag-1 residual correlation before and after the AR(1) term.
  dumbbell <- lag_summary |>
    mutate(stage = if_else(adjustment == "Before AR(1) term", "before", "after"),
      response = factor(response, levels = c("White rate", "Black rate")),
      stage = factor(stage, levels = c("before", "after"),
        labels = c("Before AR(1) term", "After AR(1) term")),
      label = sprintf("%.2f [%.2f, %.2f]", median, lower_95, upper_95))
  spans <- dumbbell |>
    group_by(response) |>
    summarise(low = min(median), high = max(median), .groups = "drop")
  stopifnot(nrow(dumbbell) == 4, nrow(spans) == 2, all(spans$high > spans$low))
  p_dependence <- ggplot(dumbbell, aes(median, response)) +
    geom_vline(xintercept = 0, color = color_mid, linewidth = 0.35, linetype = "22") +
    geom_segment(data = spans, aes(x = low, xend = high, y = response, yend = response),
      color = "#B4BABF", linewidth = 0.5) +
    geom_point(aes(shape = stage, color = response), size = 2.2, fill = "white", stroke = 0.7) +
    geom_text(aes(label = label, hjust = if_else(stage == "Before AR(1) term", 1, 0)),
      nudge_y = 0.3, family = font_regular, size = 8 / .pt, color = color_ink) +
    scale_shape_manual(values = c("Before AR(1) term" = 21, "After AR(1) term" = 16),
      guide = guide_legend(override.aes = list(color = color_ink))) +
    scale_color_manual(values = c("Black rate" = color_diagnostic_black,
      "White rate" = color_diagnostic_white), guide = "none") +
    scale_x_continuous(limits = c(-0.05, 0.88), breaks = seq(0, 0.8, 0.2),
      labels = label_number(accuracy = 0.1)) +
    scale_y_discrete(expand = expansion(add = c(0.45, 0.7))) +
    labs(title = "C  Residual dependence",
      x = "Mean residual lag-1 correlation", y = NULL) +
    theme_r1() +
    theme(panel.grid = element_blank(), panel.grid.major = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(color = "#303A41", linewidth = 0.2),
      legend.position = "top", legend.justification = "right",
      legend.direction = "horizontal", legend.margin = margin(0, 0, -4, 0),
      legend.box.spacing = grid::unit(0, "pt"),
      plot.title = element_text(margin = margin(b = -9)))
  density_notes <- function(panel, observed_xy, draws_xy, observed_at, draws_at) {
    panel +
      annotate("segment", x = observed_at[1], y = observed_at[2], xend = observed_xy[1],
        yend = observed_xy[2], linewidth = 0.25, color = color_ink) +
      annotate("text", x = observed_xy[1], y = observed_xy[2], label = "Observed",
        hjust = 1.08, family = font_regular, size = 8 / .pt, color = color_ink) +
      annotate("segment", x = draws_at[1], y = draws_at[2], xend = draws_xy[1],
        yend = draws_xy[2], linewidth = 0.25, color = color_ink) +
      annotate("text", x = draws_xy[1], y = draws_xy[2], label = "50 posterior\npredictive draws",
        hjust = -0.06, lineheight = 0.95, family = font_regular, size = 8 / .pt,
        color = color_ink)
  }
  axis_lines <- theme(axis.line = element_line(color = "#303A41", linewidth = 0.2))
  p_density_black <- density_notes(p_density_black, c(7.0, 0.95), c(8.12, 1.02),
    c(7.42, 0.8), c(7.9, 0.8)) + coord_cartesian(clip = "off") + axis_lines
  p_density_white <- p_density_white + axis_lines
  figure_zoom <- ((p_density_black | p_density_white) / p_dependence) +
    plot_layout(heights = c(1, 0.5)) +
    plot_annotation(theme = theme(plot.margin = margin(3, 5, 5, 5)))
  stem <- if ("--zoom-layout" %in% commandArgs(trailingOnly = TRUE)) {
    "figure_s2_diagnostic_zoom_trial"
  } else {
    "figure_s2_posterior_predictive_checks"
  }
  save_figure(figure_zoom, stem, 7.25, 4.9)
} else {
  save_figure(figure_s2, "figure_s2_diagnostic_legacy", 7.25, 5.9)
}

if (!style_only) saveRDS(
  list(black = yrep_black, white = yrep_white),
  file.path(results_dir, "r1_posterior_predictive_draws.rds")
)

cat("Posterior predictive checks\n")
print(ppc_table, n = Inf, width = Inf)
cat("\nResidual autocorrelation checks\n")
print(lag1_table, n = Inf, width = Inf)
