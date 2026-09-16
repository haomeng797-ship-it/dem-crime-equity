# Typography and palette trial for Figure 2; reuse the saved posterior draws.
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(tidyr)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
work_root <- normalizePath(file.path(script_dir, ".."))
results_dir <- file.path(work_root, "results")
figures_dir <- file.path(work_root, "figures")
input <- file.path(results_dir, "r1_posterior_draws.rds")
original_figure <- file.path(figures_dir, "figure_2_joint_posterior.png")
protected_hashes <- tools::md5sum(c(input, original_figure))
draws <- readRDS(input) |>
  filter(model == "AR(1) log-rate", design == "between")
stopifnot(nrow(draws) == 18000L, !anyDuplicated(draws$.draw))

prob_both <- mean(draws$ratio_pct > 0 & draws$gap_per_100k > 0)
prob_disagree <- mean(draws$ratio_pct > 0 & draws$gap_per_100k <= 0)
stopifnot(abs(prob_both - 0.675722222222222) < 1e-12,
          abs(prob_disagree - 0.298944444444444) < 1e-12)

rate_summary <- draws |>
  select(.draw, black_rate_pct, white_rate_pct) |>
  pivot_longer(-.draw, names_to = "estimand", values_to = "estimate") |>
  group_by(estimand) |>
  summarise(
    median = median(estimate),
    lower_50 = quantile(estimate, 0.25), upper_50 = quantile(estimate, 0.75),
    lower_95 = quantile(estimate, 0.025), upper_95 = quantile(estimate, 0.975),
    .groups = "drop"
  ) |>
  mutate(rate = factor(estimand,
    levels = c("white_rate_pct", "black_rate_pct"),
    labels = c("White rate", "Black rate")))

saved <- read.csv(file.path(results_dir, "r1_posterior_summary.csv")) |>
  filter(model == "AR(1) log-rate", design == "between")
verified <- inner_join(rate_summary, saved, by = "estimand", suffix = c("", "_saved"))
stopifnot(nrow(verified) == 2,
          max(abs(verified$median - verified$median_saved)) < 1e-10,
          max(abs(verified$lower_95 - verified$lower_95_saved)) < 1e-10,
          max(abs(verified$upper_95 - verified$upper_95_saved)) < 1e-10)

font <- "Times New Roman"
ink <- "#000000"
blue <- "#5B87A5"
cloud_gray <- "#999999"
theme_print <- function() {
  theme_classic(base_family = font, base_size = 9.5) +
    theme(
      text = element_text(color = ink),
      axis.text = element_text(color = ink, size = 8.7),
      axis.title = element_text(color = ink, size = 9.5),
      axis.title.x = element_text(margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 5)),
      axis.line = element_line(color = ink, linewidth = 0.3),
      axis.ticks = element_line(color = ink, linewidth = 0.3),
      axis.ticks.length = grid::unit(2.5, "pt"),
      plot.title = element_text(color = ink, face = "bold", size = 10,
                                 margin = margin(b = 3)),
      plot.subtitle = element_text(color = ink, size = 8.8, margin = margin(b = 7)),
      plot.caption = element_text(color = ink),
      legend.text = element_text(color = ink),
      legend.title = element_text(color = ink),
      strip.text = element_text(color = ink),
      plot.tag = element_text(color = ink),
      plot.margin = margin(7, 7, 5, 7),
      legend.position = "none"
    )
}

quadrant_note <- function(probability, second_line, top = TRUE) {
  labels <- c("Ratio increases", second_line, sprintf("%.1f%%", 100 * probability))
  y <- if (top) {
    grid::unit(1, "npc") - grid::unit(c(5, 10, 16), "mm")
  } else {
    grid::unit(c(19, 14, 8), "mm")
  }
  grobs <- lapply(seq_along(labels), function(i) grid::textGrob(
    labels[i], x = grid::unit(1, "npc") - grid::unit(1.5, "mm"), y = y[i],
    just = c("right", "centre"),
    gp = grid::gpar(col = ink, fontfamily = font, fontsize = if (i == 3) 9.7 else 9,
                    fontface = if (i == 3) "bold" else "plain")
  ))
  annotation_custom(do.call(grid::grobTree, grobs))
}

p_joint <- ggplot(draws, aes(ratio_pct, gap_per_100k)) +
  geom_hline(yintercept = 0, color = ink, linewidth = 0.35, linetype = "22") +
  geom_vline(xintercept = 0, color = ink, linewidth = 0.35, linetype = "22") +
  geom_point(color = cloud_gray, size = 0.65, alpha = 0.06) +
  stat_density_2d(aes(color = after_stat(level)), bins = 5, linewidth = 0.36) +
  scale_color_gradient(low = "#B7B7B7", high = "#858585", guide = "none") +
  quadrant_note(prob_both, "Gap increases", top = TRUE) +
  quadrant_note(prob_disagree, "Gap decreases", top = FALSE) +
  scale_x_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  labs(
    title = "A  Joint posterior for the disparity measures",
    subtitle = "One-SD higher long-run democracy, between states",
    x = "Change in Black/White ratio",
    y = "Change in Black-White gap per 100,000"
  ) + theme_print()

p_rates <- ggplot(rate_summary, aes(median, rate, color = rate)) +
  geom_vline(xintercept = 0, color = ink, linewidth = 0.35, linetype = "22") +
  geom_segment(aes(x = lower_95, xend = upper_95, yend = rate), linewidth = 0.7) +
  geom_segment(aes(x = lower_50, xend = upper_50, yend = rate), linewidth = 1.8) +
  geom_point(aes(shape = rate), size = 2.3) +
  scale_color_manual(values = c("Black rate" = ink, "White rate" = blue)) +
  scale_shape_manual(values = c("Black rate" = 16, "White rate" = 15)) +
  scale_x_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
  labs(
    title = "B  Underlying group-specific rates",
    subtitle = "Median, 50% interval, and 95% interval",
    x = "Rate change", y = NULL
  ) + theme_print() + theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

# Color changes must leave the density-contour coordinates unchanged.
reference_density <- ggplot_build(
  ggplot(draws, aes(ratio_pct, gap_per_100k)) + stat_density_2d(bins = 5)
)$data[[1]]
current_density <- ggplot_build(p_joint)$data[[4]]
stopifnot(isTRUE(all.equal(reference_density[c("x", "y", "level", "group")],
                           current_density[c("x", "y", "level", "group")],
                           check.attributes = FALSE)))

# Inspect rendered text grobs, including axes and the fixed-position annotations.
text_colors <- function(grob) {
  colors <- if (inherits(grob, "text")) grob$gp$col else character()
  for (child in c(grob$grobs, as.list(grob$children))) {
    colors <- c(colors, text_colors(child))
  }
  colors
}
validation_file <- tempfile(fileext = ".pdf")
grDevices::quartz(type = "pdf", file = validation_file, family = font,
                  width = 7.25, height = 3.75)
rendered_colors <- c(text_colors(ggplotGrob(p_joint)), text_colors(ggplotGrob(p_rates)))
stopifnot(length(rendered_colors) >= 15, all(grDevices::col2rgb(rendered_colors) == 0))
invisible(dev.off())
unlink(validation_file)

figure <- p_joint + p_rates + plot_layout(widths = c(1.65, 1))
stem <- file.path(figures_dir, "figure_2_joint_posterior_high_contrast")
pdf_device <- function(filename, width, height, bg, ...) {
  grDevices::quartz(type = "pdf", file = filename, width = width, height = height,
                    family = font, bg = bg, title = "Joint posterior for racial imprisonment disparities")
}
ggsave(paste0(stem, ".pdf"), figure, width = 7.25, height = 3.75,
       device = pdf_device, bg = "white")
ggsave(paste0(stem, ".png"), figure, width = 7.25, height = 3.75,
       dpi = 600, device = ragg::agg_png, bg = "white")
ggsave(paste0(stem, ".tif"), figure, width = 7.25, height = 3.75,
       dpi = 600, device = ragg::agg_tiff, compression = "lzw", bg = "white")
ggsave(paste0(stem, "_preview.png"), figure, width = 7.25, height = 3.75,
       dpi = 200, device = ragg::agg_png, bg = "white")
stopifnot(identical(tools::md5sum(c(input, original_figure)), protected_hashes))
write.csv(rate_summary, file.path(results_dir, "r1_figure2_high_contrast_check.csv"), row.names = FALSE)
message("High-contrast Figure 2 exported; posterior summaries and all rendered text colors verified; original files unchanged.")
