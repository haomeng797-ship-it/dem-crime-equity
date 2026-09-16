# Align Figure 3 with the accepted Figure 2 typography and palette.
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
input_hash <- unname(tools::md5sum(input))
model_levels <- c("AR(1) log-rate", "Log-rate, no AR(1)", "Region + AR(1)",
                  "Negative-binomial count", "Recording-screen sample")
summaries <- readRDS(input) |>
  filter(design == "between") |>
  mutate(model = factor(model, levels = rev(model_levels))) |>
  select(.draw, model, ratio_pct, gap_per_100k) |>
  pivot_longer(c(ratio_pct, gap_per_100k), names_to = "estimand", values_to = "estimate") |>
  group_by(model, estimand) |>
  summarise(median = median(estimate),
            lower_50 = quantile(estimate, 0.25), upper_50 = quantile(estimate, 0.75),
            lower_95 = quantile(estimate, 0.025), upper_95 = quantile(estimate, 0.975),
            .groups = "drop")
stopifnot(nrow(summaries) == 10, !anyNA(summaries),
          all(summaries$lower_95 <= summaries$lower_50),
          all(summaries$lower_50 <= summaries$median),
          all(summaries$median <= summaries$upper_50),
          all(summaries$upper_50 <= summaries$upper_95))
saved <- read.csv(file.path(results_dir, "r1_posterior_summary.csv")) |>
  filter(design == "between", estimand %in% c("ratio_pct", "gap_per_100k"))
check <- inner_join(summaries, saved, by = c("model", "estimand"), suffix = c("", "_saved"))
stopifnot(nrow(check) == 10,
          max(abs(check$median - check$median_saved)) < 1e-10,
          max(abs(check$lower_95 - check$lower_95_saved)) < 1e-10,
          max(abs(check$upper_95 - check$upper_95_saved)) < 1e-10)

source(file.path(script_dir, "figure_style.R"))
font <- font_regular
heading_font <- font_heading
panel <- function(estimand_name, title, x_label, color, show_y) {
  ggplot(filter(summaries, estimand == estimand_name), aes(median, model)) +
    geom_vline(xintercept = 0, color = "#69757E", linewidth = 0.3, linetype = "22") +
    geom_segment(aes(x = lower_95, xend = upper_95, yend = model),
                 color = color, linewidth = 0.6) +
    geom_segment(aes(x = lower_50, xend = upper_50, yend = model),
                 color = color, linewidth = 1.35) +
    geom_point(shape = 21, size = 2.1, stroke = 0.3, color = "white", fill = color) +
    labs(title = title, x = x_label, y = NULL) +
    theme_minimal(base_family = font, base_size = 8.6) +
    theme(
      text = element_text(color = "#000000"),
      axis.text = element_text(color = "#000000", size = 8),
      axis.title = element_text(color = "#000000", size = 8.6),
      axis.title.x = element_text(margin = margin(t = 7)),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(color = "#E4E8EB", linewidth = 0.25),
      axis.ticks.x = element_line(color = "#303A41", linewidth = 0.28),
      axis.ticks.length = grid::unit(2, "pt"),
      axis.text.y = if (show_y) element_text(color = "#000000", size = 8) else element_blank(),
      plot.title = element_text(family = heading_font, color = "#000000", face = "plain",
                                 size = font_heading_size, margin = margin(b = 9)),
      plot.margin = margin(7, 7, 5, 7),
      legend.position = "none"
    )
}

figure <- panel("ratio_pct", "A  Relative disparity", "Change in Black/White ratio (%)",
                 color_ratio, TRUE) +
  panel("gap_per_100k", "B  Absolute disparity", "Change in gap per 100,000",
        color_gap, FALSE) + plot_layout(widths = c(1.15, 1))

stem <- file.path(figures_dir, "figure_3_robustness_publication")
pdf_device <- function(filename, width, height, bg, ...) {
  grDevices::quartz(type = "pdf", file = filename, width = width, height = height,
                    family = font, bg = bg, title = "Sensitivity of racial imprisonment disparity estimates")
}
ggsave(paste0(stem, ".pdf"), figure, width = 7.25, height = 3.45,
       device = pdf_device, bg = "white")
ggsave(paste0(stem, ".png"), figure, width = 7.25, height = 3.45,
       dpi = 600, device = ragg::agg_png, bg = "white")
ggsave(paste0(stem, ".tif"), figure, width = 7.25, height = 3.45,
       dpi = 600, device = ragg::agg_tiff, compression = "lzw", bg = "white")
ggsave(paste0(stem, "_preview.png"), figure, width = 7.25, height = 3.45,
       dpi = 400, device = ragg::agg_png, bg = "white")
stopifnot(identical(unname(tools::md5sum(input)), input_hash))
message("Figure 3 styled; all ten medians and 95% intervals match the saved results.")
