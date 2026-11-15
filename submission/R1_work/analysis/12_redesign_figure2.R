# A unified publication layout for the existing joint posterior and rate contrasts.
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
inputs <- file.path(results_dir, c("r1_posterior_draws.rds", "r1_posterior_summary.csv"))
input_hash <- tools::md5sum(inputs)
draws <- readRDS(inputs[1]) |>
  filter(model == "AR(1) log-rate", design == "between")
stopifnot(nrow(draws) == 18000L, !anyDuplicated(draws$.draw))
prob_both <- mean(draws$ratio_pct > 0 & draws$gap_per_100k > 0)
prob_disagree <- mean(draws$ratio_pct > 0 & draws$gap_per_100k <= 0)
stopifnot(abs(prob_both - 0.675722222222222) < 1e-12,
          abs(prob_disagree - 0.298944444444444) < 1e-12)
rates <- draws |>
  select(black_rate_pct, white_rate_pct) |>
  pivot_longer(everything(), names_to = "estimand", values_to = "estimate") |>
  group_by(estimand) |>
  summarise(median = median(estimate),
            lower_50 = quantile(estimate, 0.25), upper_50 = quantile(estimate, 0.75),
            lower_95 = quantile(estimate, 0.025), upper_95 = quantile(estimate, 0.975),
            .groups = "drop") |>
  mutate(group = if_else(estimand == "black_rate_pct", "Black rate", "White rate"),
         y = if_else(estimand == "black_rate_pct", 2, 1),
         estimate_label = sprintf("%.1f%%  [%.1f, %.1f]", median, lower_95, upper_95))
saved <- read.csv(inputs[2]) |>
  filter(model == "AR(1) log-rate", design == "between")
check <- inner_join(rates, saved, by = "estimand", suffix = c("", "_saved"))
stopifnot(nrow(check) == 2,
          max(abs(check$median - check$median_saved)) < 1e-10,
          max(abs(check$lower_95 - check$lower_95_saved)) < 1e-10,
          max(abs(check$upper_95 - check$upper_95_saved)) < 1e-10)

source(file.path(script_dir, "figure_style.R"))
font <- font_regular
heading_font <- font_heading
ink <- color_ink
blue <- color_white
theme_publication <- function() {
  theme_classic(base_family = font, base_size = 8.6) +
    theme(
      text = element_text(color = ink),
      axis.text = element_text(color = ink, size = 8),
      axis.title = element_text(color = ink, size = 8.6),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
      axis.line = element_line(color = "#303A41", linewidth = 0.2),
      axis.ticks = element_line(color = "#303A41", linewidth = 0.2),
      axis.ticks.length = grid::unit(2, "pt"),
      plot.title = element_text(family = heading_font, face = "plain", color = ink,
                                 size = font_heading_size, margin = margin(b = 4)),
      plot.subtitle = element_text(color = ink, size = 8, margin = margin(b = 9)),
      plot.caption = element_text(color = ink),
      legend.position = "none",
      plot.margin = margin(7, 7, 5, 7)
    )
}

probability_note <- function(probability, gap_text, top) {
  label <- sprintf("Ratio increases, %s\n%.1f%% of draws", gap_text, 100 * probability)
  annotation_custom(grid::textGrob(label,
    x = grid::unit(1, "npc") - grid::unit(2, "mm"),
    y = if (top) grid::unit(1, "npc") - grid::unit(2, "mm") else grid::unit(2, "mm"),
    just = c("right", if (top) "top" else "bottom"),
    gp = grid::gpar(fontfamily = font, col = ink, fontsize = 8, lineheight = 1.15)))
}

# Show a fixed random subset of the draws so single points stay visible; the contours
# and every reported number still use all 18,000 draws.
set.seed(20261115)
shown_draws <- draws[sample.int(nrow(draws), 3000L), ]
p_joint <- ggplot(draws, aes(ratio_pct, gap_per_100k)) +
  geom_point(data = shown_draws, color = "#1A1A1A", size = 1.0, alpha = 0.11, shape = 16) +
  stat_density_2d(color = "#404040", bins = 5, n = 300, linewidth = 0.24, linejoin = "round") +
  geom_hline(yintercept = 0, color = "#69757E", linewidth = 0.3, linetype = "22") +
  geom_vline(xintercept = 0, color = "#69757E", linewidth = 0.3, linetype = "22") +
  probability_note(prob_both, "gap increases", TRUE) +
  probability_note(prob_disagree, "gap does not increase", FALSE) +
  scale_x_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
  scale_y_continuous(breaks = c(-250, 0, 250, 500)) +
  coord_cartesian(xlim = c(-11, 40), ylim = c(-310, 440)) +
  labs(title = "A  Relative and absolute disparity",
       x = "Change in Black/White ratio",
       y = "Change in Black-White gap (per 100,000)") + theme_publication()

rate_draws <- draws |>
  select(black_rate_pct, white_rate_pct) |>
  pivot_longer(everything(), names_to = "estimand", values_to = "estimate") |>
  mutate(group = factor(if_else(estimand == "black_rate_pct", "Black rate", "White rate"),
                        levels = c("White rate", "Black rate")))
rates <- mutate(rates, group = factor(group, levels = levels(rate_draws$group)))
p_rates <- ggplot(rate_draws, aes(x = estimate, y = group, color = group, fill = group)) +
  geom_vline(xintercept = 0, color = "#69757E", linewidth = 0.2, linetype = "22") +
  ggdist::stat_slab(height = 0.62, alpha = 0.22, color = NA, normalize = "groups") +
  ggdist::stat_slab(height = 0.62, fill = NA, linewidth = 0.2, normalize = "groups") +
  geom_segment(data = rates, aes(x = lower_95, xend = upper_95, y = group, yend = group),
               linewidth = 0.4, inherit.aes = FALSE, color = c(color_black, blue)[
                 match(as.character(rates$group), c("Black rate", "White rate"))]) +
  geom_segment(data = rates, aes(x = lower_50, xend = upper_50, y = group, yend = group),
               linewidth = 1.0, inherit.aes = FALSE, color = c(color_black, blue)[
                 match(as.character(rates$group), c("Black rate", "White rate"))]) +
  geom_point(data = rates, aes(x = median, y = group), size = 1.5, shape = 16,
             inherit.aes = FALSE, color = c(color_black, blue)[
               match(as.character(rates$group), c("Black rate", "White rate"))]) +
  geom_text(data = rates, aes(x = median, y = as.numeric(group) - 0.17,
            label = estimate_label), inherit.aes = FALSE,
            color = ink, family = font, size = 8 / .pt) +
  scale_color_manual(values = c("Black rate" = color_black, "White rate" = blue)) +
  scale_fill_manual(values = c("Black rate" = color_black, "White rate" = blue)) +
  scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20),
                     labels = scales::label_number(suffix = "%", accuracy = 1)) +
  scale_y_discrete(expand = expansion(add = c(0.42, 0.72))) +
  coord_cartesian(xlim = c(-32, 20)) +
  labs(title = "B  Group-specific imprisonment rates", x = "Rate change", y = NULL) +
  theme_publication() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

# Re-styling must preserve the existing kernel-density contour geometry.
reference_density <- ggplot_build(
  ggplot(draws, aes(ratio_pct, gap_per_100k)) + stat_density_2d(bins = 5, n = 300)
)$data[[1]]
current_density <- ggplot_build(p_joint)$data[[2]]
stopifnot(isTRUE(all.equal(reference_density[c("x", "y", "level", "group")],
                           current_density[c("x", "y", "level", "group")],
                           check.attributes = FALSE)))

figure <- (p_joint | p_rates) + plot_layout(widths = c(1.3, 1)) + plot_annotation(
  theme = theme(plot.margin = margin(3, 5, 5, 5))
)

stem <- file.path(figures_dir, "figure_2_joint_posterior")
pdf_device <- function(filename, width, height, bg, ...) {
  grDevices::quartz(type = "pdf", file = filename, width = width, height = height,
                    family = font, bg = bg, title = "Joint posterior and group-specific imprisonment rates")
}
ggsave(paste0(stem, ".pdf"), figure, width = 7.25, height = 3.7,
       device = pdf_device, bg = "white")
ggsave(paste0(stem, ".png"), figure, width = 7.25, height = 3.7,
       dpi = 600, device = ragg::agg_png, bg = "white")
ggsave(paste0(stem, ".tif"), figure, width = 7.25, height = 3.7,
       dpi = 600, device = ragg::agg_tiff, compression = "lzw", bg = "white")
ggsave(paste0(stem, "_preview.png"), figure, width = 7.25, height = 3.7,
       dpi = 400, device = ragg::agg_png, bg = "white")
stopifnot(identical(tools::md5sum(inputs), input_hash))
write.csv(rates, file.path(results_dir, "r1_figure2_redesign_check.csv"), row.names = FALSE)
message("Figure 2 redesigned; probabilities, intervals, contour coordinates, and unchanged inputs verified.")
