# Retain the two-panel geometry while making state correspondence explicit.
suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
work_root <- normalizePath(file.path(script_dir, ".."))
input <- file.path(work_root, "results", "r1_state_measurement_summary.csv")
input_hash <- unname(tools::md5sum(input))
states <- read.csv(input)
stopifnot(nrow(states) == 50, !anyDuplicated(states$state_abbr))
stopifnot(all(states$white_rate >= 0 & states$white_rate <= 900),
          all(states$black_rate >= 0 & states$black_rate <= 4250))
stopifnot(max(abs(states$ratio - states$black_rate / states$white_rate)) < 1e-10,
          max(abs(states$gap - states$black_rate + states$white_rate)) < 1e-8)

examples <- states[match(c("MD", "SD", "MA", "NH"), states$state_abbr), ]
examples$pair <- c("ratio", "ratio", "gap", "gap")
examples$label_x <- c(18, 485, 18, 375)
examples$label_y <- c(1450, 2790, 365, 365)
examples$leader_x <- c(120, 525, 115, 450)
examples$leader_y <- c(1250, 2540, 540, 540)
examples$ratio_label <- sprintf("%s\n%.2f times", examples$state_name, examples$ratio)
examples$gap_label <- paste0(examples$state_name, "\n",
                             format(round(examples$gap), big.mark = ",", trim = TRUE), " more")

font_regular <- "Avenir Next"
font_heading <- "Avenir Next Medium"
colors <- c(ratio = "#B2473E", gap = "#007A78")
ink <- "#202428"
muted <- "#656C72"
guides <- "#D8DDE0"
ratio_guides <- data.frame(
  slope = c(3, 5, 7, 9),
  x = c(825, 735, 515, 310)
)
ratio_guides$y <- ratio_guides$slope * ratio_guides$x
ratio_guides$label <- paste(ratio_guides$slope, "times")
gap_guides <- data.frame(gap = seq(500, 2500, 500), x = 785)
gap_guides$y <- gap_guides$x + gap_guides$gap
gap_guides$label <- paste(format(gap_guides$gap, big.mark = ",", trim = TRUE), "more")

base <- function() {
  ggplot(states, aes(white_rate, black_rate)) +
    scale_x_continuous(breaks = c(0, 200, 400, 600, 800),
                       labels = scales::label_comma(), expand = expansion(mult = 0)) +
    scale_y_continuous(breaks = seq(0, 4000, 1000),
                       labels = scales::label_comma(), expand = expansion(mult = 0)) +
    coord_cartesian(xlim = c(0, 900), ylim = c(0, 4250), expand = FALSE) +
    labs(x = "White imprisonment rate", y = "Black imprisonment rate") +
    theme_classic(base_family = font_regular, base_size = 8.5) +
    theme(
      text = element_text(color = ink),
      axis.text = element_text(color = muted, size = 8),
      axis.line = element_line(color = "#AEB5BA", linewidth = 0.3),
      axis.ticks = element_line(color = "#AEB5BA", linewidth = 0.3),
      axis.ticks.length = grid::unit(2, "pt"),
      axis.title = element_text(size = 8.5),
      axis.title.x = element_text(margin = margin(t = 5)),
      plot.title = element_text(family = font_heading, face = "plain", size = 10),
      plot.subtitle = element_text(size = 8.4, color = muted, margin = margin(b = 8)),
      plot.margin = margin(5, 6, 4, 5),
      legend.position = "none"
    )
}

points_and_labels <- function(label_field) {
  list(
    geom_point(color = "#6D7478", size = 1.65, alpha = 0.75),
    geom_segment(data = examples,
                 aes(xend = leader_x, yend = leader_y, color = pair),
                 linewidth = 0.35, show.legend = FALSE),
    geom_point(data = examples, aes(fill = pair, shape = pair),
               color = "white", size = 3.1, stroke = 0.5),
    geom_label(data = examples,
               aes(x = label_x, y = label_y, label = .data[[label_field]], color = pair),
               family = font_regular, size = 2.55, hjust = 0, lineheight = 1.1,
               fill = "white", linewidth = 0, label.padding = grid::unit(0.1, "lines")),
    scale_color_manual(values = colors),
    scale_fill_manual(values = colors),
    scale_shape_manual(values = c(ratio = 21, gap = 24))
  )
}

left <- base() +
  geom_abline(data = ratio_guides, aes(slope = slope, intercept = 0),
               color = guides, linewidth = 0.6) +
  geom_label(data = ratio_guides, aes(x = x, y = y, label = label),
             inherit.aes = FALSE, color = muted, fill = "white", linewidth = 0,
             size = 2.5, family = font_regular, hjust = 0.5, vjust = 0.5,
             label.padding = grid::unit(0.08, "lines")) +
  points_and_labels("ratio_label") +
  labs(title = "A  Relative disparity (ratio)",
       subtitle = "How many times the White rate?")

right <- base() +
  geom_abline(data = gap_guides, aes(slope = 1, intercept = gap),
               color = guides, linewidth = 0.6) +
  geom_label(data = gap_guides, aes(x = x, y = y, label = label),
             inherit.aes = FALSE, color = muted, fill = "white", linewidth = 0,
             size = 2.5, family = font_regular, hjust = 0.5, vjust = 0.5,
             label.padding = grid::unit(0.08, "lines")) +
  points_and_labels("gap_label") +
  labs(title = "B  Absolute disparity (gap)",
       subtitle = "How much higher than the White rate?")

figure <- (left | right) + plot_annotation(
  title = "Same 50 states and positions in both panels",
  subtitle = "2016-2020 averages; the same four example states are labeled in each panel",
  caption = paste(
    "Rates and gaps: per 100,000 residents aged 15-64. Gray lines mark equal ratios (A) or equal gaps (B).",
    "Colored labels report each example state's value on that panel's measure. Lines are reference values, not fitted trends.",
    sep = "\n"
  ),
  theme = theme(
    plot.title = element_text(family = font_heading, face = "plain", size = 11, color = ink),
    plot.subtitle = element_text(family = font_regular, size = 8.5, color = muted,
                                 margin = margin(b = 8)),
    plot.caption = element_text(family = font_regular, size = 7.4, color = muted,
                                hjust = 0, lineheight = 1.25, margin = margin(t = 7)),
    plot.margin = margin(7, 7, 7, 7)
  )
)

stem <- file.path(work_root, "figures", "figure_1_geometry_reader")
pdf_device <- function(filename, width, height, bg, ...) {
  grDevices::quartz(type = "pdf", file = filename, width = width, height = height,
                    family = font_regular, bg = bg,
                    title = "Relative and absolute racial imprisonment disparities")
}
ggsave(paste0(stem, ".pdf"), figure, width = 7.25, height = 4.6,
       device = pdf_device, bg = "white")
ggsave(paste0(stem, ".png"), figure, width = 7.25, height = 4.6,
       dpi = 600, device = ragg::agg_png, bg = "white")
ggsave(paste0(stem, ".tif"), figure, width = 7.25, height = 4.6,
       dpi = 600, device = ragg::agg_tiff, compression = "lzw", bg = "white")
ggsave(paste0(stem, "_preview.png"), figure, width = 7.25, height = 4.6,
       dpi = 200, device = ragg::agg_png, bg = "white")

stopifnot(identical(unname(tools::md5sum(input)), input_hash))
write.csv(examples, file.path(work_root, "results", "r1_geometry_reader_labels.csv"),
          row.names = FALSE)
message("Geometry figure exported; both panels use identical states, coordinates, and example identities.")
