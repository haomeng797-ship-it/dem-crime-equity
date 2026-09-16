# Compare local typefaces using manuscript labels at final physical sizes.
suppressPackageStartupMessages({library(ggplot2); library(patchwork)})
families <- c("Helvetica Neue", "Optima", "Avenir Book")
titles <- c("Helvetica Neue Medium", "Optima", "Avenir Medium")
sample_panel <- function(family, heading, name) {
  frame <- data.frame(y = 1:3, median = c(12.7, 15.4, 8.2),
                      lower = c(-0.03, 3.5, -5.2), upper = c(26.2, 29.1, 22.9))
  ggplot(frame, aes(median, y)) +
    geom_vline(xintercept = 0, color = "#737B80", linetype = "22", linewidth = .3) +
    geom_segment(aes(x = lower, xend = upper, yend = y), color = "#9B5361", linewidth = .6) +
    geom_point(color = "#9B5361", size = 2) +
    scale_y_continuous(breaks = 1:3, labels = c("AR(1) log-rate", "Negative-binomial count", "Recording-screen sample")) +
    labs(title = "A  Relative disparity", subtitle = "Median and 95% credible interval",
      x = "Change in Black/White ratio (%)", y = NULL, caption = name) +
    theme_minimal(base_family = family, base_size = 8.6) +
    theme(text = element_text(color = "black"), axis.text = element_text(color = "black", size = 8),
      panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#E4E8EB", linewidth = .25),
      plot.title = element_text(family = heading, face = "plain", size = 10),
      plot.subtitle = element_text(size = 8), plot.margin = margin(8,12,8,12))
}
panels <- lapply(seq_along(families), function(i) sample_panel(families[i], titles[i], families[i]))
ggsave("/private/tmp/spp_font_comparison.png", wrap_plots(panels, ncol = 1),
  width = 6.4, height = 8.4, dpi = 300, device = ragg::agg_png, bg = "white")
