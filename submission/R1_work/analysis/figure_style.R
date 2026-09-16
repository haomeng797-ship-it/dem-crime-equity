# Shared typography, colors, and export settings for the R1 figure set.
font_regular <- "Optima"
font_heading <- "Optima"
font_heading_size <- 10
font_emphasis <- font_heading
color_ink <- "#000000"
color_ratio <- "#9B5361"
color_gap <- "#487B70"
# Figure 2 keeps its accepted black/blue encoding.
color_black <- "#171717"
color_white <- "#0072B2"
color_diagnostic_black <- "#8C6270"
color_diagnostic_white <- "#648276"
color_mid <- "#737B80"
color_grid <- "#E4E8EB"

theme_r1 <- function(base_size = 8.6) {
  ggplot2::theme_minimal(base_family = font_regular, base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(color = color_ink),
      axis.title = ggplot2::element_text(size = base_size, color = color_ink),
      axis.text = ggplot2::element_text(size = 8, color = color_ink),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 6)),
      axis.ticks = ggplot2::element_line(color = "#303A41", linewidth = 0.28),
      axis.ticks.length = grid::unit(2, "pt"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = color_grid, linewidth = 0.25),
      plot.title = ggplot2::element_text(family = font_heading, size = font_heading_size,
        face = "plain", margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(size = 8, color = color_ink,
        margin = ggplot2::margin(b = 9)),
      plot.caption = ggplot2::element_text(size = 7.7, color = color_ink),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 8, color = color_ink),
      legend.position = "top",
      plot.margin = ggplot2::margin(7, 7, 5, 7)
    )
}

save_figure <- function(plot, stem, width, height) {
  pdf_device <- function(filename, width, height, bg, ...) {
    grDevices::quartz(type = "pdf", file = filename, width = width, height = height,
      family = font_regular, bg = bg, title = "Racial imprisonment disparity estimates")
  }
  path <- file.path(figures_dir, stem)
  ggplot2::ggsave(paste0(path, ".pdf"), plot, width = width, height = height,
    device = pdf_device, bg = "white")
  ggplot2::ggsave(paste0(path, ".png"), plot, width = width, height = height,
    dpi = 600, device = ragg::agg_png, bg = "white")
  ggplot2::ggsave(paste0(path, ".tif"), plot, width = width, height = height,
    dpi = 600, device = ragg::agg_tiff, compression = "lzw", bg = "white")
  ggplot2::ggsave(paste0(path, "_preview.png"), plot, width = width, height = height,
    dpi = 400, device = ragg::agg_png, bg = "white")
}
