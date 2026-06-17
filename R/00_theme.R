# 00_theme.R -- shared academic figure style, sourced by the plotting scripts (02-06).
# Goal: a restrained, journal-style look (Okabe-Ito colorblind-safe palette, a clean academic
# serif, thin axes, light gridlines, left-aligned titles, 300 dpi).

suppressPackageStartupMessages({ library(ggplot2) })

FONT <- Sys.getenv("FIG_FONT", "Charter")   # web figures use Charter; set FIG_FONT="Times New Roman" for the PDF figure set

# Okabe-Ito palette (the de-facto standard for accessible academic figures)
# Muted "editorial / jewel-tone" palette: deep ink-blue, terracotta, muted teal, plum, antique gold.
# Lower saturation and a touch darker than primary palettes, for a more restrained, high-end look.
PAL <- c(navy = "#21425A", rust = "#B0543B", teal = "#3E7C71", plum = "#6F4A68", gold = "#C29A47")

# consistent assignments across figures
# region scatter needs HUE separation (points overlap), so four distinct-but-muted hues:
# steel blue, muted emerald, antique gold, dusty violet.
region_pal <- c("Midwest" = "#2F6FA8", "Northeast" = "#3F8A5E",
                "South"   = "#C49A33", "West"      = "#7E5AA3")
wb_pal <- c("Between-state (cross-sectional)" = "#B0543B",
            "Within-state (over time)"        = "#21425A")
class_pal <- c("1" = "#21425A", "2" = "#B0543B", "3" = "#3E7C71",
               "4" = "#6F4A68", "5" = "#C29A47")

theme_poli <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = FONT) +
    theme(
      text             = element_text(color = "grey15"),
      # title: darkest + bold + largest, so it stays clearly distinct from the secondary text
      plot.title       = element_text(face = "bold", size = rel(1.18), color = "grey10",
                                      margin = margin(b = 2)),
      # secondary text: darkened and enlarged for legibility, but kept lighter than the title
      plot.subtitle    = element_text(color = "grey30", size = rel(0.95), margin = margin(b = 10)),
      plot.caption     = element_text(color = "grey30", size = rel(0.82), hjust = 0,
                                      margin = margin(t = 10)),
      axis.title       = element_text(color = "grey20", size = rel(0.95)),
      axis.title.x     = element_text(margin = margin(t = 6)),
      axis.title.y     = element_text(margin = margin(r = 6)),
      axis.text        = element_text(color = "grey25", size = rel(0.9)),
      axis.line        = element_line(color = "#8A817A", linewidth = 0.3),
      axis.ticks       = element_line(color = "#8A817A", linewidth = 0.3),
      panel.grid.major = element_line(color = "#E7E2DB", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      panel.spacing    = unit(1.1, "lines"),
      strip.text       = element_text(face = "bold", color = "grey15", size = rel(1.0)),
      legend.position  = "top",
      legend.title     = element_text(color = "grey20"),
      legend.text      = element_text(color = "grey20"),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin      = margin(12, 14, 10, 12)
    )
}

theme_set(theme_poli())

# high-resolution save via the ragg device (crisp text, system fonts)
save_fig <- function(plot, file, width = 8, height = 5) {
  outdir <- Sys.getenv("FIG_OUTDIR", "")           # default "" -> save to figures/ as written
  if (nzchar(outdir)) file <- sub("/figures/", paste0("/", outdir, "/"), file, fixed = TRUE)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  ggsave(file, plot, width = width, height = height, dpi = 300,
         device = ragg::agg_png, bg = "white")
}
