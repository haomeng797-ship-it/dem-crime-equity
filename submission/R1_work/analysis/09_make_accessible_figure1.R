# Policy-facing Figure 1, using existing descriptive summaries only.
suppressPackageStartupMessages(library(grid))

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
work_root <- normalizePath(file.path(script_dir, ".."))
results_dir <- file.path(work_root, "results")
figures_dir <- file.path(work_root, "figures")
input_file <- file.path(results_dir, "r1_state_measurement_summary.csv")
input_hash <- unname(tools::md5sum(input_file))
states <- read.csv(input_file)

stopifnot(nrow(states) == 50L, !anyDuplicated(states$state_abbr))
stopifnot(all(is.finite(states$black_rate)), all(states$white_rate > 0))
stopifnot(max(abs(states$ratio - states$black_rate / states$white_rate)) < 1e-10)
stopifnot(max(abs(states$gap - (states$black_rate - states$white_rate))) < 1e-8)
stopifnot(all(states$rank_ratio == rank(-states$ratio, ties.method = "min")))
stopifnot(all(states$rank_gap == rank(-states$gap, ties.method = "min")))

example_ids <- c("MD", "SD", "MA", "NH")
examples <- states[match(example_ids, states$state_abbr), ]
stopifnot(identical(examples$state_abbr, example_ids))
rank_change <- abs(states$rank_ratio - states$rank_gap)
bins <- cut(rank_change, breaks = seq(0, 35, 5), right = FALSE)
bin_counts <- as.integer(table(bins))
stopifnot(!anyNA(bins), sum(bin_counts) == 50L)
moving_ten <- sum(rank_change >= 10)
median_move <- median(rank_change)
top_ten_overlap <- sum(states$rank_ratio <= 10 & states$rank_gap <= 10)
stopifnot(moving_ten == 27L, median_move == 10.5, top_ten_overlap == 5L)

font_regular <- "Avenir Next"
font_heading <- "Avenir Next Medium"
ink <- "#202428"
muted <- "#626970"
rule <- "#DDE1E4"
black_group <- "#2E3338"
white_group <- "#5B7F9B"
rank_accent <- "#007A78"
number <- function(x) format(round(x), big.mark = ",", trim = TRUE, scientific = FALSE)

# Explicit page coordinates keep the rate axis separate from the summary columns.
draw_figure <- function() {
  grid.newpage()
  text <- function(label, x, y, size = 9, color = ink, just = "left", heading = FALSE) {
    label_grob <- textGrob(label, x, y, just = just, gp = gpar(
      fontfamily = if (heading) font_heading else font_regular,
      fontsize = size, col = color
    ))
    label_width <- convertWidth(grobWidth(label_grob), "npc", valueOnly = TRUE)
    label_height <- convertHeight(grobHeight(label_grob), "npc", valueOnly = TRUE)
    left <- x - switch(just, left = 0, centre = label_width / 2, right = label_width)
    stopifnot(left >= 0, left + label_width <= 1,
              y - label_height / 2 >= 0, y + label_height / 2 <= 1)
    grid.draw(label_grob)
  }
  line <- function(x0, y0, x1, y1, color = rule, width = 0.55) {
    grid.segments(x0, y0, x1, y1, gp = gpar(col = color, lwd = width))
  }
  point <- function(x, y, group) {
    grid.points(unit(x, "npc"), unit(y, "npc"), pch = if (group == "Black") 16 else 21,
                size = unit(2, "mm"), gp = gpar(
                  col = if (group == "Black") black_group else white_group,
                  fill = "white", lwd = 1
                ))
  }

  text("A  Similar ratios or gaps can accompany different imprisonment rates",
       0.025, 0.967, size = 10.3, heading = TRUE)
  text("Selected state averages, 2016-2020", 0.025, 0.934, size = 8.7, color = muted)

  rate_left <- 0.265
  rate_right <- 0.675
  rate_x <- function(value) rate_left + value / 2500 * (rate_right - rate_left)
  ratio_x <- 0.775
  gap_x <- 0.932
  text("Group-specific rates", rate_left, 0.894, heading = TRUE)
  point(rate_left + 0.009, 0.864, "White")
  text("White", rate_left + 0.025, 0.864, size = 8.3)
  point(rate_left + 0.135, 0.864, "Black")
  text("Black", rate_left + 0.151, 0.864, size = 8.3)
  text("Ratio", ratio_x, 0.894, just = "centre", heading = TRUE)
  text("Black / White", ratio_x, 0.864, just = "centre", size = 8, color = muted)
  text("Gap", gap_x, 0.894, just = "centre", heading = TRUE)
  text("Black - White", gap_x, 0.864, just = "centre", size = 8, color = muted)

  ticks <- seq(0, 2500, 500)
  for (tick in ticks) {
    line(rate_x(tick), 0.511, rate_x(tick), 0.825, width = 0.45)
    text(number(tick), rate_x(tick), 0.492, just = "centre", size = 7.9, color = muted)
  }
  text("Similar ratios", 0.025, 0.837, size = 8.2, color = muted)
  text("Similar gaps", 0.025, 0.650, size = 8.2, color = muted)
  ys <- c(0.791, 0.720, 0.605, 0.534)
  for (i in seq_len(nrow(examples))) {
    row <- examples[i, ]
    y <- ys[i]
    text(row$state_name, 0.025, y, size = 9.1)
    line(rate_x(row$white_rate), y, rate_x(row$black_rate), y,
         color = "#AAB3BA", width = 1.15)
    point(rate_x(row$white_rate), y, "White")
    point(rate_x(row$black_rate), y, "Black")
    text(number(row$white_rate), rate_x(row$white_rate), y + 0.023,
         just = "centre", size = 8.1, color = white_group)
    text(number(row$black_rate), rate_x(row$black_rate), y + 0.023,
         just = "centre", size = 8.1, color = black_group)
    text(sprintf("%.2f", row$ratio), ratio_x, y, just = "centre", size = 9.4)
    text(number(row$gap), gap_x, y, just = "centre", size = 9.4)
  }
  text("Rates and gaps per 100,000 (ages 15-64). Summaries use unrounded rates.",
       0.025, 0.458, size = 8, color = muted)
  line(0.025, 0.432, 0.975, 0.432)

  text("B  Changing the measure changes state rankings", 0.025, 0.403,
       size = 10.3, heading = TRUE)
  text("All 50 states, 2016-2020; rank 1 = largest disparity", 0.025, 0.371,
       size = 8.7, color = muted)

  chart_left <- 0.080
  chart_right <- 0.657
  chart_bottom <- 0.109
  chart_top <- 0.308
  count_y <- function(n) chart_bottom + n / 15 * (chart_top - chart_bottom)
  text("Number of states", chart_left, 0.330, size = 8.2, color = muted)
  for (tick in c(0, 5, 10, 15)) {
    line(chart_left, count_y(tick), chart_right, count_y(tick), width = 0.45)
    text(as.character(tick), chart_left - 0.017, count_y(tick),
         just = "right", size = 7.9, color = muted)
  }
  bar_step <- (chart_right - chart_left) / length(bin_counts)
  centers <- chart_left + (seq_along(bin_counts) - 0.5) * bar_step
  for (i in seq_along(bin_counts)) {
    height <- count_y(bin_counts[i]) - chart_bottom
    grid.rect(x = centers[i], y = chart_bottom + height / 2,
              width = bar_step * 0.72, height = height,
              gp = gpar(fill = if (i <= 2) "#B2BBC2" else rank_accent, col = NA))
    text(as.character(bin_counts[i]), centers[i], count_y(bin_counts[i]) + 0.014,
         just = "centre", size = 8.3)
    text(paste0((i - 1) * 5, "-", i * 5 - 1), centers[i], chart_bottom - 0.025,
         just = "centre", size = 7.9)
  }
  text("Places moved when switching from ratio to gap",
       (chart_left + chart_right) / 2, 0.049, just = "centre", size = 8.6)

  text(sprintf("%d of 50 states", moving_ten), 0.714, 0.293,
       size = 11, color = rank_accent, heading = TRUE)
  text("move at least 10 places", 0.714, 0.266, size = 8.1)
  text(sprintf("Median move: %.1f places", median_move), 0.714, 0.211, size = 8.5)
  text(sprintf("%d of 10 states", top_ten_overlap), 0.714, 0.154,
       size = 11, heading = TRUE)
  text("appear in both top-ten lists", 0.714, 0.127, size = 8.1)
}

stem <- file.path(figures_dir, "figure_1_rates_and_rank_changes")
width <- 7.25
height <- 6.1
grDevices::quartz(type = "pdf", file = paste0(stem, ".pdf"),
                  width = width, height = height, family = font_regular, bg = "white",
                  title = "Racial imprisonment rates and changes in state rankings")
draw_figure()
invisible(dev.off())
ragg::agg_png(paste0(stem, ".png"), width = width, height = height,
              units = "in", res = 600, background = "white")
draw_figure()
invisible(dev.off())
ragg::agg_tiff(paste0(stem, ".tif"), width = width, height = height,
               units = "in", res = 600, compression = "lzw", background = "white")
draw_figure()
invisible(dev.off())
ragg::agg_png(paste0(stem, "_preview.png"), width = width, height = height,
              units = "in", res = 180, background = "white")
draw_figure()
invisible(dev.off())

stopifnot(identical(unname(tools::md5sum(input_file)), input_hash))
write.csv(data.frame(
  bin_lower = seq(0, 30, 5), bin_upper = seq(4, 34, 5), states = bin_counts
), file.path(results_dir, "r1_figure1_rank_change_bins.csv"), row.names = FALSE)
write.csv(data.frame(
  check = c("input_rows", "maximum_ratio_reconstruction_error",
            "maximum_gap_reconstruction_error", "states_moving_at_least_ten",
            "median_rank_movement", "top_ten_overlap", "spearman_rank_correlation"),
  value = c(nrow(states), max(abs(states$ratio - states$black_rate / states$white_rate)),
            max(abs(states$gap - states$black_rate + states$white_rate)), moving_ten,
            median_move, top_ten_overlap, cor(states$rank_ratio, states$rank_gap, method = "spearman"))
), file.path(results_dir, "r1_figure1_accessibility_checks.csv"), row.names = FALSE)
message("Figure 1 exported from verified existing summaries; no models were loaded or refitted.")
