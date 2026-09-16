# Quantify how relative and absolute disparity rank the same states differently.

suppressPackageStartupMessages({
  library(dplyr)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE))
} else {
  getwd()
}
project_root <- normalizePath(file.path(script_dir, "..", "..", ".."), mustWork = TRUE)
results_dir <- file.path(project_root, "submission", "R1_work", "results")

data <- read.csv(file.path(results_dir, "r1_rebuilt_analysis_data.csv"))

state_summary <- data |>
  filter(year >= 2016, year <= 2020) |>
  group_by(state_name, state_abbr) |>
  summarise(
    black_rate = mean(black_prison_pop_rate, na.rm = TRUE),
    white_rate = mean(white_prison_pop_rate, na.rm = TRUE),
    democracy = mean(democracy, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    ratio = black_rate / white_rate,
    gap = black_rate - white_rate,
    rank_ratio = min_rank(desc(ratio)),
    rank_gap = min_rank(desc(gap)),
    rank_difference = rank_gap - rank_ratio,
    absolute_rank_difference = abs(rank_difference)
  )

top_overlap <- function(k) {
  sum(state_summary$rank_ratio <= k & state_summary$rank_gap <= k)
}

summary_statistics <- tibble(
  statistic = c(
    "Pearson correlation: ratio and gap",
    "Spearman rank correlation: ratio and gap",
    "Median absolute rank difference",
    "Mean absolute rank difference",
    "Maximum absolute rank difference",
    "States differing by at least 10 rank places",
    "States differing by at least 20 rank places",
    "Top-five overlap",
    "Top-ten overlap"
  ),
  value = c(
    cor(state_summary$ratio, state_summary$gap),
    cor(state_summary$ratio, state_summary$gap, method = "spearman"),
    median(state_summary$absolute_rank_difference),
    mean(state_summary$absolute_rank_difference),
    max(state_summary$absolute_rank_difference),
    sum(state_summary$absolute_rank_difference >= 10),
    sum(state_summary$absolute_rank_difference >= 20),
    top_overlap(5),
    top_overlap(10)
  )
)

annual_summary <- data |>
  filter(
    year <= 2022,
    is.finite(black_prison_pop_rate),
    is.finite(white_prison_pop_rate),
    black_prison_pop_rate > 0,
    white_prison_pop_rate > 0
  ) |>
  group_by(year) |>
  mutate(
    ratio = black_prison_pop_rate / white_prison_pop_rate,
    gap = black_prison_pop_rate - white_prison_pop_rate,
    rank_ratio = min_rank(desc(ratio)),
    rank_gap = min_rank(desc(gap))
  ) |>
  summarise(
    states = n(),
    rank_correlation = cor(ratio, gap, method = "spearman"),
    median_absolute_rank_difference = median(abs(rank_ratio - rank_gap)),
    states_differing_by_10_or_more = sum(abs(rank_ratio - rank_gap) >= 10),
    top_ten_overlap = sum(rank_ratio <= 10 & rank_gap <= 10),
    .groups = "drop"
  )

example_states <- state_summary |>
  filter(state_abbr %in% c("MD", "SD", "MA", "NH")) |>
  arrange(match(state_abbr, c("MD", "SD", "MA", "NH")))

write.csv(state_summary, file.path(results_dir, "r1_state_measurement_summary.csv"), row.names = FALSE)
write.csv(summary_statistics, file.path(results_dir, "r1_rank_agreement_summary.csv"), row.names = FALSE)
write.csv(annual_summary, file.path(results_dir, "r1_annual_rank_agreement.csv"), row.names = FALSE)
write.csv(example_states, file.path(results_dir, "r1_figure1_example_states.csv"), row.names = FALSE)

cat("2016-2020 rank agreement\n")
print(summary_statistics, n = Inf)
cat("\nIllustrative state pairs\n")
print(
  example_states |>
    select(state_abbr, black_rate, white_rate, ratio, gap, rank_ratio, rank_gap),
  n = Inf
)
cat("\nAnnual range, 2000-2022\n")
print(
  summarise(
    annual_summary,
    minimum_rank_correlation = min(rank_correlation),
    maximum_rank_correlation = max(rank_correlation),
    minimum_top_ten_overlap = min(top_ten_overlap),
    maximum_top_ten_overlap = max(top_ten_overlap),
    minimum_states_differing_by_10 = min(states_differing_by_10_or_more),
    maximum_states_differing_by_10 = max(states_differing_by_10_or_more)
  ),
  width = Inf
)
