# 12_national_decomposition.R
# The national Black/White prison-rate ratio narrowed sharply after 2000 -- the field's
# most-quoted equity trend. Decompose the narrowing on the log scale:
#   dlog(ratio) = dlog(Black rate) - dlog(white rate),
# so the share of narrowing owed to the white rate RISING is dlog(W) / -dlog(ratio).
# Rates are aggregated from Vera counts and populations (prison, ages 15-64), using only
# states with data in BOTH endpoint years so composition changes cannot masquerade as trend.

suppressPackageStartupMessages(library(dplyr))

proj <- Sys.getenv("DEM_CRIME_ROOT", unset = getwd())
v <- read.csv(file.path(proj, "data", "raw", "incarceration_trends_state.csv")) |>
  select(year, state_name, black_prison_pop, white_prison_pop,
         black_pop_15to64, white_pop_15to64) |>
  filter(if_all(black_prison_pop:white_pop_15to64, is.finite))

decompose <- function(y0, y1) {
  s0 <- v |> filter(year == y0)
  s1 <- v |> filter(year == y1)
  common <- intersect(s0$state_name, s1$state_name)
  agg <- function(s) s |>
    filter(state_name %in% common) |>
    summarise(B = 1e5 * sum(black_prison_pop) / sum(black_pop_15to64),
              W = 1e5 * sum(white_prison_pop) / sum(white_pop_15to64))
  a <- agg(s0); b <- agg(s1)
  dR <- log(b$B / b$W) - log(a$B / a$W)
  dB <- log(b$B) - log(a$B); dW <- log(b$W) - log(a$W)
  cat(sprintf("%d -> %d  (%d states in both years)\n", y0, y1, length(common)))
  cat(sprintf("  ratio %.2f -> %.2f | Black %.0f -> %.0f (%+.0f%%) | white %.0f -> %.0f (%+.0f%%)\n",
              a$B / a$W, b$B / b$W, a$B, b$B, 100 * (b$B / a$B - 1),
              a$W, b$W, 100 * (b$W / a$W - 1)))
  cat(sprintf("  dlog(ratio) = %.3f = dlog(B) %.3f - dlog(W) %.3f\n", dR, dB, dW))
  cat(sprintf("  share of narrowing from Black decline: %.0f%% | from white rise: %.0f%%\n\n",
              100 * (-dB) / (-dR), 100 * dW / (-dR)))
}

decompose(2000, 2019)   # pre-COVID window
decompose(2000, 2022)   # full window (pandemic-era declines pull the white rate back down)
