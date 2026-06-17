# 05_gbtm_trajectories.R
# Step 4: group-based MULTIVARIATE trajectory modeling (gbmt) -- the method born in
# criminology (Nagin & Land 1993, to type life-course offending). Here we type the 50 states
# by their JOINT 2000-2022 trajectory of (i) State Democracy Index and (ii) log Black
# incarceration rate. Class number chosen by BIC. Classes then characterized by region and by
# the White rate / Black-White ratio (the equity read).

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(gbmt); library(ggplot2) })
source("R/00_theme.R")
set.seed(1)

proj <- path.expand("~/Documents/dem-crime-equity")
df <- readRDS(file.path(proj, "data", "state_dem_incarceration.rds"))

# regular 50-state x 2000-2022 grid; gbmt imputes the few missing cells
base <- df |>
  filter(year >= 2000, year <= 2022, black_prison_pop_rate > 0) |>
  mutate(log_black = log(black_prison_pop_rate)) |>
  select(state_abbr, region, year, democracy, log_black,
         black_prison_pop_rate, white_prison_pop_rate, bw_ratio)
reg <- distinct(base, state_abbr, region)
grid <- base |>
  select(-region) |>
  tidyr::complete(state_abbr, year = 2000:2022) |>
  left_join(reg, by = "state_abbr") |>
  arrange(state_abbr, year) |> as.data.frame()
cat("states:", n_distinct(grid$state_abbr), " rows:", nrow(grid), "\n\n")

# --- fit gbmt for ng = 2..5 (quadratic, standardized series); pick by BIC ---
fit_K <- function(K) {
  m <- NULL
  invisible(capture.output(
    m <- try(gbmt(x.names = c("democracy", "log_black"), unit = "state_abbr",
                  time = "year", d = 2, ng = K, scaling = 2, data = grid), silent = TRUE)))
  m
}
sel <- data.frame()
fits <- list()
for (K in 2:5) {
  m <- fit_K(K)
  if (!inherits(m, "try-error") && !is.null(m)) {
    fits[[as.character(K)]] <- m
    sel <- rbind(sel, data.frame(ng = K, bic = unname(m$ic["bic"]),
                                 min_appa = min(m$appa),
                                 min_class = min(table(m$assign))))
  }
}
cat("model selection:\n"); print(sel, row.names = FALSE)
best <- sel$ng[which.min(sel$bic)]
cat("\nchosen by BIC: ng =", best, "\n\n")
m <- fits[[as.character(best)]]

# --- attach class membership ---
cls <- tibble(state_abbr = names(m$assign), class = factor(m$assign))
grid <- left_join(grid, cls, by = "state_abbr")

# --- characterize each class ---
prof <- grid |>
  filter(year %in% c(2000, 2022)) |>
  group_by(class, year) |>
  summarise(democracy = mean(democracy, na.rm = TRUE),
            black_rate = mean(black_prison_pop_rate, na.rm = TRUE),
            white_rate = mean(white_prison_pop_rate, na.rm = TRUE),
            bw_ratio = mean(bw_ratio, na.rm = TRUE), .groups = "drop") |>
  arrange(class, year)
cat("class profiles (means at 2000 vs 2022):\n"); print(as.data.frame(prof), digits = 3)

cat("\nclass membership:\n")
for (g in levels(cls$class)) cat("  Class", g, ":", paste(cls$state_abbr[cls$class == g], collapse = " "), "\n")

cls2 <- left_join(cls, reg, by = "state_abbr")
cat("\nregion mix by class:\n"); print(table(class = cls2$class, region = cls2$region))

# --- plot class-mean trajectories (observed) ---
traj <- grid |>
  group_by(class, year) |>
  summarise(`State Democracy Index` = mean(democracy, na.rm = TRUE),
            `Black incarceration rate (per 100k)` = mean(black_prison_pop_rate, na.rm = TRUE),
            `Black / White ratio` = mean(bw_ratio, na.rm = TRUE), .groups = "drop") |>
  pivot_longer(-c(class, year), names_to = "series", values_to = "value") |>
  mutate(series = factor(series, levels = c("State Democracy Index",
            "Black incarceration rate (per 100k)", "Black / White ratio")))

p <- ggplot(traj, aes(year, value, color = class)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = class_pal) +
  facet_wrap(~series, scales = "free_y") +
  labs(title = paste0("State trajectory classes (gbmt), 2000-2022:  ng = ", best),
       subtitle = "Class-mean paths. States typed jointly on democracy + log Black incarceration; ratio shown for the equity read.",
       x = NULL, y = NULL, color = "Class",
       caption = "Group-based multivariate trajectory model (gbmt; Nagin/Land method). Sources: SDI 2.0; Vera Incarceration Trends.") +
  theme_poli(base_size = 12)

save_fig(p, file.path(proj, "figures", "04_gbtm_classes.png"), width = 11, height = 4.6)
cat("\nsaved figures/04_gbtm_classes.png\n")
