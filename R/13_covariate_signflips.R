# 10_covariate_signflips.R
# Does the sign flip generalize beyond the democracy index? Correlate three common
# structural determinants of incarceration and political economy with the four
# disparity measures (state means over 2016-2020, as in Figure 2):
#   - poverty rate (Census SAIPE, averaged 2016-2020; data/raw/saipe_est??us.csv)
#   - percent urban (2020 Census; data/raw/census_urban_rural_2020.csv)
#   - governor's party (January 2019 snapshot, plus the share of window years with a
#     Republican governor as a robustness coding; Independent years count as non-Republican)
# Descriptive only: the governor's party stands in for a bundle of policy and regional
# differences; no partisan effect is claimed.

suppressPackageStartupMessages(library(dplyr))

proj <- Sys.getenv("DEM_CRIME_ROOT", unset = getwd())
df <- readRDS(file.path(proj, "data", "state_dem_incarceration.rds"))

st <- df |> filter(year %in% 2016:2020) |>
  group_by(state_name) |>
  summarise(ratio = mean(bw_ratio, na.rm = TRUE),
            black = mean(black_prison_pop_rate, na.rm = TRUE),
            white = mean(white_prison_pop_rate, na.rm = TRUE),
            gap   = mean(black_prison_pop_rate - white_prison_pop_rate, na.rm = TRUE),
            democracy = mean(democracy, na.rm = TRUE), .groups = "drop")

cov <- read.csv(file.path(proj, "data", "raw", "state_covariates.csv"))

# Republican share of window years 2016-2020. Most states kept one party all window;
# the exceptions below are coded by majority party within each calendar year.
rep_years <- c(
  "New Hampshire" = 4, "Vermont" = 4, "Missouri" = 4, "Kentucky" = 4,   # R except one year
  "West Virginia" = 3,                                                   # D 2016-17, R 2018-20
  "Illinois" = 3, "Kansas" = 3, "Maine" = 3, "Michigan" = 3,             # R 2016-18, D 2019-20
  "Nevada" = 3, "New Mexico" = 3, "Wisconsin" = 3,
  "New Jersey" = 2,                                                      # R 2016-17, D 2018-20
  "Alaska" = 2,                                                          # Independent 2016-18, R 2019-20
  "North Carolina" = 1                                                   # R 2016, D 2017-20
)
cov <- cov |>
  mutate(rep_gov = as.integer(gov_party_jan2019 == "R"),
         rep_share = ifelse(state_name %in% names(rep_years),
                            rep_years[state_name] / 5, rep_gov))

m <- inner_join(st, cov, by = "state_name") |> filter(is.finite(ratio))
cat("states:", nrow(m), "\n\n")

show <- function(x, label) {
  r <- sapply(m[c("ratio", "black", "white", "gap")],
              function(y) cor(x, y, use = "complete.obs"))
  cat(sprintf("%-30s ratio %+0.2f | Black %+0.2f | white %+0.2f | gap %+0.2f\n",
              label, r[1], r[2], r[3], r[4]))
}
show(m$democracy,             "State Democracy Index (paper)")
show(m$poverty_pct_2016_2020, "Poverty rate (SAIPE 2016-20)")
show(m$pct_urban_2020,        "Percent urban (2020 Census)")
show(m$rep_gov,               "Republican governor (Jan 2019)")
show(m$rep_share,             "Republican share 2016-20 (robust)")
