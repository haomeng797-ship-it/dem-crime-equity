# 11_spec_curve.R -- a full-crossing specification curve for the democracy -> inequality effect.
# Crosses 4 inequality MEASURES x 2 comparison DESIGNS (between- vs within-state) x year-trend in/out
# = 16 defensible specifications. Each outcome is z-scored so the democracy coefficients are
# comparable (SD of outcome per SD of democracy). Shows the sign is decided by the choices.
suppressPackageStartupMessages({ library(dplyr); library(lme4); library(ggplot2) })
proj <- Sys.getenv("DEM_CRIME_ROOT", unset = getwd())
source(file.path(proj, "R", "00_theme.R"))
d <- readRDS(file.path(proj, "data", "state_dem_incarceration.rds")) |>
  filter(is.finite(bw_ratio), black_prison_pop_rate > 0, white_prison_pop_rate > 0, is.finite(democracy)) |>
  group_by(state_abbr) |> mutate(n = n(), dem_between = mean(democracy), dem_within = democracy - dem_between) |>
  ungroup() |> filter(n >= 10) |> mutate(year_c = year - 2011)
z <- function(v) as.numeric(scale(v))
d$dem_between_z <- z(d$dem_between); d$dem_within_z <- z(d$dem_within)
d$y_ratio <- z(log(d$bw_ratio)); d$y_black <- z(log(d$black_prison_pop_rate))
d$y_white <- z(log(d$white_prison_pop_rate)); d$y_gap <- z(d$black_prison_pop_rate - d$white_prison_pop_rate)
outs <- c("Black/White ratio"="y_ratio","absolute Black rate"="y_black",
          "absolute White rate"="y_white","Black-White gap"="y_gap")
specs <- expand.grid(measure=names(outs), design=c("between-state","within-state"),
                     yr=c(TRUE,FALSE), stringsAsFactors=FALSE)
res <- lapply(seq_len(nrow(specs)), function(i){
  s <- specs[i,]; term <- if(s$design=="between-state") "dem_between_z" else "dem_within_z"
  rhs <- paste0("dem_between_z + dem_within_z", if(s$yr) " + year_c" else "", " + (1|state_abbr)")
  m <- lmer(as.formula(paste0(outs[[s$measure]]," ~ ",rhs)), data=d)
  co <- summary(m)$coefficients
  data.frame(measure=s$measure, design=s$design, yr=s$yr,
             est=co[term,"Estimate"], se=co[term,"Std. Error"])
}) |> bind_rows() |>
  mutate(lo=est-1.96*se, hi=est+1.96*se) |> arrange(est) |> mutate(rank=row_number())

cat(sprintf("specs: %d | est range %.2f to %.2f | %d positive, %d negative\n",
    nrow(res), min(res$est), max(res$est), sum(res$est>0), sum(res$est<0)))

p <- ggplot(res, aes(rank, est, color=measure, shape=design)) +
  geom_hline(yintercept=0, linetype="dashed", color="grey35", linewidth=.4) +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=0, linewidth=.5, alpha=.7) +
  geom_point(size=2.6) +
  scale_color_manual(values=c("Black/White ratio"="#AC827A","absolute Black rate"="#8C8C8E",
                              "absolute White rate"="#6F8AA1","Black-White gap"="#C29A47")) +
  scale_shape_manual(values=c("between-state"=16,"within-state"=17)) +
  labs(title="The democracy-inequality effect across 16 defensible specifications",
       subtitle=paste("Each point is one analytic choice (measure x comparison design x year-trend in/out).",
                      "The sign is set by the choices, not the data: it runs from clearly negative to clearly positive.", sep="\n"),
       x="specification (ranked by effect)", y="standardized democracy effect (SD of outcome per SD)",
       color=NULL, shape=NULL) +
  theme_poli(base_size=12) + theme(legend.position="top", legend.box="vertical")
sv <- function(f) ggsave(f, p, width=8.5, height=5, dpi=300, device=ragg::agg_png, bg="white")
Sys.unsetenv("FIG_FONT"); source(file.path(proj,"R","00_theme.R")); sv(file.path(proj,"figures","07_spec_curve.png"))
Sys.setenv(FIG_FONT="Times New Roman"); source(file.path(proj,"R","00_theme.R")); sv(file.path(proj,"figures_pdf","07_spec_curve.png"))
cat("saved figures/07_spec_curve.png + figures_pdf/07_spec_curve.png\n")
