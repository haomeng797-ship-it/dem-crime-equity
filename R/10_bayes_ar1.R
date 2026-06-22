# 10_bayes_ar1.R -- address the over-confident P=0.99: the R/08 model ignored within-state serial
# dependence (the autocorrelation Demonstration 3 itself flags). Refit the joint model with an AR(1)
# residual structure per state, which widens the intervals honestly. Cross-outcome correlation is
# carried by correlated state intercepts (rescor is incompatible with ar()).
suppressPackageStartupMessages({ library(brms); library(dplyr); library(posterior) })
proj <- Sys.getenv("DEM_CRIME_ROOT", unset = getwd())
d <- read.csv(file.path(proj, "data", "state_dem_incarceration.csv")) |>
  filter(!is.na(black_prison_pop_rate), !is.na(white_prison_pop_rate),
         black_prison_pop_rate > 0, white_prison_pop_rate > 0, !is.na(democracy)) |>
  mutate(logB = log(black_prison_pop_rate), logW = log(white_prison_pop_rate)) |>
  group_by(state_abbr) |> mutate(demM_raw = mean(democracy)) |> ungroup() |>
  mutate(demW_raw = democracy - demM_raw, yearc = (year - 2011)/10)
sm <- d |> distinct(state_abbr, demM_raw)
d$demM <- (d$demM_raw - mean(sm$demM_raw))/sd(sm$demM_raw); d$demW <- d$demW_raw/sd(d$demW_raw)
d <- d |> arrange(state_abbr, year)

bfB <- bf(logB ~ demW + demM + yearc + (1|p|state_abbr) + ar(time = year, gr = state_abbr))
bfW <- bf(logW ~ demW + demM + yearc + (1|p|state_abbr) + ar(time = year, gr = state_abbr))
pr  <- c(prior(normal(0,1), class=b, resp="logB"), prior(normal(0,1), class=b, resp="logW"))
fp <- file.path(proj, "bayes_fit_ar1.rds")
if (file.exists(fp)) { fit <- readRDS(fp) } else {
  fit <- brm(bfB + bfW, data = d, prior = pr, chains = 4, iter = 4000, warmup = 1000,
             seed = 20260620, cores = 4, refresh = 0, control = list(adapt_delta = 0.95, max_treedepth = 12))
  saveRDS(fit, fp)
}
cat("max Rhat:", round(max(rhat(fit), na.rm=TRUE),4),
    " | divergences:", sum(subset(nuts_params(fit), Parameter=='divergent__')$Value), "\n")
dr <- as_draws_df(fit)
Bb<-dr$b_logB_demM; Wb<-dr$b_logW_demM; Bw<-dr$b_logB_demW; Ww<-dr$b_logW_demW; rb<-Bb-Wb; rw<-Bw-Ww
q<-function(x)sprintf("%+.3f [%+.3f, %+.3f]",median(x),quantile(x,.025),quantile(x,.975))
cat("\n=== AR(1) model, BETWEEN-state per +1 SD democracy ===\n")
cat("  ratio:",q(rb)," Black:",q(Bb)," White:",q(Wb),"\n")
cat("  P(ratio>0 & White<0) =",round(mean(rb>0 & Wb<0),3),"  (was 0.99 without AR1)\n")
cat("  P(ratio>0 & Black<0) =",round(mean(rb>0 & Bb<0),3),"\n")
cat("=== WITHIN-state ===\n")
cat("  ratio:",q(rw)," Black:",q(Bw)," White:",q(Ww),"\n")
# autocor estimate
arp <- summary(fit)$cor_pars; if(!is.null(arp)) { cat("\nAR(1) estimates:\n"); print(round(arp,3)) }
cat("\n===== AR1 DONE =====\n")

# --- figure (regenerated from the AR(1) posterior, same styling as R/08) ---
suppressPackageStartupMessages({ library(ggplot2); library(tidyr); library(ggridges) })
source(file.path(proj, "R", "00_theme.R"))
.pd <- data.frame(`Black/White ratio`=rb, `absolute Black`=Bb, `absolute White`=Wb, check.names=FALSE) |>
  pivot_longer(everything(), names_to="measure", values_to="effect")
.pd$measure <- factor(.pd$measure, levels=c("absolute White","absolute Black","Black/White ratio"))
.mk <- function() ggplot(.pd, aes(effect, measure, fill=measure)) +
  geom_density_ridges(scale=1.5, alpha=.9, color="white", linewidth=.3, rel_min_height=.004) +
  geom_vline(xintercept=0, linetype="dashed", color="grey35", linewidth=.4) +
  scale_fill_manual(values=c("absolute White"="#6F8AA1","absolute Black"="#8C8C8E","Black/White ratio"="#AC827A"), guide="none") +
  scale_x_continuous(limits=c(-.4,.4)) +
  labs(title="The measures are contrasts of one posterior",
       subtitle=paste("Effect of +1 SD between-state democracy (serial dependence modeled). The ratio is positive only",
                      "because white imprisonment (blue) falls faster than Black: the denominator does the work.", sep="\n"),
       x="effect on log rate per +1 SD democracy (between-state)", y=NULL) + theme_poli(base_size=13)
.sv <- function(f) ggsave(f, .mk(), width=8, height=4.3, dpi=300, device=ragg::agg_png, bg="white")
Sys.unsetenv("FIG_FONT"); source(file.path(proj,"R","00_theme.R")); .sv(file.path(proj,"figures","06_bayes_contrasts.png"))
Sys.setenv(FIG_FONT="Times New Roman"); source(file.path(proj,"R","00_theme.R")); .sv(file.path(proj,"figures_pdf","06_bayes_contrasts.png"))
cat("figure regenerated from AR(1) posterior\n")
