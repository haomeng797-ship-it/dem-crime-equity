# Democracy, Crime, and Equity in the U.S. States

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21611284.svg)](https://doi.org/10.5281/zenodo.21611284)

### A Joint Bayesian Framework for Estimating Racial Disparity in Imprisonment When the Measures Disagree

A reproducible study in R on public state-level data, asking whether stronger state democracy tracks less racial inequality in incarceration, and whether felon re-enfranchisement changes it. The point is mostly methodological: run the same question through four ordinary choices about measurement and design and the answer keeps shifting, so what looks like a finding about race and democracy often comes down to the analyst's choices. The full argument and results are in the paper, linked below.

**Author:** Miura Meng

*Revise and resubmit, Statistics and Public Policy.*

## Read it

- [Read the paper (web)](https://haomeng797-ship-it.github.io/dem-crime-equity/paper/dem_crime_equity_paper.html)
- [The paper as a formatted PDF](https://haomeng797-ship-it.github.io/dem-crime-equity/paper/dem_crime_equity_paper.pdf)

A longer, more exploratory data walk-through is also included as a self-contained report:

- [Read the report (web)](https://haomeng797-ship-it.github.io/dem-crime-equity/report/dem_crime_equity.html)

The matching `.qmd` files are the Quarto sources.

## Reproduce

The analysis needs only R (>= 4.5). Install the packages:

```r
install.packages(c("dplyr", "tidyr", "ggplot2", "lme4", "gbmt", "tidysynth",
                   "brms", "posterior", "ggridges", "ragg", "ggrepel", "knitr"))
```

Then run the scripts in order to rebuild the merged dataset, every model, and the figures:

```bash
Rscript R/01_load_merge.R            # build the merged state-year dataset
Rscript R/02_first_plot.R            # cross-section: democracy vs. the Black/White ratio
Rscript R/03_equity_measures.R       # four ways to measure inequity
Rscript R/04_within_state_models.R   # within/between mixed models
Rscript R/05_gbtm_trajectories.R     # trajectory typology
Rscript R/06_causal_amendment4.R     # Florida synthetic control
Rscript R/07_robustness.R            # synthetic-control robustness checks
Rscript R/08_bayes_expansion.R       # joint Bayesian model of the Black and white rates
Rscript R/09_bayes_robustness.R      # robustness checks for the joint model
Rscript R/10_bayes_ar1.R             # the headline model: joint model with AR(1) errors
Rscript R/11_spec_curve.R            # 16-specification curve
Rscript R/12_national_decomposition.R # decomposing the national ratio decline
Rscript R/13_covariate_signflips.R   # the sign flip beyond democracy: poverty, urbanicity, party
Rscript R/14_hispanic_robustness.R   # Hispanic-misclassification robustness
```

The Bayesian scripts (`08`–`10`, `14`) fit models with `brms`/Stan; each takes a few minutes and
caches its fit as an `.rds` at the repo root, refitting only if the cache is absent.

`R/00_theme.R` holds the shared figure style and is sourced by the plotting scripts; figures land in `figures/`.

Rebuilding the paper and report documents is optional and not needed to check the analysis. It also requires [Quarto](https://quarto.org) (and, for the PDF, `quarto install tinytex` plus the Times New Roman / Charter fonts used in the figures). The one-liner `bash run_all.sh` runs the whole pipeline end to end: the analysis, both figure sets, and the rendered documents.

## Repository layout

| Path | Contents |
|---|---|
| `R/` | analysis scripts `00`–`14` (run `01`–`14` in order) |
| `data/raw/` | third-party source data (see `data/SOURCES.md`) |
| `data/` | `state_dem_incarceration.{rds,csv}`, the merged dataset built by `R/01` |
| `figures/` | generated figures for the web (Charter, PNG 300 dpi) |
| `figures_pdf/` | the same figure set in Times, for the PDF paper |
| `paper/` | the paper: Quarto source + rendered PDF and HTML |
| `report/` | the longer data report: Quarto source + rendered HTML |

## Revision materials

The folder `submission/R1_work/` holds the code, saved results, and figures for the revised version of the paper, which is under review. The revision narrows the paper to the measurement question and the joint model of the two rates, and it leaves out the trajectory, synthetic-control, and specification-curve analyses. Apart from the data in `data/`, the folder does not depend on anything in `R/`. The scripts are in `submission/R1_work/analysis/`. Run them from the repository root in this order:

1. `00_rebuild_and_audit_data.R` rebuilds the analysis data from the raw files and checks it against `data/state_dem_incarceration.csv`.
2. `04_refit_primary_model.R` and `05_refit_sensitivity_models.R` refit the primary and sensitivity models. Each takes several minutes, and the fitted models are written to `submission/R1_work/models/`, which is not tracked.
3. `01_extract_posterior_estimands.R`, `03_audit_model_diagnostics.R`, `06_posterior_predictive_checks.R`, `07_descriptive_measurement_summary.R`, and `08_gap_estimand_audit.R` produce the estimates, diagnostics, and descriptive comparisons in `submission/R1_work/results/`.
4. `14_unify_figure_style.R` redraws every figure by running `02`, `12`, `13`, and `06` in turn, and `16_rate_covariance_check.R` reproduces the check of how the correlation between the two rate coefficients affects the ratio estimate.

The saved results contain the estimates reported in the paper, so they can be checked without refitting any model, and `16` runs from them alone. Script `14` needs steps 2 and 3 first, because it also redraws Figure S1 from the fitted primary model. Run `14` rather than `02` by itself: `02` also draws earlier layouts of Figures 2 and 3 under the final file names, and `14` replaces them with the versions from `12` and `13`. `estimand_spec.md` and `figure_spec.md` in the analysis folder describe the estimands and the figure conventions.

Where each part of the revised paper comes from (paths relative to `submission/R1_work/`):

| In the revised paper | Saved output | Script |
|---|---|---|
| Analysis sample (Section 3.2) | `results/r1_data_audit.csv` | `00` |
| Hispanic-recording screen (Section 3.2, Appendix C) | `results/r1_recording_screen.csv` | `05` |
| State examples and rank comparisons (Sections 2 and 5.1) | `results/r1_figure1_example_states.csv`, `results/r1_state_measurement_summary.csv`, `results/r1_rank_agreement_summary.csv`, `results/r1_annual_rank_agreement.csv` | `07` |
| Between- and within-state estimates (Sections 5.2 to 5.4, Tables C1 and C2) | `results/r1_posterior_summary.csv`, `results/r1_joint_probabilities.csv`, `results/r1_posterior_draws.rds` | `01` |
| Gap standardization (Appendix B) | `results/r1_gap_estimand_audit.csv` | `08` |
| Rate covariance check (Section 6) | `results/r1_rate_covariance_check.csv` | `16` |
| Model formulas and software versions (Appendix A) | `results/r1_model_formulas.csv`, `results/r1_refit_session_info.txt` | `03`, `04` |
| Convergence diagnostics (Table D1) | `results/r1_model_diagnostics.csv`, `results/r1_parameter_diagnostics.csv` | `03` |
| Posterior predictive and residual checks (Section 5.4, Table D2) | `results/r1_posterior_predictive_checks.csv`, `results/r1_residual_autocorrelation_checks.csv` | `06` |
| Figure 1 | `figures/figure_1_estimand_geometry.*` | `02` |
| Figure 2 | `figures/figure_2_joint_posterior.*` | `12` |
| Figure 3 | `figures/figure_3_robustness.*` | `13` |
| Figure S1 | `figures/figure_s2_posterior_predictive_checks.*` (the file keeps its working name) | `06` |

`figures/figure_1_estimand_geometry_and_ranks.*`, an extended version of Figure 1 with a rank panel, is not used in the paper.

## Data sources

All data are public. Each dataset belongs to its providers and is subject to their own terms; cite
them as the data source. See [`data/SOURCES.md`](data/SOURCES.md).

- State Democracy Index 2.0, by Grumbach & Bitton (UC Berkeley Democracy Policy Lab)
- Incarceration Trends, by the Vera Institute of Justice
- Voter Turnout 1980–2022, by M. McDonald (UF Election Lab)

## Methods

Linear mixed models with a within/between (Mundlak) decomposition (`lme4`); group-based multivariate
trajectory modeling (`gbmt`); synthetic control with placebo inference (`tidysynth`); a
specification curve over 16 measure-and-design combinations; and the paper's central model, a joint
Bayesian multilevel model of the Black and white imprisonment rates with correlated state effects
and AR(1) errors (`brms`).

## License

Code is released under the MIT License (see [`LICENSE`](LICENSE)). The data in `data/` are not
covered by that license and remain subject to the terms of their original providers.
