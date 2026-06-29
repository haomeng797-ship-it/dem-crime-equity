# Democracy, Crime, and Equity in the U.S. States

### The Measure Makes the Finding
*Racial disparity, democracy, and punishment in the U.S. states.*

A small, reproducible study in R on public state-level data, asking whether stronger state democracy tracks less racial inequality in incarceration, and whether felon re-enfranchisement changes it. The point is mostly methodological: run the same question through four ordinary choices about measurement and design and the answer keeps shifting, so what looks like a finding about race and democracy often comes down to the analyst's choices. The full argument and results are in the paper, linked below.

**Author:** Miura Meng

## Read it

The main write-up is a short paper, **"The Measure Makes the Finding."**

- [Read the paper (web)](https://haomeng797-ship-it.github.io/dem-crime-equity/paper/dem_crime_equity_paper.html)
- [The paper as a formatted PDF](https://haomeng797-ship-it.github.io/dem-crime-equity/paper/dem_crime_equity_paper.pdf)

A longer, more exploratory data walk-through is also included as a self-contained report:

- [Read the report (web)](https://haomeng797-ship-it.github.io/dem-crime-equity/report/dem_crime_equity.html)

The matching `.qmd` files are the Quarto sources.

## Reproduce

The analysis needs only R (>= 4.5). Install the packages:

```r
install.packages(c("dplyr", "tidyr", "ggplot2", "lme4", "gbmt", "tidysynth", "ragg", "ggrepel", "knitr"))
```

Then run the scripts in order to rebuild the merged dataset, every model, and the figures:

```bash
Rscript R/01_load_merge.R          # build the merged state-year dataset
Rscript R/02_first_plot.R          # cross-section: democracy vs. the Black/White ratio
Rscript R/03_equity_measures.R     # four ways to measure inequity
Rscript R/04_within_state_models.R # within/between mixed models
Rscript R/05_gbtm_trajectories.R   # trajectory typology
Rscript R/06_causal_amendment4.R   # Florida synthetic control
Rscript R/07_robustness.R          # synthetic-control robustness checks
```

`R/00_theme.R` holds the shared figure style and is sourced by the plotting scripts; figures land in `figures/`.

Rebuilding the paper and report documents is optional and not needed to check the analysis. It also requires [Quarto](https://quarto.org) (and, for the PDF, `quarto install tinytex` plus the Times New Roman / Charter fonts used in the figures). The one-liner `bash run_all.sh` runs the whole pipeline end to end: the analysis, both figure sets, and the rendered documents.

## Repository layout

| Path | Contents |
|---|---|
| `R/` | analysis scripts `00`–`07` (run `01`–`07` in order) |
| `data/raw/` | third-party source data (see `data/SOURCES.md`) |
| `data/` | `state_dem_incarceration.{rds,csv}`, the merged dataset built by `R/01` |
| `figures/` | generated figures for the web (Charter, PNG 300 dpi) |
| `figures_pdf/` | the same figure set in Times, for the PDF paper |
| `paper/` | the paper: Quarto source + rendered PDF and HTML |
| `report/` | the longer data report: Quarto source + rendered HTML |

## Data sources

All data are public. Each dataset belongs to its providers and is subject to their own terms; cite
them as the data source. See [`data/SOURCES.md`](data/SOURCES.md).

- State Democracy Index 2.0, by Grumbach & Bitton (UC Berkeley Democracy Policy Lab)
- Incarceration Trends, by the Vera Institute of Justice
- Voter Turnout 1980–2022, by M. McDonald (UF Election Lab)

## Methods

Linear mixed models with a within/between (Mundlak) decomposition (`lme4`); group-based multivariate
trajectory modeling (`gbmt`); synthetic control with placebo inference (`tidysynth`).

## License

Code is released under the MIT License (see [`LICENSE`](LICENSE)). The data in `data/` are not
covered by that license and remain subject to the terms of their original providers.
