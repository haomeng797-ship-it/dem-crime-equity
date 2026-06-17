# Democracy, Crime, and Equity in the U.S. States

Does state democratic backsliding track racial inequality in incarceration, and can felon
re-enfranchisement undo it? A small, fully reproducible study in R using public data. Its recurring
argument is methodological: how the question is measured and designed, more than the raw data,
decides the finding.

**Author:** Miura Meng

## Summary

The relationship between state democracy and racial inequality in incarceration turns out to depend
almost entirely on how the question is measured and designed. A naive cross-section suggests that
more democratic states have wider racial disparities, but that is an artifact of the popular
Black/White incarceration ratio. Following each state over time, stronger democracy coincides with
lower incarceration of both groups, though a national decline is the larger force. And Florida's
2018 Amendment 4, the largest restoration of felon voting rights in modern U.S. history, produced
no measurable change in turnout, because a follow-up law (SB7066) first required people to pay
outstanding court fines. The recurring lesson: careful choices about measurement and design keep
overturning the convenient story.

## Key findings

- **Measurement decides the answer.** The Black/White incarceration ratio is higher in more
  democratic states only because they imprison far fewer white people; by the absolute Black rate
  or the Black–white gap, the relationship flips or flattens.
- **Within states over time** (mixed models), stronger democracy goes with lower incarceration of
  both groups, but the effect is modest next to a nationwide decarceration trend.
- **A trajectory typology** (group-based trajectory modeling) shows the states that democratized
  decarcerated the most, while those that backslid did so least.
- **A causal test** (synthetic control of Florida's Amendment 4) finds no turnout effect, because
  the reform was largely neutralized by a pay-your-fines-first requirement.

## Read it

The main write-up is a short academic paper, **"The Measure Makes the Finding."** It argues that the
link between democracy, race, and punishment is decided less by the data than by how inequality is
measured and how the study is designed.

- Paper, formatted PDF: [`paper/dem_crime_equity_paper.pdf`](paper/dem_crime_equity_paper.pdf)
- Paper, web version: [`paper/dem_crime_equity_paper.html`](paper/dem_crime_equity_paper.html)

A longer, more exploratory data walk-through is also included as a self-contained report:

- [`report/dem_crime_equity.html`](report/dem_crime_equity.html)

The matching `.qmd` files are the Quarto sources.

## Reproduce

Requires R (>= 4.5) and [Quarto](https://quarto.org). Install the R packages:

```r
install.packages(c("dplyr", "tidyr", "ggplot2", "lme4", "gbmt", "tidysynth", "ragg", "knitr"))
```

Then run the pipeline from the repository root, in order:

```bash
Rscript R/01_load_merge.R          # build the merged state-year dataset
Rscript R/02_first_plot.R          # cross-section: democracy vs. the Black/White ratio
Rscript R/03_equity_measures.R     # four ways to measure inequity
Rscript R/04_within_state_models.R # within/between mixed models
Rscript R/05_gbtm_trajectories.R   # trajectory typology
Rscript R/06_causal_amendment4.R   # Florida synthetic control

quarto render report/dem_crime_equity.qmd       # the report (HTML)
quarto render paper/dem_crime_equity_paper.qmd  # the paper (PDF + HTML)
```

`R/00_theme.R` holds the shared figure style (font and palette) and is sourced by the plotting
scripts. Figures are generated twice: `figures/` uses the Charter font for the web, and
`figures_pdf/` uses Times to match the PDF paper. The scripts write `figures/` by default; prefix a
run with `FIG_FONT="Times New Roman" FIG_OUTDIR="figures_pdf"` to produce the PDF set.

## Repository layout

| Path | Contents |
|---|---|
| `R/` | analysis scripts `00`–`06` (run `01`–`06` in order) |
| `data/raw/` | third-party source data (see `data/SOURCES.md`) |
| `data/` | `state_dem_incarceration.{rds,csv}`, the merged dataset built by `R/01` |
| `figures/` | generated figures for the web (Charter, PNG 300 dpi) |
| `figures_pdf/` | the same figure set in Times, for the PDF paper |
| `paper/` | the workshop paper: Quarto source + rendered PDF and HTML (English) |
| `report/` | the longer data report: Quarto source + rendered HTML (English) |

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
