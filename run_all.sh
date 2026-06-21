#!/usr/bin/env bash
# run_all.sh -- regenerate everything from raw data: both figure sets, all analyses,
# and the rendered paper + report. Run from anywhere (it cd's to the repo root).
# Needs R + the packages in the README, Quarto, and (for the PDF) a LaTeX engine
# plus the Times New Roman / Charter fonts.
set -euo pipefail
cd "$(dirname "$0")"

QUARTO=$(command -v quarto || echo "$HOME/quarto/bin/quarto")

echo "== dataset + web figures (Charter) + analyses =="
Rscript R/01_load_merge.R
Rscript R/02_first_plot.R
Rscript R/03_equity_measures.R
Rscript R/04_within_state_models.R
Rscript R/05_gbtm_trajectories.R
Rscript R/06_causal_amendment4.R
Rscript R/07_robustness.R
Rscript R/08_bayes_expansion.R

echo "== PDF figures (Times) into figures_pdf/ =="
for s in 02_first_plot 03_equity_measures 04_within_state_models 05_gbtm_trajectories 06_causal_amendment4 08_bayes_expansion; do
  FIG_FONT="Times New Roman" FIG_OUTDIR="figures_pdf" Rscript "R/${s}.R"
done

echo "== render documents =="
"$QUARTO" render report/dem_crime_equity.qmd
"$QUARTO" render paper/dem_crime_equity_paper.qmd

echo "== done: figures/, figures_pdf/, paper, and report all regenerated =="
