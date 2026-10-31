# R1 figure specification

Status: the conventions used for the figures in the revised paper, based on the journal's author
instructions and a visual audit of recent 2026 Statistics and Public Policy articles. The shared
settings are in `figure_style.R`.

## Journal constraints

- Figures must remain legible at journal size.
- Lettering must be uniform across figures.
- Lines and shadings must survive reproduction.
- Vector PDF is the working master; 600-dpi PNG and TIFF are submission derivatives.
- Figure captions carry the explanation. The artwork should not contain paragraph-length titles,
  subtitles, sources, or methodological disclaimers.

## Visual system

- Typeface: Helvetica throughout the artwork.
- Type size at final width: 8.6-pt base with 8-pt axis and legend text (`theme_r1()`); in-panel
  labels are set slightly smaller.
- Main-text width: 7.25 inches for two-column figures, 3.5 inches for single-column figures.
- Background: white.
- Grid: only faint major grid lines when they materially aid comparison.
- Borders: no enclosing box; retain only necessary axes.
- Uncertainty: intervals and light bands, with opacity that also works in grayscale.
- Accessibility: every color distinction must also be carried by position, shape, or line type.

## Semantic colors

- Relative disparity (ratio): `#9B5361`.
- Absolute disparity (gap): `#2E4A6B`.
- Black group-specific rate: near-black `#171717`.
- White group-specific rate: blue `#0072B2`.
- Observations and reference marks: neutral grays.

## Figures in the revised paper

1. Figure 1 (`figure_1_estimand_geometry`): state averages for 2016-2020. Panel A shows that equal
   ratios lie on rays from the origin, and Panel B shows that equal gaps lie on parallel lines,
   with a Gap 0 reference line.
2. Figure 2 (`figure_2_joint_posterior`): Panel A plots the ratio change against the gap change
   for the between-state contrast, with posterior density contours; Panel B shows the two
   group-specific rate changes that generate the result.
3. Figure 3 (`figure_3_robustness`): paired interval plots for the ratio and the gap across the
   primary and four sensitivity models.
4. Figure S1 (`figure_s2_posterior_predictive_checks`, a working file name): posterior predictive
   densities for the two log rates, and lag-1 residual dependence before and after the AR(1) term.

## Quality gate

Each figure must be checked at full width and at final print size, in color and grayscale. Axis text,
panel labels, symbols, and intervals must remain readable after insertion into the 12-point,
double-spaced manuscript. No figure is final until its vector and raster versions agree visually.
