# R1 estimand specification

Status: the specification used in the revised paper. Equation numbers refer to that version.

## Conceptual distinction

- Relative disparity: Black/White imprisonment-rate ratio.
- Absolute disparity: Black minus White imprisonment-rate gap per 100,000.
- Group-specific burden: Black and White imprisonment rates per 100,000.

The joint model does not combine these quantities into a composite score. It estimates the two
race-specific rates and derives each estimand from the same posterior draws.

## Primary comparison

The primary application is descriptive and between states. It compares states one standard
deviation apart in their long-run State Democracy Index score, using the between-state
coefficients `beta_B` and `beta_W` of the two log-rate equations.

For every posterior draw, calculate:

1. Percent change in the Black/White ratio: `100 * (exp(beta_B - beta_W) - 1)` (equation 6).
2. Change in the Black-White rate gap per 100,000: the two proportional changes applied to the
   observed mean Black and White rates for 2016-2020, the window used in Figure 1 (equation 7).
3. Percent change in the Black imprisonment rate: `100 * (exp(beta_B) - 1)` (equation 5).
4. Percent change in the White imprisonment rate: `100 * (exp(beta_W) - 1)` (equation 5).
5. The joint probability that relative disparity increases while absolute disparity does not
   (equation 8).

The first two quantities are disparity estimands. Items 3 and 4 are the burden estimands, and they
also explain how the two disparity measures move. `08_gap_estimand_audit.R` compares the gap
translation in item 2 with fixed-effect and observed-state standardization (Appendix B).

## Secondary comparison

Repeat the calculations with the within-state coefficients, for a one-SD increase above a state's
long-run democracy level. This answers a different descriptive question and is not treated as a
causal estimate or as a superior version of the between-state comparison.

## Model sequence

- Primary: joint Gaussian model of log rates with state intercepts, correlated outcomes, and AR(1)
  residuals.
- Sensitivity 1: joint Gaussian model without AR(1).
- Sensitivity 2: primary model with Census-region fixed effects.
- Sensitivity 3: joint negative-binomial count model with population offsets.
- Sensitivity 4: primary model without the 12 states flagged by the Hispanic-recording screen.

## Reporting rule

Report posterior medians, 95% credible intervals, and directional probabilities. Do not describe an
interval crossing zero as evidence of no effect. Do not call the Black rate or White rate a measure
of racial disparity.
